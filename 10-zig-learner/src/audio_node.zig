const std = @import("std");
const jack = @import("../src/c_jack_audio.zig");
const jack_t = jack.jack_t;
const jack_f = jack.jack_f;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const allocator = std.heap.c_allocator;
const smp_t = jack_t.jack_default_audio_sample_t;

pub const Node = struct {
    // used to avoid double renders
    ticks: u64,
    // the generate function is expected to call the generate functions of the inputs
    inputs: ?[]*Node, // only knows inputs, not outputs
    // holds generated results
    outputs: ?[]*[]f64,
    // used to process data, "clock", number of samples, returns outputs
    generate: fn(*Node, u64, u32) ?[]*[]f64,
    init: fn(*Node, *u8) bool,
    cleanup: fn(*Node) void,
    // holds implementation dependent data
    data: ?*c_void,
};

pub fn noop_generator(_node: *Node, _clock: u64, _nsmps: u32) ?[]*[]f64 {
    return null;
}

pub fn noop_init(_node: *Node, _client: *u8) bool {
    return false;
}

pub fn noop_cleanup(_node: *Node) void {
}

pub const MonoSink = struct {
    label: [*:0]const u8,
    out: *u8
};

pub fn sink(node: *Node) u8 {
    node.ticks = 0;
    node.generate = sink_generate;
    node.init = sink_init;
    node.cleanup = sink_cleanup;
    var sink_alloc: anyerror![]MonoSink = allocator.alloc(MonoSink, 1);
    if (sink_alloc) |the_sink| {
        var snk = &the_sink[0];
        snk.label = "output";
        node.data = @ptrCast(*c_void, snk);
    } else |err| {
        return 1;
    }
    var inputs_alloc: anyerror![]*Node = allocator.alloc(*Node, 1);
    if (inputs_alloc) |in| {
        node.inputs = in;
    } else |err| {
        return 1;
    }
    return 0;
}

pub fn sink_generate(node: *Node, ticks: u64, nsmps: u32) ?[]*[]f64 {
    node.ticks = ticks; // note that we ignore "ticks", as we only output as a final sink
    var src: *Node = undefined;
    if (node.inputs) |inputs| {
        src = inputs[0];
    } else {
        return null;
    }
    const dest = @ptrCast(*MonoSink, @alignCast(@alignOf(*MonoSink), node.data));

    if (src.generate(src, ticks, nsmps)) |in| {
        var buff_raw = jack_f.port_get_buffer(dest.out, nsmps);
        var input = in[0].*;
        var write_buffer = @ptrCast([*]smp_t, @alignCast(@alignOf(*smp_t), buff_raw));
        var i: usize = 0;
        while (i < nsmps) {
            write_buffer[i] = @floatCast(f32, input[i]);
            i += 1;
        }
    }
    return null;
}

pub fn sink_init(node: *Node, client: *u8) bool {
    const dest = @ptrCast(*MonoSink, @alignCast(@alignOf(*MonoSink), node.data));
    print("hooking up output: \"{}\"\n", .{dest.label});
    const nsmps = jack_f.get_buffer_size(client);
    var port_alloc = jack_f.port_register(client, dest.label,
        jack_t.JACK_DEFAULT_AUDIO_TYPE, jack_t.JackPortIsOutput, nsmps);
    if (port_alloc) |port| {
        dest.out = port;
        return true;
    } else {
        print("no more Jack ports available\n", .{});
        return false;
    }
}

pub fn sink_cleanup(node: *Node) void {
    // TODO - this *does* need freeing
    // allocator.free(node.data);
    // TODO - this *does* need freeing
    // allocator.free(node.inputs);
}

pub const MonoSource = struct {
    label: [*:0]const u8,
    in: *u8
};

pub fn source(node: *Node) u8 {
    node.ticks = 0;
    node.generate = source_generate;
    node.init = source_init;
    node.cleanup = source_cleanup;
    var source_alloc: anyerror![]MonoSource = allocator.alloc(MonoSource, 1);
    if (source_alloc) |the_source| {
        var src = &the_source[0];
        src.label = "input";
        node.data = @ptrCast(*c_void, src);
    } else |err| {
        return 1;
    }
    var outputs_alloc: anyerror![]*[]f64 = allocator.alloc(*[]f64, 1);
    if (outputs_alloc) |outputs| {
        node.outputs = outputs;
    } else |err| {
        return 1;
    }
    return 0;
}

pub fn source_generate(node: *Node, ticks: u64, nsmps: u32) ?[]*[]f64 {
    var outputs = node.outputs orelse return null;
    var output = outputs[0].*;
    print("node: {*}, node.ticks: {}, ticks: {}\n", .{node, node.ticks, ticks});
    if (node.ticks == ticks) { // only need to get data once per cycle
        return undefined;
    }
    node.ticks = ticks;
    const src = @ptrCast(*MonoSource, @alignCast(@alignOf(*MonoSource), node.data));
    if(@ptrCast(?[*]smp_t, @alignCast(@alignOf([*]smp_t), jack_f.port_get_buffer(src.in, nsmps)))) |in_buffer| {
         print("filling buffer {} from {}, {} ticks\n", .{output, in_buffer, ticks});
         for (in_buffer[0..nsmps]) |v, i| {
            output[i] = v;
         }
    } else {
        return undefined;
    }
    return outputs;
}

pub fn source_init(node: *Node, client: *u8) bool {
    const src = @ptrCast(*MonoSource, @alignCast(@alignOf(*MonoSource), node.data));
    print("hooking up input: \"{}\"\n", .{src.label});
    const nsmps = jack_f.get_buffer_size(client);
    var port_alloc = jack_f.port_register(client, src.label,
        jack_t.JACK_DEFAULT_AUDIO_TYPE, jack_t.JackPortIsInput, nsmps);
    if (port_alloc) |port| {
        src.in = port;
    } else {
        print("no more Jack ports available\n", .{});
        return false;
    }
    var buff_alloc: anyerror![]f64 = allocator.alloc(f64, nsmps);
    if (buff_alloc) |*buffer| {
        print("allocated buffer {}\n", .{buffer});
        if (node.outputs) |outputs| {
            outputs[0] = buffer;
        } else {
            // TODO - this *does* need freeing
            // allocator.free(buffer);
            return false;
        }
    } else |err| {
        return false;
    }
    return true;
}

pub fn source_cleanup(node: *Node) void {
    if (node.outputs) |outputs| {
        // TODO - this *does* need freeing
        // allocator.free(outputs[0]);
        allocator.free(outputs);
    }
    // TODO - this *does* need freeing
    // allocator.free(node.data);
}

pub fn nodes_to_void(nodes: *[]Node) *c_void {
    return @ptrCast(*c_void, nodes);
}

pub fn void_to_nodes(data: *c_void) *[]Node {
    return @ptrCast(*[]Node, @alignCast(@alignOf(*[]Node), data));
}

pub fn blank_node(n: u64) []Node {
    var node_alloc: anyerror![]Node = allocator.alloc(Node, n);
    if (node_alloc) |the_node| {
        var node = &the_node[0];
        node.ticks = 0;
        node.outputs = undefined;
        node.inputs = undefined;
        node.generate = noop_generator;
        node.init = noop_init;
        node.cleanup = noop_cleanup;
        node.data = undefined;
        return the_node;
    } else |err| {
        return undefined;
    }
}

pub fn free_node(node: []Node) void {
    allocator.free(node);
}

const testing = std.testing;
test "nodes allocation and free" {
    var node: []Node = blank_node(1);
    free_node(node);
}

test "node void casting round trip" {
    var node: []Node = blank_node(1);
    free_node(node);
    var cross: *c_void = nodes_to_void(&node);
    var node_alias = void_to_nodes(cross).*;
    node_alias[0].ticks = 42;
    testing.expectEqual(node[0].ticks, 42);
}

test "node cast then iteration" {
    var nodes_source: []Node = blank_node(10);
    defer free_node(nodes_source);
    var nodes_ptr = void_to_nodes(nodes_to_void(&nodes_source));
    var nodes = nodes_ptr.*;
    for (nodes) |a_node, i| {
        nodes[i].ticks = 12;
    }
}
