const std = @import("std");
const jack = @import("../src/c_jack_audio.zig").jack;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const allocator = std.heap.c_allocator;

pub const Node = struct {
    // used to avoid double renders
    ticks: u64,
    // the generate function is expected to call the generate functions of the inputs
    inputs: ?[]Node, // only knows inputs, not outputs
    // holds generated results
    outputs: ?[][]f64,
    // used to process data, "clock", number of samples, returns outputs
    generate: fn(*Node, u64, u32) ?[][]f64,
    init: fn(*Node, *jack.jack_client_t) bool,
    cleanup: fn(*Node) void,
    // holds implementation dependent data
    data: ?*c_void,
};

pub fn noop_generator(_node: *Node, _clock: u64, _nsmps: u32) ?[][]f64 {
    return null;
}

pub fn noop_init(_node: *Node, _client: *jack.jack_client_t) bool {
    return false;
}

pub fn noop_cleanup(_node: *Node) void {
}

pub const MonoSink = struct {
    label: [*:0]const u8,
    out: *jack.jack_port_t
};

pub fn sink(node: *Node, port: *MonoSink) void {
    node.ticks = 0;
    node.generate = sink_generate;
    node.init = sink_init;
    node.cleanup = sink_cleanup;
    node.data = @ptrCast(*c_void, port);
}

pub fn sink_generate(node: *Node, ticks: u64, nsmps: u32) ?[][]f64 {
    node.ticks = ticks; // note that we ignore "ticks", as we only output as a final sink
    var src: *Node = undefined;
    if (node.inputs) |inputs| {
        src = &inputs[0];
    } else {
        return null;
    }
    const dest = @ptrCast(*MonoSink, @alignCast(@alignOf(*MonoSink), node.data));
    // generate data from source
    const generated = @ptrCast(?[*]u8, &src.generate(src, ticks, nsmps));
    // copy that data to dest.out
    const copy_count = @sizeOf(jack.jack_default_audio_sample_t) * nsmps;
    if (@ptrCast(?[*]u8, jack.jack_port_get_buffer(dest.out, nsmps))) |output|
        if (generated) |in|
            @memcpy(output, in, copy_count);
    return null;
}

pub fn sink_init(node: *Node, client: *jack.jack_client_t) bool {
    const dest = @ptrCast(*MonoSink, @alignCast(@alignOf(*MonoSink), node.data));
    print("hooking up output: \"{}\"\n", .{dest.label});
    if (jack.jack_port_register(client, dest.label, jack.JACK_DEFAULT_AUDIO_TYPE, jack.JackPortIsOutput, 0)) |port| {
        dest.out = port;
        return true;
    } else {
        print("no more Jack ports available\n", .{});
        return false;
    }
}

pub fn sink_cleanup(node: *Node) void {
    // const dest = @ptrCast(*MonoSInk, @alignCast(@alignOf(*MonoSink, node.data)));
}

pub const MonoSource = struct {
    label: [*:0]const u8,
    in: *jack.jack_port_t
};

pub fn source(node: *Node, port: *MonoSource) void {
    node.ticks = 0;
    node.generate = source_generate;
    node.init = source_init;
    node.cleanup = source_cleanup;
    node.data = @ptrCast(*c_void, port);
}

pub fn source_generate(node: *Node, ticks: u64, nsmps: u32) ?[][]f64 {
    var outputs = node.outputs orelse return null;
    if (node.ticks != ticks) { // only need to get data once per cycle
        const src = @ptrCast(*MonoSource, @alignCast(@alignOf(*MonoSource), node.data));
        const copy_count = @sizeOf(jack.jack_default_audio_sample_t) * nsmps;
        if(@ptrCast(?[*]f64, @alignCast(@alignOf([*]f64), jack.jack_port_get_buffer(src.in, nsmps)))) | buffer|
            // just forward it
            outputs[0] = buffer[0..nsmps];
        node.ticks = ticks;
    }
    return node.outputs;
}

pub fn source_init(node: *Node, client: *jack.jack_client_t) bool {
    // const nsmps = jack.jack_get_buffer_size(client);
    const src = @ptrCast(*MonoSource, @alignCast(@alignOf(*MonoSource), node.data));
    print("hooking up input: \"{}\"\n", .{src.label});
    if (jack.jack_port_register(client, src.label, jack.JACK_DEFAULT_AUDIO_TYPE, jack.JackPortIsInput, 0)) |port| {
        src.in = port;
        return true;
    } else {
        print("no more Jack ports available\n", .{});
        return false;
    }
}

pub fn source_cleanup(node: *Node) void {
    // const source = @ptrCast(*MonoSOurce, @alignCast(@alignOf(*MonoSource, node.data)));
}

/// a core issue:

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
