const std = @import("std");
const jack = @import("../src/c_jack_audio.zig").jack;

pub const Node = struct {
    // used to avoid double renders
    ticks: i64,
    // the generate function is expected to call the generate functions of the inputs
    inputs: ?[]Node, // only knows inputs, not outputs
    // holds generated results
    outputs: ?[]*f64,
    // used to process data, "clock", number of samples, returns outputs
    generate: fn(*Node, u64, u64) ?[]*f64,
    init: fn(*Node, *jack.jack_client_t) bool,
    cleanup: fn(*Node) void,
    // holds implementation dependent data
    data: ?*c_void,
};

pub const MonoSink = struct {
    label: [*:0]const u8,
    out: *jack.jack_port_t
};

pub fn sink(id: u64, node: *Node, port: *MonoSink) void {
    node.ticks = -1;
    node.generate = sink_generator;
    node.init = sink_init;
    node.cleanup = sink_cleanup;
    node.data = @ptrCast(*c_void, port);
}

pub fn sink_generator(node: *Node, ticks: u64, nsmps: u64) ?[]*f64 {
    node.ticks = ticks; // note that we ignore "ticks", as we only output as a final sink
    const source = node.inputs[0] orelse return null;
    const dest = @ptrCast(*MonoSink, @alignCast( @alignOf(*MonoSink, node.data)));
    // generate data from source
    generated = source.generate(source, ticks, nsmps);
    // copy that data to dest.out
    const copy_count = @sizeOf(jack.jack_default_audio_sample_t) * nframes;
    if (@ptrCast([*]u8, jack.jack_port_get_buffer, dest.out)) |output|
        @memcpy(output, generated, copy_count);
    return null;
}

pub fn sink_init(node: *Node, client: jack.jack_client_t) bool {
    const dest = @ptrCast(*MonoSInk, @alignCast(@alignOf(*MonoSink, node.data)));
    if (jack.jack_port_register(client, dest.label, jack.JACK_DEFAULT_AUDIO_TYPE, jack.JackPortIsInput, 0)) |port| {
        dest.out = port;
        return true;
    } else {
        print("no more Jack ports available\n", .{});
        return false;
    }
}

pub fn sink_cleanup(node: *Node) bool {
    // const dest = @ptrCast(*MonoSInk, @alignCast(@alignOf(*MonoSink, node.data)));
    return true;
}

pub const MonoSource = struct {
    label: [*:0]const u8,
    in: *jack.jack_port_t
};

pub fn source(id: u64, node: *Node, port: *MonoSource) void {
    node.id = id;
    node.ticks = -1;
    node.generate = source_generator;
    node.init = node_generate;
    node.cleanup = source_cleanup;
    node.data = @ptrCast(*c_void, port);
}

pub fn source_generate(node: *Node, ticks: u64, nsmps: u64) ?[]f64 {
    var outputs = node.outputs orelse return null;
    if (node.ticks != ticks) { // only need to get data once per cycle
        const source = @ptrCast(*MonoSource, @alignCast(@alignOf(*MonoSource, node.data)));
        const copy_count = @sizeOf(jack.jack_default_audio_sample_t) * nframes;
        if(@ptrCast([*]u8, jack.jack_port_get_buffer(source.in, nframes))) | buffer|
            // just forward it
            outputs[0] = buffer;
        node.ticks = ticks;
    }
    return node.outputs;
}

pub fn source_init(node: *Node, client: jack.jack_client_t) bool {
    // const nsmps = jack.jack_get_buffer_size(client);
    const source = @ptrCast(*MonoSOurce, @alignCast(@alignOf(*MonoSource, node.data)));
    if (jack.jack_port_register(client, source.label, jack.JACK_DEFAULT_AUDIO_TYPE, jack.JackPortIsInput, 0)) |port| {
        source.in = port;
        return true;
    } else {
        print("no more Jack ports available\n", .{});
        return false;
    }
}

pub fn source_cleanup(node: *Node) bool {
    // const source = @ptrCast(*MonoSOurce, @alignCast(@alignOf(*MonoSource, node.data)));
    return true;
}
