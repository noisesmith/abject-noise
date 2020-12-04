const std = @import("std");
const print = std.debug.print;
const jack_connect = @import("../src/jack_connect.zig");
const jack_process = @import("../src/jack_process.zig");
const node = @import("../src/audio_node.zig");

pub fn main() anyerror!u8 {
    var jack_server_name: ?[*:0]const u8 = null;
    var nodes = node.blank_node(2);
    print("running with nodes {*}, {*}\n", .{&nodes[0], &nodes[1]});
    defer node.free_node(nodes);

    var source_error = node.source(&nodes[0]);
    if (source_error != 0)
        return source_error;

    var sink_error = node.sink(&nodes[1]);
    if (sink_error != 0)
        return sink_error;

    if (nodes[1].inputs) |inputs| {
        inputs[0] = &nodes[0];
    } else {
        return 1;
    }

    sort_nodes(nodes);

    const jack_result = jack_connect.start_audio( jack_server_name,
        &nodes, jack_process.process_audio, jack_process.prep, jack_process.shutdown);

    std.log.info("audio processing task returned {}.\n", .{jack_result});
    return jack_result;
}

fn sort_nodes(nodes: []node.Node) void {
    // TODO - eventually we want an in-place topological sort here
}
