const std = @import("std");
const jack_connect = @import("../src/jack_connect.zig");
const jack_process = @import("../src/jack_process.zig");
const node = @import("../src/audio_node.zig");

pub fn main() anyerror!u8 {
    var jack_server_name: ?[*:0]const u8 = null;
    var source_data = node.MonoSource{
        .label = "input",
        .in = undefined
    };
    var source_inputs = [1][]f64{undefined};
    var nodes = node.blank_node(2);
    defer node.free_node(nodes);
    //var source_node = node.Node{
    //    .ticks = 0,
    nodes[0].outputs = &source_inputs;
    //    .inputs = undefined,
    //    .generate = node.noop_generator,
    //    .init = node.noop_init,
    //    .cleanup = node.noop_cleanup,
    //    .data = undefined
    //};
    node.source(&nodes[0], &source_data);

    var sink_data = node.MonoSink{
        .label = "output",
        .out = undefined
    };
    var sink_input = [_]node.Node{nodes[0]};
    //var sink_node = node.Node{
    //    .ticks = 0,
        nodes[1].inputs = &sink_input;
    //    .outputs = undefined,
    //    .generate = node.noop_generator,
    //    .init = node.noop_init,
    //    .cleanup = node.noop_cleanup,
    //    .data = undefined
    //};
    node.sink(&nodes[1], &sink_data);
    const jack_result = jack_connect.start_audio(
        jack_server_name,
        &nodes,
        jack_process.process_audio,
        jack_process.prep,
        jack_process.shutdown
        );
    std.log.info("audio processing task returned {}.\n", .{jack_result});
    return jack_result;
}
