const std = @import("std");
const jack_connect = @import("../src/jack_connect.zig");
const jack_process = @import("../src/jack_process.zig");
const node = @import("../src/audio_node.zig");

pub fn main() anyerror!u8 {
    var jack_server_name: ?[*:0]const u8 = null;
    const source_data = node.MonoSource{
        .label = "input",
        .in = undefined
    };
    const outputs: []*f64 = &.{null};
    const source_node = node.Node{
        .inputs = undefined,
        .outputs = &outputs
    };
    const source_gen = node.source(source_node, source_data);
    const sink_data = node.MonoSink{
        .label = "output",
    };
    const sink_node = node.Node{
        .inputs = .{source_gen},
        .outputs = undefined
    };
    const sink_gen = node.sink(sink_node, sink_data);
    const audio_context = .{
        source_gen,
        sink_gen
    };
    const jack_result = jack_connect.start_audio(
        jack_server_name,
        audio_context,
        jack_process.process_audio,
        jack_process.prep,
        jack_process.shutdown
        );
    std.log.info("audio processing task returned {}.\n", .{jack_result});
    return jack_result;
}
