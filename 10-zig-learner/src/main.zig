const std = @import("std");
const jack_connect = @import("../src/jack_connect.zig");
const jack_process = @import("../src/jack_process.zig");
const copy = @import("../src/copy_node.zig");

pub fn main() anyerror!u8 {
    var jack_server_name: ?[*:0]const u8 = null;
    var audio_context = copy.Data{
        .input = undefined,
        .output = undefined,
    };
    const jack_result = jack_connect.start_audio(
        jack_server_name,
        @ptrCast(*c_void, &audio_context),
        jack_process.process_audio,
        jack_process.prep,
        jack_process.shutdown
        );
    std.log.info("audio processing task returned {}.\n", .{jack_result});
    return jack_result;
}
