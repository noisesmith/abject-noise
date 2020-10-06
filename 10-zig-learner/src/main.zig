const std = @import("std");
const jack_connect = @import("../src/jack_connect.zig");
const jack_process = @import("../src/jack_process.zig");

pub fn main() anyerror!u8 {
    var jack_server_name: ?[*:0]const u8 = null;
    var audio_context = jack_process.Context{
        .input = undefined,
        .output = undefined,
    };
    const jack_result = jack_connect.start_audio(
        jack_server_name,
        @ptrCast(*c_void, &audio_context),
        jack_process.process_audio,
        jack_process.prep
        );
    std.log.info("audio processing task returned {}.\n", .{jack_result});
    return jack_result;
}
