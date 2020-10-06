const std = @import("std");
const env = @import("../src/audio_envelope.zig");
//const print = std.debug.print;

pub const Head = struct {
    contour: *env.Env,
    index: u64,
    stop_at: u64,
    pause: bool,
};


/// a player or a recorder, depending on source / dest
pub fn transfer(frame_count: u64, state: *Head, source: [*]f64, dest: [*]f64) u64 { // TODO - also stereo
    // fills up to frame_count samples of source, into dest
    // the data offset logic should all be in the construction of source / dest slices
    var write_count = 0;
    var io_count = state.stop_at - state.index;
    if (frame_count < io_count)
        io_count = frame_count;
    if (source.len < io_count)
        io_count = source.len;
    if (dest.len < io_count)
        io_count = dest.len;
    // TODO - what does state.index mean and how does that relate to source / dest ?
    while (write_count < io_count) {
        const scale = env.env_next(state.contour);
        if (!state.pause) {
            dest[write_count] += scale * source[state.index];
            state.index += 1;
        }
        write_count += 1;
    }
    return write_count;
}
