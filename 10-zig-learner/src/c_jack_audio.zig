// only use this for types please...
const jack_raw = @import("../src/jack_raw.zig");
pub const jack_t = jack_raw.jack_lib;
const jack_debug = @import("../src/jack_debug.zig");

const debug = false;

pub const jack_f = blk: {
    if (!debug) {
        break :blk jack_raw.jack_f;
    } else {
        break :blk jack_debug.jack_f;
    }
};
