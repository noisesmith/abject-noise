const std = @import("std");
const node = @import("../src/audio_node.zig");
const jack = @import("../src/c_jack_audio.zig").jack;
const exit = std.process.exit;
const print = std.debug.print;

pub fn prep(client: *jack.jack_client_t, nodes: []node.Node) bool {

    for (nodes) |node| {
        if (node.ticks == -1) {
            node.ticks = 0;
            if (!prep(client, node.inputs))
                return false;
        }
    }
    return true;
}

pub fn process_audio(nframes: jack.jack_nframes_t, data: ?*c_void) callconv(.C) c_int {
    const nodes = @ptrCast([]node.Node, @alignCast(@alignOf([]node.Node, data)));
    var ticks = 0;
    for (nodes) |node|
        if (node.ticks > ticks)
            ticks = node.ticks;
    for (nodes) |node|
        _ = node.generate(node, ticks, nframes);
}

pub fn shutdown(user_data: ?*c_void) callconv(.C) void {
    const nodes = @ptrCast([]node.Node, @alignCast(@alignOf([]node.Node, data)));
    for (nodes) |node|
        node.cleanup(node);
    exit(0);
}
