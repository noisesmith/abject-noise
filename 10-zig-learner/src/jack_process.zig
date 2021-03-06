const std = @import("std");
const node = @import("../src/audio_node.zig");
const jack_t = @import("../src/c_jack_audio.zig").jack_t;
const exit = std.process.exit;
const print = std.debug.print;

pub fn prep(client: *u8, data: *c_void) c_int {
    var nodes_ptr = node.void_to_nodes(data);
    var nodes = nodes_ptr.*;
    for (nodes) |a_node, i| {
        if (a_node.ticks == 0) {
            nodes[i].ticks = 1;
            if (!nodes[i].init(&nodes[i], client))
                return 1;
        }
    }
    return 0;
}

pub fn process_audio(nframes: jack_t.jack_nframes_t, data: ?*c_void) callconv(.C) c_int {
    if (data) |user_data| {
        var nodes_ptr = node.void_to_nodes(user_data);
        var nodes = nodes_ptr.*;
        var ticks: u64 = 2;
        for (nodes) |a_node| {
            if (a_node.ticks > ticks)
                ticks = a_node.ticks;
        }
        ticks = ticks + 1;
        for (nodes) |a_node, i| {
            print("generating for node {}, {*}\n", .{i, &nodes[i]});
            _ = a_node.generate(&nodes[i], ticks, nframes);
        }
    }
    return 0;
}

pub fn shutdown(user_data: ?*c_void) callconv(.C) void {
    const nodes = @ptrCast(*[]node.Node, @alignCast(@alignOf([]node.Node), user_data));
    for (nodes.*) |a_node, i|
        a_node.cleanup(&nodes.*[i]);
    exit(0);
}
