const std = @import("std");
const node = @import("../src/audio_node.zig");
const jack = @import("../src/c_jack_audio.zig").jack;
const exit = std.process.exit;
const print = std.debug.print;

pub fn prep(client: *jack.jack_client_t, data: *c_void) c_int {
    var nodes_ptr = node.void_to_nodes(data);
    var nodes = nodes_ptr.*;
    print("user_data {}, one set of nodes {}\n", .{data, nodes});
    for (nodes) |a_node, i| {
        print("one node\n", .{});
        if (a_node.ticks == 0) {
            print("init of node\n", .{});
            nodes[i].ticks = 1;
            if (prep(client, @ptrCast(*c_void, &nodes[i].inputs)) != 0)
                return 1;
        }
    }
    return 1;
}

pub fn process_audio(nframes: jack.jack_nframes_t, data: ?*c_void) callconv(.C) c_int {
    const nodes = @ptrCast(*[]node.Node, @alignCast(@alignOf(*[]node.Node), data));
    var ticks: u64 = 2;
    for (nodes.*) |a_node| {
        if (a_node.ticks > ticks)
            ticks = a_node.ticks;
    }
    for (nodes.*) |a_node, i|
        _ = a_node.generate(&nodes.*[i], ticks, nframes);
    return 0;
}

pub fn shutdown(user_data: ?*c_void) callconv(.C) void {
    const nodes = @ptrCast(*[]node.Node, @alignCast(@alignOf([]node.Node), user_data));
    for (nodes.*) |a_node, i|
        a_node.cleanup(&nodes.*[i]);
    exit(0);
}
