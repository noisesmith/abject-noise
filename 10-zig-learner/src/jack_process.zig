const std = @import("std");
const player = @import("../src/playhead.zig");
const copy = @import("../src/copy_node.zig");
const jack = @import("../src/c_jack_audio.zig").jack;
const exit = std.process.exit;
const print = std.debug.print;

pub fn prep(client: *jack.jack_client_t, user_data: ?*c_void) c_int {
    var context = context_userdata(user_data);

    var success = register_ports(client, context);
    if (success != 0)
        return success;

    success = jack.jack_activate(client);
    if (success != 0) {
        print("cannot activate client\n", .{});
        return success;
    }

    success = connect_ports(client, context);
    return success;
}

fn register_ports(client: *jack.jack_client_t, context: *copy.Data) c_int {
    // TODO - generalize for more nodes
    return copy.register_ports(client, context);
}

fn connect_ports(client: *jack.jack_client_t, context: *copy.Data) c_int {
    // TODO - generalize for more nodes
    return copy.connect_ports(client, context);
}

pub fn process_audio(nframes: jack.jack_nframes_t, user_data: ?*c_void) callconv(.C) c_int {
    // TODO - generalize for more nodes
    return copy.process_audio(nframes, context_userdata(user_data));
}

pub fn shutdown(user_data: ?*c_void) callconv(.C) void {
    copy.cleanup(context_userdata(user_data));
    exit(1);
}

fn context_userdata(user_data: ?*c_void) *copy.Data {
    const context_alignment = @alignOf(*copy.Data);
    const aligned = @alignCast(context_alignment, user_data);
    return @ptrCast(*copy.Data, aligned);
}
