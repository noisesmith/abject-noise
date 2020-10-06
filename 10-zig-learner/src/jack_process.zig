const std = @import("std");
const player = @import("../src/playhead.zig");
const print = std.debug.print;
const jack = @import("../src/c_jack_audio.zig").jack;
const destroy = std.heap.c_allocator.destroy;

pub const Context = struct {
    input: ?*jack.jack_port_t,
    output: ?*jack.jack_port_t,
};

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

fn register_ports(client: *jack.jack_client_t, context: *Context) c_int {
    context.input = jack.jack_port_register(client, "input", jack.JACK_DEFAULT_AUDIO_TYPE,
        jack.JackPortIsInput, 0);
    context.output = jack.jack_port_register(client, "output", jack.JACK_DEFAULT_AUDIO_TYPE,
        jack.JackPortIsOutput, 0);

    if ((context.input == null) or (context.output == null)) {
        print("no more Jack ports available\n", .{});
        return 1;
    }
    return 0;
}

fn connect_ports(client: *jack.jack_client_t, context: *Context) c_int {
    var ports: *?[*:0]const u8 = undefined;
    const hardware_output = jack.JackPortIsPhysical | jack.JackPortIsOutput;
    ports = jack.jack_get_ports(client, null, null, hardware_output) orelse {
        print("no physical capture ports available\n", .{});
        return 1;
    };
    defer destroy(ports);

    // if we were doing more than just using the first port returned, it would be something like:
    // for (std.mem.span(ports)) |port| { // do something }
    if (jack.jack_connect(client, ports.*, jack.jack_port_name(context.input)) != 0) {
        print("cannot connect input ports\n", .{});
    }
    destroy(ports);
    const hardware_input = jack.JackPortIsPhysical | jack.JackPortIsInput;
    ports = jack.jack_get_ports(client, null, null, hardware_input) orelse {
        print("no physical playback ports available\n", .{});
        return 1;
    };
    if (jack.jack_connect(client, jack.jack_port_name(context.output), ports.*) != 0) {
        print("connot connect output ports", .{});
    }
    return 0;
}


pub fn process_audio(nframes: jack.jack_nframes_t, user_data: ?*c_void) callconv(.C) c_int {
    var context = context_userdata(user_data);
    var in = @ptrCast([*]u8, jack.jack_port_get_buffer(context.input, nframes));
    var out = @ptrCast([*]u8, jack.jack_port_get_buffer(context.output, nframes));

    // just copy input to output
    @memcpy(out, in, @sizeOf(jack.jack_default_audio_sample_t) * nframes);
    return 0;
}

fn context_userdata(user_data: ?*c_void) *Context {
    const context_alignment = @alignOf(*Context);
    const aligned = @alignCast(context_alignment, user_data);
    return @ptrCast(*Context, aligned);
}
