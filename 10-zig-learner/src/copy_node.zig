const std = @import("std");
const jack = @import("../src/c_jack_audio.zig").jack;
const node = @import("../src/audio_node.zig");
const alloc = std.heap.c_allocator.alloc;
const free = std.heap.c_allocator.free;
const destroy = std.heap.c_allocator.destroy;
const print = std.debug.print;

pub const Data = struct {
    input: node.Input,
    output: node.Output
};

pub fn register_ports(client: *jack.jack_client_t, context: *Data) c_int {
    context.input.ports = alloc(*jack.jack_port_t, 1) catch |_| return 1;
    if (context.input.ports) |in| {
            if (jack.jack_port_register(client, "input",
                    jack.JACK_DEFAULT_AUDIO_TYPE, jack.JackPortIsInput, 0)) |port| {
                    in[0] = port;
                } else {
                    print("no more Jack ports available\n", .{});
                    return 1;
                }
    }
    context.output.ports = alloc(*jack.jack_port_t, 1) catch |_| return 1;
    if (context.output.ports) |out| {
            if (jack.jack_port_register(client, "output",
                    jack.JACK_DEFAULT_AUDIO_TYPE, jack.JackPortIsOutput, 0)) |port| {
                out[0] = port;
            } else {
                print("no more Jack ports available\n", .{});
                return 1;
            }
    }
    return 0;
}

pub fn cleanup(context: *Data) void {
    if (context.input.ports) |in|
        free(in);
    if (context.output.ports) |out|
        free(out);
}

pub fn connect_ports(client: *jack.jack_client_t, context: *Data) c_int {
    var in_ports: *?[*:0]const u8 = undefined;
    var out_ports: *?[*:0]const u8 = undefined;
    const hardware_output = jack.JackPortIsPhysical | jack.JackPortIsOutput;
    in_ports = jack.jack_get_ports(client, null, null, hardware_output) orelse {
        print("no physical capture ports available\n", .{});
        return 1;
    };
    defer destroy(in_ports);

    // if we were doing more than just using the first port returned, it would be something like:
    // for (std.mem.span(ports)) |port| { // do something }
    if (context.input.ports) |in|
        if (jack.jack_connect(client, in_ports.*, jack.jack_port_name(in[0])) != 0)
            print("cannot connect input ports\n", .{});
    const hardware_input = jack.JackPortIsPhysical | jack.JackPortIsInput;
    out_ports = jack.jack_get_ports(client, null, null, hardware_input) orelse {
        print("no physical playback ports available\n", .{});
        return 1;
    };
    defer destroy(out_ports);
    if (context.output.ports) |out|
        if (jack.jack_connect(client, jack.jack_port_name(out[0]), out_ports.*) != 0)
            print("connot connect output ports", .{});
    return 0;
}

pub fn process_audio(nframes: jack.jack_nframes_t, context: *Data) callconv(.C) c_int {
    var in: ?[*]u8 = undefined;
    var out: ?[*]u8 = undefined;
    if (context.input.ports) |in_port|
       in = @ptrCast([*]u8, jack.jack_port_get_buffer(in_port[0], nframes));
    if (context.output.ports) |out_port|
       out = @ptrCast([*]u8, jack.jack_port_get_buffer(out_port[0], nframes));
    if (in) |source|
        if (out) |dest|
            @memcpy(dest, source, @sizeOf(jack.jack_default_audio_sample_t) * nframes);
    // just copy input to output
    return 0;
}
