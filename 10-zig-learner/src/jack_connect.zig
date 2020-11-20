const std = @import("std");
const print = std.debug.print;
const sleep = std.time.sleep;
const c_jack = @import("../src/c_jack_audio.zig");
const jack_t = c_jack.jack_t;
const jack_f = c_jack.jack_f;
const node = @import("../src/audio_node.zig");

const jack_cb_t = fn(u32, ?*c_void) callconv(.C) c_int;
const jack_prep_t = fn(*jack_t.jack_client_t, *c_void) c_int;
const jack_cleanup_t = fn(?*c_void) callconv(.C) void;

pub fn start_audio(server_name: ?[*:0]const u8,
    audio_context: *[]node.Node,
    process_callback: jack_cb_t,
    prep_callback: jack_prep_t,
    cleanup_callback: jack_cleanup_t) u8 {
    var status: jack_t.jack_status_t = undefined; // output param on the next call
    var ctx_userdata = node.nodes_to_void(audio_context);
    print("audio_context is {}, userdata is {}\n", .{audio_context, ctx_userdata});
    var debug = audio_context;
    const client = jack_f.client_open("zig-learner", .JackNullOption, &status, server_name) orelse {
        print("jack_client_open() failed, status = {}\n", .{@enumToInt(status)});
        if ((@enumToInt(status) & jack_t.JackServerFailed) != 0) {
            print("failed to connect to JACK server\n", .{});
        }
        return 1;
    };
    defer _ = jack_f.client_close(client);
    print("process connected to jack server\n", .{});

    debug = node.void_to_nodes(ctx_userdata);

    if (handle_status(client, status) != 0)
        return 2;

    // set up callbacks
    if (prep_callback(client, ctx_userdata) != 0)
        return 3;
    print("nodes prepared for synthesis\n", .{});
    _ = jack_f.set_process_callback(client, process_callback, ctx_userdata);
    print("process callback set\n", .{});
    jack_f.on_shutdown(client, cleanup_callback, ctx_userdata);
    // info
    print("engine sample rate: {}\n", .{jack_f.get_sample_rate(client)});

    if (jack_f.activate(client) != 0) {
        print("cannot activate client\n", .{});
        return 1;
    }

    // don't return until we are done processing audio
    sleep(std.math.maxInt(u64));
    return 0;
}

fn handle_status(client: *jack_t.jack_client_t, status: jack_t.jack_status_t) u8 {
    if ((@enumToInt(status) & jack_t.JackServerStarted) != 0) {
        print("new jack server started\n", .{});
    }
    if ((@enumToInt(status) & jack_t.JackNameNotUnique) != 0) {
        const client_name = jack_f.get_client_name(client);
        print("unique name '{}' assigned\n", .{client_name});
    }

    print("jack status OK\n", .{});
    return 0;
}
