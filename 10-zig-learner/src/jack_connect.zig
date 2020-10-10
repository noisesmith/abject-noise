const std = @import("std");
const print = std.debug.print;
const sleep = std.time.sleep;
const jack = @import("../src/c_jack_audio.zig").jack;

const jack_cb_t = fn(u32, ?*c_void) callconv(.C) c_int;
const jack_prep_t = fn(*jack.jack_client_t, ?*c_void) c_int;
const jack_cleanup_t = fn(?*c_void) callconv(.C) void;

pub fn start_audio(server_name: ?[*:0]const u8,
    user_data: *c_void,
    process_callback: jack_cb_t,
    prep_callback: jack_prep_t,
    cleanup_callback: jack_cleanup_t) u8 {
    var status: jack.jack_status_t = undefined; // output param on the next call
    const client = jack.jack_client_open("zig-learner", .JackNullOption, &status, server_name) orelse {
        print("jack_client_open() failed, status = {}\n", .{@enumToInt(status)});
        if ((@enumToInt(status) & jack.JackServerFailed) != 0) {
            print("failed to connect to JACK server\n", .{});
        }
        return 1;
    };
    defer _ = jack.jack_client_close(client);

    if (handle_status(client, status) != 0)
        return 2;

    // set up callbacks
    _ = jack.jack_set_process_callback(client, process_callback, user_data);
    jack.jack_on_shutdown(client, cleanup_callback, user_data);
    // info
    print("engine sample rate: {}\n", .{jack.jack_get_sample_rate(client)});

    if (prep_callback(client, user_data) != 0)
        return 3;

    // don't return until we are done processing audio
    sleep(std.math.maxInt(u64));
    return 0;
}

fn handle_status(client: *jack.jack_client_t, status: jack.jack_status_t) u8 {
    if ((@enumToInt(status) & jack.JackServerStarted) != 0) {
        print("new jack server started\n", .{});
    }
    if ((@enumToInt(status) & jack.JackNameNotUnique) != 0) {
        const client_name = jack.jack_get_client_name(client);
        print("unique name '{}' assigned\n", .{client_name});
    }

    return 0;
}
