const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const allocator = std.heap.c_allocator;
const jack_raw = @import("../src/jack_raw.zig");
const jack_t = jack_raw.jack_lib;
const jack_rawf = jack_raw.jack_f;

pub const jack_f = .{
    .activate = activate,
    .client_close = client_close,
    .client_open = client_open,
    .get_client_name = get_client_name,
    .get_sample_rate = get_sample_rate,
    .on_shutdown = on_shutdown,
    .port_get_buffer = port_get_buffer,
    .port_register = port_register,
    .set_process_callback = set_process_callback
};

// TODO - create a new opaquely wrapped "client" object,
// so that this and jack_raw can have the same API

// TODO - static data structure for holding registered client callbacks
//var callbacks = .{
//    .on_shutdown: ?*jack_t.JackShutdownCallback = undefined,
//    .process: ?*jack_t.JackProcessCallback = undefined
//};


// TODO - update registry
fn activate(client: *jack_t.jack_client_t) c_int {
    // return jack_lib.jack_activate(client);
    print("debug: activation of client {}\n", .{client});
    return 0;
}


// TODO - update registry
fn client_close(client: *jack_t.jack_client_t) c_int {
    // return jack_lib.jack_client_close(client);
    print("debug: close of client {}\n", .{client});
    return 0;
}

// TODO - update registry
// TODO - opaque client
fn client_open(client_name: ?[*:0]const u8,
    options: jack_t.jack_options_t,
    status: *jack_t.jack_status_t,
    server_name: ?[*:0]const u8) ?*jack_t.jack_client_t {
    var client = jack_rawf.client_open(client_name, options, status, server_name);
    print("debug: allocation of client {}\n", .{client});
    return client;
}

// TODO - use registry
fn get_client_name(client: *jack_t.jack_client_t) [*:0]const u8 {
    // return jack_lib.jack_get_client_name(client);
    print("debug: getting name of client {}\n", .{client});
    return "debug-client";
}

fn get_sample_rate(client: *jack_t.jack_client_t) jack_t.jack_nframes_t {
    // return jack_lib.jack_get_sample_rate(client);
   print("debug: getting sr of client {}\n", .{client});
   return 48000;
}

// TODO - update registry
fn on_shutdown(client: *jack_t.jack_client_t,
    cb: jack_t.JackShutdownCallback,
    arg: *c_void) void {
   print("debug: setting shuttdown callback for client {}\n", .{client});
   // jack_lib.jack_on_shutdown(client, cb, arg);
}

// TODO - mock out buffer fetch with alloc
fn port_get_buffer(port: *jack_t.jack_port_t,
    nframes: jack_t.jack_nframes_t) ?*c_void {
    return jack_rawf.port_get_buffer(port, nframes);
}

// TODO - mock out buffer register, or noop?
fn port_register(client: *jack_t.jack_client_t,
    port_name: [*:0]const u8,
    port_type: [*:0]const u8,
    flags: u64,
    buffer_size: u64) ?*jack_t.jack_port_t {
    return jack_rawf.port_register(client, port_name, port_type, flags, buffer_size);
}

// TODO - update registry
fn set_process_callback(client: *jack_t.jack_client_t,
    cb: jack_t.JackProcessCallback,
    arg: *c_void) c_int {
    return jack_rawf.set_process_callback(client, cb, arg);
}
