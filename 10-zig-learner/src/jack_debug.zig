const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const allocator = std.heap.c_allocator;
const jack_raw = @import("../src/jack_raw.zig");
const jack_t = jack_raw.jack_lib;

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

const DummyClient = struct {
    name: ?[*:0]const u8,
    active_p: bool,
    shutdown_fn: jack_t.JackShutdownCallback,
    shutdown_data: *u8,
    process_fn: jack_t.JackProcessCallback,
    process_data: *u8
};

fn get_client(data: *u8) *DummyClient {
    return @ptrCast(*DummyClient, @alignCast(@alignOf(*DummyClient), data));
}

fn as_data(client: *DummyClient) ?*u8 {
    return @ptrCast(*u8, client);
}


fn activate(data: *u8) c_int {
    var client = get_client(data);
    print("debug: activation of client {}\n", .{client});
    client.active_p = true;
    return 0;
}


fn client_close(data: *u8) c_int {
    var client = get_client(data);
    print("debug: close of client {}\n", .{client});
    return 0;
}

fn client_open(client_name: ?[*:0]const u8,
    options: jack_t.jack_options_t,
    status: *jack_t.jack_status_t,
    server_name: ?[*:0]const u8) ?*u8 {
    var client_alloc: anyerror![]DummyClient = allocator.alloc(DummyClient, 1);
    if (client_alloc) |the_client| {
        print("debug: allocation of client {}\n", .{the_client});
        var client = &the_client[0];
        client.name = client_name;
        client.active_p = false;
        return @ptrCast(*u8, client);
    } else |err| {
        return undefined;
    }
}

fn get_client_name(data: *u8) ?[*:0]const u8 {
    var client = get_client(data);
    print("debug: getting name of client {}, {}\n", .{client, client.name});
    return client.name;
}

fn get_sample_rate(data: *u8) jack_t.jack_nframes_t {
    var client = get_client(data);
    print("debug: getting sr of client {}\n", .{client});
    return 48000;
}

fn on_shutdown(data: *u8,
    cb: jack_t.JackShutdownCallback,
    arg: *c_void) void {
    var client = get_client(data);
    print("debug: setting shuttdown callback for client {}\n", .{client});
    client.shutdown_data = @ptrCast(*u8, arg);
    client.shutdown_fn = cb;
}

fn port_get_buffer(port: ?*u8,
    nframes: jack_t.jack_nframes_t) ?*c_void {
    if (port) |p| {
        return @ptrCast(*c_void, p);
    } else {
        return undefined;
    }
}

fn port_register(client: *u8,
    port_name: [*:0]const u8,
    port_type: [*:0]const u8,
    flags: u64,
    buffer_size: u64) ?*u8 {
    var buffer: anyerror![]f64 = allocator.alloc(f64, buffer_size);
    if (buffer) |b| {
        return @ptrCast(*u8, b);
    } else |err| {
        return undefined;
    }
}

fn set_process_callback(data: *u8,
    cb: jack_t.JackProcessCallback,
    arg: *c_void) c_int {
    var client = get_client(data);
    client.process_data = @ptrCast(*u8, arg);
    client.process_fn = cb;
    return 0;
}
