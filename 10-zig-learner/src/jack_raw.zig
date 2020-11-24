pub const jack_lib = @cImport({
    @cInclude("jack/jack.h");
});

pub const jack_f = .{
    .activate = activate,
    .client_close = client_close,
    .client_open = client_open,
    .get_client_name = get_client_name,
    .get_sample_rate = get_sample_rate,
    .on_shutdown = on_shutdown,
    .port_get_buffer = jack_lib.jack_port_get_buffer,
    .port_register = port_register,
    .set_process_callback = set_process_callback
};

fn get_client (data: *u8) *jack_lib.jack_client_t {
    return @ptrCast(*jack_lib.jack_client_t, @alignCast(@alignOf(*jack_lib.jack_client_t), data));
}

fn as_data(client: *jack_lib.jack_client_t) ?*u8 {
    return @ptrCast(*u8, client);
}

fn activate(data: *u8) c_int {
    var client = get_client(data);
    return jack_lib.jack_activate(client);
}


fn client_close(data: *u8) c_int {
    var client = get_client(data);
    return jack_lib.jack_client_close(client);
}

fn client_open(client_name: ?[*:0]const u8,
    options: jack_lib.jack_options_t,
    status: *jack_lib.jack_status_t,
    server_name: ?[*:0]const u8) ?*u8 {
    var client = jack_lib.jack_client_open(client_name, options, status);
    if (client) |c| {
        return as_data(c);
    } else {
        return null;
    }
}

fn get_client_name(data: *u8) [*:0]const u8 {
    var client = get_client(data);
    return jack_lib.jack_get_client_name(client);
}

fn get_sample_rate(data: *u8) jack_lib.jack_nframes_t {
    var client = get_client(data);
    return jack_lib.jack_get_sample_rate(client);
}

fn on_shutdown(data: *u8,
    cb: jack_lib.JackShutdownCallback,
    arg: *c_void) void {
    var client = get_client(data);
    jack_lib.jack_on_shutdown(client, cb, arg);
}

// fn port_get_buffer(port: *jack_t.jack_port_t,
//     nframes: jack_t.jack_nframes_t) ?*c_void {
//     return jack_rawf.port_get_buffer(port, nframes);
// }

fn port_register(data: *u8,
    port_name: [*:0]const u8,
    port_type: [*:0]const u8,
    flags: u64,
    buffer_size: u64) ?*jack_lib.jack_port_t {
    var client = get_client(data);
    return jack_lib.jack_port_register(client, port_name, port_type, flags, buffer_size);
}

fn set_process_callback(data: *u8,
    cb: jack_lib.JackProcessCallback,
    arg: *c_void) c_int {
    var client = get_client(data);
    return jack_lib.jack_set_process_callback(client, cb, arg);
}
