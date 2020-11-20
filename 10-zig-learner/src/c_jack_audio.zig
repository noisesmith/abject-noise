pub const jack_t = @cImport({
    @cInclude("jack/jack.h");
});

// the following exists so that it can be mocked out for testing / debugging
// the types will be used as normal from jack_t
pub const jack_f = .{
    .activate = jack_t.jack_activate,
    .client_close = jack_t.jack_client_close,
    .client_open = jack_t.jack_client_open,
    .get_client_name = jack_t.jack_get_client_name,
    .get_sample_rate = jack_t.jack_get_sample_rate,
    .on_shutdown = jack_t.jack_on_shutdown,
    .port_get_buffer = jack_t.jack_port_get_buffer,
    .port_register = jack_t.jack_port_register,
    .set_process_callback = jack_t.jack_set_process_callback
};
