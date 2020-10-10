const jack = @import("../src/c_jack_audio.zig").jack;

pub const Node = struct {
    id: u64,
    inputs: []Node,
    outputs: []Node,
    // used to process data, number of samples, buffers of generated data
    generate: fn(*Node, u64, *[]f64) u64,
    // holds implementation dependent data
    data: ?*c_void,
};

pub const Input = struct {
    ports: ?[]*jack.jack_port_t
};

pub const Output = struct {
    ports: ?[]*jack.jack_port_t
};

