pub const semi_sigmoid_curve_length = 1024;
pub const semi_sigmoid_fade_in: [semi_sigmoid_curve_length]f64 = blk:{
    var tmp: [semi_sigmoid_curve_length]f64 = undefined;
    var half_curve = semi_sigmoid_curve_length / 2;
    var total = @intToFloat(f64, half_curve);
    var i = 0;
    var increment: f64 = 1.0 / total;
    var frac: f64 = 0.0;
    // climb expo for first half
    while(i < half_curve) {
        // x^3 curve, reaching 0.5 when we reach half_curve
        tmp[i] = frac * frac * frac * 0.5;
        frac += increment;
        i += 1;
    }
    // climb y mirror around 0.5 of expo for second half
    while(i < semi_sigmoid_curve_length) {
        // y mirror of the above curve, 0.5 up to 1
        tmp[i] = 1.0 - tmp[half_curve - (i - half_curve)];
        i += 1;
    }
    break :blk tmp;
};

pub const semi_sigmoid_fade_out: [semi_sigmoid_curve_length]f64 = blk:{
    var tmp: [semi_sigmoid_curve_length]f64 = undefined;
    // the whole thing is a y mirror of fade_in along 0.5
    for (tmp) |*item, i|
        item.* = 1.0 - fade_in[i];
    break :blk tmp;
};

// TODO - simpler to have a "env_end" instead of "decay_start" ?
pub const Env = struct {
    next_env: ?Env,
    constant_factor: f64,
    loop: bool,
    time_offset: i32,
    decay_start: u32,
    curve_in: ?[*]f64,
    curve_out: ?[*]f64,
};

pub fn env_next(state: *Env) f64 {
    var scale = state.constant_factor;
    // envelopes stack
    var restart_time ?u32 = null;
    if (next_env) |nested|
        scale *= env_next(&nested);
    if (state.curve_in) |fade_in|
        if (state.time_offset > 0 and state.time_offset < fade_in.len)
            scale *=  fade_in[state.time_offset];
    var fade_pos = state.time_offset - state.decay_start;
    if (state.curve_out) |fade_out| {
        restart_time = state.decay_start + fade_out.len;
        if (fade_pos > 0 and fade_pos < fade_out.len)
            scale *= fade_out[fade_pos];
        if (fade_pos > fade_out.len)
            scale = 0.0;
    }
    state.time_offset += 1;
    if (restart_time) |trigger|
        if(state.loop == true and state.time_offset >= restart_time)
            state.time_offset = 0;
    return scale;
}
