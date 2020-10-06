const std = @import("std");
const Pkg = std.build.Pkg;
const Builder = std.build.Builder;

const pkgs = struct {
    const app = Pkg{
        .name = "app",
        .path = "src/main.zig",
        .dependencies = &[_]Pkg{
            jack_process, jack_connect
        },
    };

    const jack_connect = Pkg{
        .name = "connect",
        .path = "src/jack_connect.zig",
        .dependencies = &[_]Pkg{
            c_jack,
        },
    };

    const jack_process = Pkg{
        .name = "process",
        .path = "src/jack_process.zig",
        .dependencies = &[_]Pkg{
            c_jack, playhead, envelope,
        },
    };

    const c_jack = Pkg{
        .name = "jack",
        .path = "src/c_jack_audio.zig",
    };

    const playhead = Pkg{
        .name = "playhead",
        .path = "src/playhead.zig",
        .dependencies = &[_]Pkg{
            envelope,
        },
    };

    const envelope = Pkg{
        .name = "envelope",
        .path = "src/envelope.zig",
    };
};

pub fn build(b: *Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("jack-learner", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.addPackage(pkgs.jack_connect);
    exe.addPackage(pkgs.jack_process);

    exe.linkLibC();
    exe.linkSystemLibrary("jack");

    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
