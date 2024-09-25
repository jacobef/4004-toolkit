const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const emulator_exe = b.addExecutable(.{
        .name = "4004-emulator",
        .root_source_file = b.path("src/emulator.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(emulator_exe);

    const assembler_exe = b.addExecutable(.{
        .name = "4004-assembler",
        .root_source_file = b.path("src/assembler.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(assembler_exe);
}
