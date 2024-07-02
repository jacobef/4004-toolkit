const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Emulator executable
    const emulator_exe = b.addExecutable(.{
        .name = "4004zig-emulator",
        .root_source_file = b.path("src/emulator.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(emulator_exe);
    const run_emulator_cmd = b.addRunArtifact(emulator_exe);
    run_emulator_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_emulator_cmd.addArgs(args);
    }
    const run_emulator_step = b.step("run-emulator", "Run the emulator");
    run_emulator_step.dependOn(&run_emulator_cmd.step);

    const emulator_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/emulator.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_emulator_unit_tests = b.addRunArtifact(emulator_unit_tests);
    const test_emulator_step = b.step("test-emulator", "Run emulator unit tests");
    test_emulator_step.dependOn(&run_emulator_unit_tests.step);

    // Assembler executable
    const assembler_exe = b.addExecutable(.{
        .name = "4004zig-assembler",
        .root_source_file = b.path("src/assembler.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(assembler_exe);
    const run_assembler_cmd = b.addRunArtifact(assembler_exe);
    run_assembler_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_assembler_cmd.addArgs(args);
    }
    const run_assembler_step = b.step("run-assembler", "Run the assembler");
    run_assembler_step.dependOn(&run_assembler_cmd.step);

    const assembler_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/assembler.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_assembler_unit_tests = b.addRunArtifact(assembler_unit_tests);
    const test_assembler_step = b.step("test-assembler", "Run assembler unit tests");
    test_assembler_step.dependOn(&run_assembler_unit_tests.step);
}
