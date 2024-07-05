const std = @import("std");
const ArrayList = std.ArrayList;

const cpus = @import("cpu.zig");
const Intel4004 = cpus.Intel4004;

const instruction_spec = @import("instruction_spec.zig");
const InstructionSpec = instruction_spec.InstructionSpec;

const lookup = instruction_spec.initOpcodeLookup();

pub fn main() !void {
    var intel4004: Intel4004 = undefined;
    intel4004.reset();

    var cmd_args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = cmd_args.next();
    const executable_path = cmd_args.next() orelse return error.NoInputFileGiven;
    const executable_file = try std.fs.cwd().openFile(executable_path, .{ .mode = .read_only });
    _ = try executable_file.readAll(&intel4004.pram.bytes);

    for (0..100) |_| {
        try intel4004.single_step();
    }
    std.debug.print("{any}\n", .{intel4004});
    // for (intel4004.dram.banks) |bank| {
    //     for (bank.chips) |chip| {
    //         std.debug.print("{any}", .{chip});
    //     }
    // }
}
