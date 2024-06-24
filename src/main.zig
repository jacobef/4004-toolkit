const std = @import("std");
const ArrayList = std.ArrayList;

const cpus = @import("cpu.zig");
const Intel4004 = cpus.Intel4004;

const instructions = @import("instruction_spec.zig");
const InstructionSpec = instructions.InstructionSpec;

const lookup = instructions.initOpcodeLookup();

pub fn main() !void {

    // var allocator_factory = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = allocator_factory.allocator();

    var intel4004: Intel4004 = undefined;
    intel4004.reset();
    intel4004.pram.bytes[0] = 0b11010100;
    intel4004.pram.bytes[1] = 0b10000001;
    try intel4004.single_step();
    try intel4004.single_step();
    std.debug.print("{any}", .{intel4004});
}
