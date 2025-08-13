const std = @import("std");
const cpu = @import("cpu.zig");
const instruction_spec = @import("instruction_spec.zig");
const Intel4004 = cpu.Intel4004;

pub fn disassemble_instruction(intel4004: *Intel4004, addr: u12, allocator: std.mem.Allocator) ![]u8 {
    const byte1 = intel4004.pram.bytes[addr];
    const inst_spec = instruction_spec.getInstructionSpec(byte1) orelse return error.illegal_instruction_error;
    const n_bytes = inst_spec.opcode_string.len / 8;
    const byte2 = if (n_bytes == 2) intel4004.pram.bytes[addr + 1] else null;
    var args_buf: [2]u16 = undefined;
    const args = inst_spec.extractArgs(byte1, byte2, &args_buf);

    var out = std.ArrayList(u8).init(allocator);
    try out.appendSlice(inst_spec.mnemonic);
    try out.appendSlice(" ");
    for (args) |arg| {
        try out.appendSlice(try std.fmt.allocPrint(allocator, "{d}", .{arg}));
        try out.appendSlice(" ");
    }
    return out.items[0 .. out.items.len - 1];
}
