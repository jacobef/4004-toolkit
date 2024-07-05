const std = @import("std");
const instruction_spec = @import("instruction_spec.zig");
const InstructionSpec = instruction_spec.InstructionSpec;
const OneOrTwoBytes = instruction_spec.OneOrTwoBytes;

fn instructionFromMnemonic(mnemonic: []const u8) ?InstructionSpec {
    for (instruction_spec.instructions.all) |inst| {
        if (std.mem.eql(u8, inst.mnemonic, mnemonic)) {
            return inst;
        }
    }
    return null;
}

fn evaluateArgument(T: type, arg: []const u8) !T {
    return try std.fmt.parseInt(T, arg, 0);
}

fn getArgMask(arg_t: type, arg: []const u8, spec: InstructionSpec, arg_i: u1) !u16 {
    const arg_val = try evaluateArgument(arg_t, arg);
    return @as(u16, @intCast(arg_val)) << spec.arg_extractors[arg_i].shift_amount;
}

fn assembleInstruction(spec: InstructionSpec, arg1: ?[]const u8, arg2: ?[]const u8) !OneOrTwoBytes {
    const assembled_as_u16 = switch (spec.func) {
        .f_no_args => spec.opcode_mask,
        .f_3 => spec.opcode_mask | try getArgMask(u3, arg1.?, spec, 0),
        .f_4 => spec.opcode_mask | try getArgMask(u4, arg1.?, spec, 0),
        .f_12 => spec.opcode_mask | try getArgMask(u12, arg1.?, spec, 0),
        .f_3_8 => spec.opcode_mask | try getArgMask(u3, arg1.?, spec, 0) | try getArgMask(u8, arg2.?, spec, 1),
        .f_4_8 => spec.opcode_mask | try getArgMask(u4, arg1.?, spec, 0) | try getArgMask(u8, arg2.?, spec, 1),
    };
    if (spec.opcode_string.len == 8) {
        return .{ .one = @intCast(assembled_as_u16 >> 8) };
    } else if (spec.opcode_string.len == 16) {
        return .{ .two = assembled_as_u16 };
    } else unreachable;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var assembled_bytes = std.ArrayList(u8).init(allocator);

    var cmd_args = try std.process.argsWithAllocator(allocator);
    _ = cmd_args.next();
    const input_file_path = cmd_args.next() orelse return error.NoInputFileGiven;
    const output_file_path = cmd_args.next() orelse return error.NoOutputFileGiven;
    const input_file = try std.fs.cwd().openFile(input_file_path, .{ .mode = .read_only });
    const output_file = try std.fs.cwd().createFile(output_file_path, .{});
    const input_chars = try input_file.readToEndAlloc(allocator, 4096 * 2 * 100);
    var line_iterator = std.mem.tokenizeScalar(u8, input_chars, '\n');

    while (line_iterator.next()) |line| {
        var word_iterator = blk: {
            const line_trimmed = std.mem.trim(u8, line, " ");
            var comment_remover = std.mem.splitScalar(u8, line_trimmed, '/');
            const line_without_comment = comment_remover.first();
            break :blk std.mem.tokenizeScalar(u8, line_without_comment, ' ');
        };

        const first_word = word_iterator.next() orelse continue;
        const instruction = instructionFromMnemonic(first_word) orelse return error.no_such_mnemonic;
        const arg1 = word_iterator.next();
        const arg2 = word_iterator.next();

        const assembled_instruction = try assembleInstruction(instruction, arg1, arg2);
        switch (assembled_instruction) {
            .one => |inst| try assembled_bytes.append(inst),
            .two => |inst| {
                try assembled_bytes.append(@intCast((inst >> 8) & 0xFF));
                try assembled_bytes.append(@intCast(inst & 0xFF));
            },
        }
    }

    const n_bytes_written = try output_file.write(assembled_bytes.items);
    std.debug.print("{d} bytes written\n", .{n_bytes_written});

    std.debug.print("{any}\n", .{assembled_bytes.items});
}
