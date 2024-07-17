const std = @import("std");
const instruction_spec = @import("instruction_spec.zig");
const cpu = @import("cpu.zig");
const InstructionSpec = instruction_spec.InstructionSpec;
const OneOrTwoBytes = instruction_spec.OneOrTwoBytes;
const CPUArgType = instruction_spec.CPUArgType;

fn instructionFromMnemonic(mnemonic: []const u8) ?InstructionSpec {
    for (instruction_spec.instructions.all) |inst| {
        if (std.mem.eql(u8, inst.mnemonic, mnemonic)) {
            return inst;
        }
    }
    return null;
}

fn evaluateArgument(T: type, arg: []const u8, labels: std.StringHashMap(u12)) !struct { val: T, type: CPUArgType }{
    if (arg.len == 0) {
        std.debug.panic("argument is length 0", .{});
    } else if (arg[arg.len-1] == 'R') {
        return .{.val = try std.fmt.parseInt(T, arg[0..arg.len-1], 10), .type = .register};
    } else if (arg[arg.len-1] == 'P') {
        return .{.val = try std.fmt.parseInt(T, arg[0..arg.len-1], 10), .type = .register_pair};
    } else if (arg[arg.len-1] == 'B') {
        return .{.val = try std.fmt.parseInt(T, arg[0..arg.len-1], 2), .type = .number};
    } else if (arg[arg.len-1] == 'H') {
        return .{.val = try std.fmt.parseInt(T, arg[0..arg.len-1], 16), .type = .number};
    } else if (arg.len == 3 and arg[0] == '\'' and arg[arg.len-1] == '\'') {
        return .{.val = @intCast(arg[1]), .type = .number};  // TODO remove UB
    } else if (arg[0] >= '0' and arg[0] <= '9') {
        return .{.val = try std.fmt.parseInt(T, arg, 10), .type = .number};
    } else {
        return .{.val = @intCast(labels.get(arg) orelse return error.no_such_label), .type = .address};
    }
}

fn getArgMask(arg_t: type, args: []const []const u8, spec: InstructionSpec, arg_i: u1, labels: std.StringHashMap(u12)) !u16 {
    const arg_evald = try evaluateArgument(arg_t, args[arg_i], labels);
    if (arg_evald.type != spec.arg_types[arg_i]) {
        std.debug.print("error in {s} ", .{spec.mnemonic});
        for (args) |arg| {
            std.debug.print("{s} ", .{arg});
        }
        std.debug.print(": expected {s}, got {s} for arg {d}\n", .{@tagName(spec.arg_types[arg_i]), @tagName(arg_evald.type), arg_i});
        return error.type_mismatch;
    }
    return @as(u16, @intCast(arg_evald.val)) << spec.arg_extractors[arg_i].shift_amount;
}

fn assembleInstruction(spec: InstructionSpec, args: []const []const u8, labels: std.StringHashMap(u12)) !OneOrTwoBytes {
    const assembled_as_u16 = switch (spec.func) {
        .f_no_args => spec.opcode_mask,
        .f_3 => spec.opcode_mask | try getArgMask(u3, args, spec, 0, labels),
        .f_4 => spec.opcode_mask | try getArgMask(u4, args, spec, 0, labels),
        .f_12 => spec.opcode_mask | try getArgMask(u12, args, spec, 0, labels),
        .f_3_8 => spec.opcode_mask | try getArgMask(u3, args, spec, 0, labels) | try getArgMask(u8, args, spec, 1, labels),
        .f_4_8 => spec.opcode_mask | try getArgMask(u4, args, spec, 0, labels) | try getArgMask(u8, args, spec, 1, labels),
    };
    if (spec.opcode_string.len == 8) {
        return .{ .one = @intCast(assembled_as_u16 >> 8) };
    } else if (spec.opcode_string.len == 16) {
        return .{ .two = assembled_as_u16 };
    } else unreachable;
}

fn getLabels(lines: []ParsedLine) !std.StringHashMap(u12) {
    var labels_to_values = std.StringHashMap(u12).init(std.heap.page_allocator);
    var addr: u12 = 0;
    for (lines) |line| {
        switch (line) {
            .instruction => |inst_line| {
                if (inst_line.label) |label| {
                    try labels_to_values.put(label, addr);
                }
                const inst = instructionFromMnemonic(inst_line.mnemonic) orelse return error.invalid_mnemonic;
                addr += @intCast(inst.opcode_string.len / 8);
            },
            .number => |num_line| {
                if (num_line.label) |label| {
                    try labels_to_values.put(label, addr);
                }
                addr += 1;
            },
            else => std.debug.panic("unimplemented asm line type", .{}),
        }
    }
    return labels_to_values;
}

const ParsedLine = union(enum) {
    instruction: struct { label: ?[]const u8, mnemonic: []const u8, args: []const []const u8 },
    number: struct { label: ?[]const u8, expr: []const u8 },
    equate: struct { label: ?[]const u8, expr: []const u8 },
    origin: []const u8 };

fn parseLine(line: []const u8, allocator: std.mem.Allocator) !?ParsedLine {
    var comment_remover = std.mem.splitScalar(u8, line, '/');
    var word_iterator = std.mem.tokenizeScalar(u8, comment_remover.first(), ' ');
    const first_word = word_iterator.peek() orelse return null;
    const label = if (first_word[first_word.len - 1] == ',') first_word[0 .. first_word.len - 1] else null;
    if (label) |_| {
        _ = word_iterator.next();
    }
    const mnemonic = word_iterator.next() orelse return error.label_without_mnemonic;

    var buf = try allocator.alloc([]const u8, 2);
    const arg1 = word_iterator.next() orelse return .{ .instruction = .{ .label = label, .mnemonic = mnemonic, .args = buf[0..0] } };
    buf[0] = arg1;
    const arg2 = word_iterator.next() orelse return .{ .instruction = .{ .label = label, .mnemonic = mnemonic, .args = buf[0..1] } };
    buf[1] = arg2;
    return .{ .instruction = .{ .label = label, .mnemonic = mnemonic, .args = buf[0..2] } };
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

    var parsed_lines_al = std.ArrayList(ParsedLine).init(allocator);
    while (line_iterator.next()) |line| {
        try parsed_lines_al.append((try parseLine(line, allocator)) orelse continue);
    }
    const parsed_lines = parsed_lines_al.items;

    const labels = try getLabels(parsed_lines);
    var it = labels.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s} -> {d}\n", .{entry.key_ptr.*, entry.value_ptr.*});
    }

    for (parsed_lines) |line| {
        switch (line) {
            .instruction => |inst_line| {
                const spec = instructionFromMnemonic(inst_line.mnemonic) orelse return error.no_such_instruction;
                const assembled_instruction = try assembleInstruction(spec, inst_line.args, labels);
                switch (assembled_instruction) {
                    .one => |inst| try assembled_bytes.append(inst),
                    .two => |inst| {
                        try assembled_bytes.append(@intCast((inst >> 8) & 0xFF));
                        try assembled_bytes.append(@intCast(inst & 0xFF));
                    },
                }
            },
            else => std.debug.panic("unsupported line type", .{})
        }
    }

    const n_bytes_written = try output_file.write(assembled_bytes.items);
    std.debug.print("{d} bytes written\n", .{n_bytes_written});

    std.debug.print("{any}\n", .{assembled_bytes.items});
}
