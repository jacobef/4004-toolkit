const std = @import("std");
const instruction_spec = @import("instruction_spec.zig");
const cpu = @import("cpu.zig");
const InstructionSpec = instruction_spec.InstructionSpec;
const OneOrTwoBytes = instruction_spec.OneOrTwoBytes;
const CPUArgType = instruction_spec.CPUArgType;

fn doNothing() void {}

fn instructionFromMnemonic(mnemonic: []const u8) ?InstructionSpec {
    for (instruction_spec.instructions.all) |inst| {
        if (std.mem.eql(u8, inst.mnemonic, mnemonic)) {
            return inst;
        }
    }
    return null;
}

const Expression = struct {
    val: u64,
    type: CPUArgType
};

fn evalCharExpr(expr: []const u8) !u8 {
    if (expr.len < 3 or expr[0] != '\'' or expr[expr.len-1] != '\'') {
        return error.invalid_char_expr;
    }
    const inner_char = expr[1..expr.len-1];
    if (std.mem.eql(u8, inner_char, "\\a")) {
        return std.ascii.control_code.bel;
    } else if (std.mem.eql(u8, inner_char, "\\d")) {
        return std.ascii.control_code.del;
    } else if (std.mem.eql(u8, inner_char, "\\n")) {
        return '\n';
    } else if (std.mem.eql(u8, inner_char, "\\s")) {
        return ' ';
    } else if (std.mem.eql(u8, inner_char, "\\t")) {
        return '\t';
    } else if (std.mem.eql(u8, inner_char, "\\\\")) {
        return '\\';
    } else if (inner_char[0] == '\\' and inner_char.len > 1) {
        return error.unrecognized_escape_sequence;
    } else if (inner_char.len > 1) {
        return error.multiple_chars_in_char_literal;
    } else {
        return inner_char[0];
    }
}

fn evalExpr(arg: []const u8, pc: u12, labels: ?std.StringHashMap(Expression)) !Expression {
    if (arg.len == 0) {
        std.debug.panic("argument is length 0", .{});
    } else if (std.mem.eql(u8, arg, "*")) {
        return .{.val = pc, .type = .address};
    } else if (arg[0] == '\'') {
        return .{.val = try evalCharExpr(arg), .type = .number};
    } else if (arg[arg.len-1] == '?') {
        const cond = arg[0..arg.len-1];
        var val: u4 = undefined;
        if (std.mem.eql(u8, cond, "Z")) {
            val = 0b0100;
        } else if (std.mem.eql(u8, cond, "NZ")) {
            val = 0b1100;
        } else if (std.mem.eql(u8, cond, "C")) {
            val = 0b0010;
        } else if (std.mem.eql(u8, cond, "NC")) {
            val = 0b1010;
        } else {
            return error.invalid_condition;
        }
        return .{.val = val, .type = .condition};
    } else if (arg[0] < '0' or arg[0] > '9') {
        if (labels) |labels_| {
            return labels_.get(arg) orelse {
                std.debug.print("no such label: {s}\n", .{arg});
                return error.no_such_label;
            };
        } else {
            return error.labels_map_not_provided;
        }
    } else if (arg[arg.len-1] == 'R') {
        return .{.val = try std.fmt.parseInt(u4, arg[0..arg.len-1], 10), .type = .register};
    } else if (arg[arg.len-1] == 'P') {
        return .{.val = try std.fmt.parseInt(u3, arg[0..arg.len-1], 10), .type = .register_pair};
    } else if (arg[arg.len-1] == 'B') {
        return .{.val = try std.fmt.parseInt(u64, arg[0..arg.len-1], 2), .type = .number};
    } else {
        return .{.val = try std.fmt.parseInt(u64, arg, 10), .type = .number};
    }
}

fn getArgMask(arg_t: type, args: []const []const u8, spec: InstructionSpec, arg_i: u1, pc: u12, labels: std.StringHashMap(Expression)) !u16 {
    const arg_evald = try evalExpr(args[arg_i], pc,  labels);
    if (arg_evald.val > std.math.maxInt(arg_t) and !(arg_t == u8 and arg_evald.type == .address)) {
        return error.arg_too_big;
    }
    if (spec.arg_types[arg_i] == .address and arg_t == u8) {
        const pc_after_instr = pc + 2;
        const jump_to = arg_evald.val;
        if (pc_after_instr & 0xF00 != jump_to & 0xF00) {
            std.debug.print("8-bit jump across a page boundary; instruction is on address {} (will jump to page {}) and is trying to jump to address {} (page {})\n",
                .{pc, (pc_after_instr & 0xF00) >> 8, jump_to, (jump_to & 0xF00) >> 8});
            return error.eight_bit_jump_across_page_boundary;
        }
    }

    const arg_val: u16 = @as(arg_t, @truncate(arg_evald.val));
    if (arg_evald.type != spec.arg_types[arg_i]) {
        std.debug.print("error in {s} ", .{spec.mnemonic});
        for (args) |arg| {
            std.debug.print("{s} ", .{arg});
        }
        std.debug.print(": expected {s}, got {s} for arg {!}\n", .{@tagName(spec.arg_types[arg_i]), @tagName(arg_evald.type), arg_i});
        return error.type_mismatch;
    }
    return arg_val << spec.arg_extractors[arg_i].shift_amount;
}

fn assembleInstruction(spec: InstructionSpec, args: []const []const u8, pc: u12, labels: std.StringHashMap(Expression)) !OneOrTwoBytes {
    const assembled_as_u16 = switch (spec.func) {
        .f_no_args => spec.opcode_mask,
        .f_3 => spec.opcode_mask | try getArgMask(u3, args, spec, 0, pc, labels),
        .f_4 => spec.opcode_mask | try getArgMask(u4, args, spec, 0, pc, labels),
        .f_12 => spec.opcode_mask | try getArgMask(u12, args, spec, 0, pc, labels),
        .f_3_8 => spec.opcode_mask | try getArgMask(u3, args, spec, 0, pc, labels) | try getArgMask(u8, args, spec, 1, pc, labels),
        .f_4_8 => spec.opcode_mask | try getArgMask(u4, args, spec, 0, pc, labels) | try getArgMask(u8, args, spec, 1, pc, labels),
    };
    if (spec.opcode_string.len == 8) {
        return .{ .one = @intCast(assembled_as_u16 >> 8) };
    } else if (spec.opcode_string.len == 16) {
        return .{ .two = assembled_as_u16 };
    } else unreachable;
}

fn getLabels(lines: []ParsedLine) !std.StringHashMap(Expression) {
    var labels_to_values = std.StringHashMap(Expression).init(std.heap.page_allocator);
    var addr: u12 = 0;
    for (lines) |line| {
        switch (line) {
            .instruction => |inst_line| {
                if (inst_line.label) |label| {
                    try labels_to_values.put(label, .{.val = addr, .type = .address });
                }
                const inst = instructionFromMnemonic(inst_line.mnemonic) orelse return error.invalid_mnemonic;
                addr += @intCast(inst.opcode_string.len / 8);
            },
            .number => |num_line| {
                if (num_line.label) |label| {
                    try labels_to_values.put(label, .{.val = addr, .type = .address });
                }
                addr += 1;
            },
            .origin => |new_pc_expr| {
                const new_pc = (try evalExpr(new_pc_expr, addr, null)).val;
                if (new_pc > std.math.maxInt(u12)) {
                    return error.origin_number_too_big;
                }
                addr = @intCast(new_pc);
            },
            .equate => doNothing(),
            .macro => std.debug.panic("getLabels encountered a macro", .{})
        }
    }
    return labels_to_values;
}

fn getJCNMacroReplacement(condition_code: []const u8, arg: []const u8, allocator: std.mem.Allocator) ![]ParsedLine {
    var out_line = std.ArrayList(u8).init(allocator);
    try out_line.appendSlice("JCN ");
    try out_line.appendSlice(condition_code);
    try out_line.appendSlice(" ");
    try out_line.appendSlice(arg);
    var out = try allocator.alloc(ParsedLine, 1);
    out[0] = (try parseLine(out_line.items, allocator)).?;
    return out;
}

// UB if args doesn't contain "->"
// fn getArrowMacroReplacement(args: []const []const u8, allocator: std.mem.Allocator) ![]ParsedLine {
//     var arrow_index: usize = undefined;
//     for (args, 0..) |arg, i| {
//         if (std.mem.eql(u8, arg, "->")) {
//             arrow_index = i;
//             break;
//         }
//     }
//     const from_exprs = args[0..arrow_index];
//     const to_exprs = args[arrow_index+1..];
//
// }

fn getMacroReplacement(name: []const u8, args: []const []const u8, allocator: std.mem.Allocator) ![]ParsedLine {
    // The JCN macros aren't really useful except for testing this function
    if (std.mem.eql(u8, name, "JZ")) {
        return getJCNMacroReplacement("Z?", args[0], allocator);
    } else if (std.mem.eql(u8, name, "JNZ")) {
        return getJCNMacroReplacement( "NZ?", args[0], allocator);
    } else if (std.mem.eql(u8, name, "JC")) {
        return getJCNMacroReplacement("C?", args[0], allocator);
    } else if (std.mem.eql(u8, name, "JNC")) {
        return getJCNMacroReplacement("NC?", args[0], allocator);
    } else {
        return error.no_such_macro;
    }
}

fn replaceMacros(lines: []ParsedLine, allocator: std.mem.Allocator) ![]ParsedLine {
    var out = std.ArrayList(ParsedLine).init(allocator);
    for (lines) |line| {
        switch (line) {
            .macro => |macro_line| {
                const replacement_lines = try getMacroReplacement(macro_line.name, macro_line.args, allocator);
                if (replacement_lines.len > 0) {
                    if (macro_line.label) |label| {
                        switch (replacement_lines[0]) {
                            .instruction => |*inst| inst.label = label,
                            .macro => |*mac| mac.label = label,
                            .number => |*num| num.label = label,
                            .equate => |_| return error.label_on_equate_line,
                            .origin => |_| return error.label_on_origin_line
                        }
                    }
                }
                try out.appendSlice(try replaceMacros(replacement_lines, allocator));
            },
            else => try out.append(line)
        }
    }
    return out.items;
}

fn getWords(line: []const u8, allocator: std.mem.Allocator) ![]const []const u8 {
    var word_iterator = std.mem.tokenizeScalar(u8, line, ' ');
    var out = std.ArrayList([]const u8).init(allocator);
    while (word_iterator.next()) |word| {
        try out.append(word);
    }
    return out.items;
}

const ParsedLine = union(enum) {
    instruction: struct { label: ?[]const u8, mnemonic: []const u8, args: []const []const u8 },
    macro: struct { label: ?[]const u8, name: []const u8, args: []const []const u8 },
    number: struct { label: ?[]const u8, expr: []const u8 },
    equate: struct { name: []const u8, expr: []const u8 },
    origin: []const u8
};

fn parseLine(line: []const u8, allocator: std.mem.Allocator) !?ParsedLine {
    var comment_remover = std.mem.splitScalar(u8, line, '/');
    const words = try getWords(comment_remover.first(), allocator);
    if (words.len == 0) return null;

    const label = if (words[0][words[0].len - 1] == ',') words[0][0..words[0].len-1] else null;
    const words_without_label = if (label) |_| words[1..] else words;

    if (std.mem.eql(u8, words_without_label[0], "=")) {
        if (words_without_label.len != 2) {
            return error.origin_wrong_number_of_expressions;
        }
        if (label) |_| {
            return error.origin_with_label;
        }
        return .{ .origin = words[1] };
    } else if (words_without_label.len >= 2 and std.mem.eql(u8, words_without_label[1], "=")) {
        if (words_without_label.len != 3) {
            return error.equate_wrong_number_of_expressions;
        }
        if (label) |_| {
            return error.equate_with_address_label;
        }
        return .{ .equate = .{.name = words_without_label[0], .expr = words_without_label[2] } };
    }
    if (words_without_label[0][0] == '#') {
        return .{ .macro = .{ .label = label, .name = words_without_label[0][1..], .args = words_without_label[1..] } };
    }
    if (instructionFromMnemonic(words_without_label[0])) |_| {
        return .{ .instruction = .{ .label = label, .mnemonic = words_without_label[0], .args = words_without_label[1..] } };
    } else {
        return .{ .number = .{.label = label, .expr = words_without_label[0] }};
    }
}

fn add_byte_at_pc(bytes: *std.ArrayList(u8), byte: u8, pc: u12) !void {
    if (pc < bytes.items.len) {
        bytes.items[pc] = byte;
    } else {
        for (0..pc-bytes.items.len) |_| {
            try bytes.append(0);
        }
        try bytes.append(byte);
    }
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
    const parsed_lines_macros_replaced = try replaceMacros(parsed_lines_al.items, allocator);

    var labels = try getLabels(parsed_lines_macros_replaced);
    var it = labels.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s} -> {!}\n", .{entry.key_ptr.*, entry.value_ptr.*});
    }
    var program_counter: u12 = 0;
    for (parsed_lines_macros_replaced) |line| {
        switch (line) {
            .instruction => |inst_line| {
                const spec = instructionFromMnemonic(inst_line.mnemonic) orelse return error.no_such_instruction;
                const assembled_instruction = try assembleInstruction(spec, inst_line.args, program_counter, labels);
                switch (assembled_instruction) {
                    .one => |inst| {
                        try add_byte_at_pc(&assembled_bytes, inst, program_counter);
                        program_counter += 1;
                    },
                    .two => |inst| {
                        try add_byte_at_pc(&assembled_bytes, @intCast((inst >> 8) & 0xFF), program_counter);
                        program_counter += 1;
                        try add_byte_at_pc(&assembled_bytes, @intCast(inst & 0xFF), program_counter);
                        program_counter += 1;
                    },
                }
            },
            .number => |num_line| {
                const num_val = (try evalExpr(num_line.expr, program_counter, labels)).val;
                if (num_val > std.math.maxInt(u8)) {
                    return error.standalone_number_too_big;
                }
                try add_byte_at_pc(&assembled_bytes, @intCast(num_val), program_counter);
                program_counter += 1;
            },
            .equate => |equate_line| {
                try labels.put(equate_line.name, try evalExpr(equate_line.expr, program_counter, labels));
            },
            .origin => |new_pc_expr| {
                const new_pc = (try evalExpr(new_pc_expr, program_counter, labels)).val;
                if (new_pc > std.math.maxInt(u12)) {
                    return error.origin_number_too_big;
                }
                program_counter = @intCast(new_pc);
            },
            else => std.debug.panic("unsupported line type", .{})
        }
    }

    _ = try output_file.write(assembled_bytes.items);

    std.debug.print("{any}\n", .{assembled_bytes.items});
}
