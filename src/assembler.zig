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
    } else if (std.mem.eql(u8, inner_char, "\\t")) {
        return '\t';
    } else if (std.mem.eql(u8, inner_char, "\\\\")) {
        return '\\';
    } else if (std.mem.eql(u8, inner_char, "\\'")) {
        return '\'';
    } else if (inner_char[0] == '\\' and inner_char.len > 1) {
        return error.unrecognized_escape_sequence;
    } else if (inner_char.len > 1) {
        return error.multiple_chars_in_char_literal;
    } else {
        return inner_char[0];
    }
}

const ExprToken = union(enum) {
    subexpr: []const u8,
    operator: enum { plus, minus, at },
};

fn tokenizeExpr(expr: []const u8, allocator: std.mem.Allocator) ![]ExprToken {
    var tokens = std.ArrayList(ExprToken).init(allocator);
    var i: usize = 0;

    while (i < expr.len) {
        switch (expr[i]) {
            '+' => {
                try tokens.append(.{ .operator = .plus });
                i += 1;
            },
            '-' => {
                try tokens.append(.{ .operator = .minus });
                i += 1;
            },
            '@' => {
                try tokens.append(.{ .operator = .at });
                i += 1;
            },
            else => {
                const start = i;
                if (expr[i] == '\'') {
                    i += 1; // Skip the open quote
                    while (i < expr.len) {
                        if (expr[i] == '\\') {
                            i += 2;
                        } else if (expr[i] == '\'') {
                            break;
                        } else {
                            i += 1;
                        }
                    }
                    if (i >= expr.len) return error.unterminated_char_literal;
                    i += 1; // Include the closing quote
                } else {
                    while (i < expr.len and !std.mem.containsAtLeast(u8, "+-@", 1, expr[i..i+1])) : (i += 1) {}
                }
                try tokens.append(.{ .subexpr = expr[start..i] });
            },
        }
    }

    return try tokens.toOwnedSlice();
}

fn getSimpleExprType(expr: []const u8, label_types: std.StringHashMap(CPUArgType)) !CPUArgType {
    if (std.mem.eql(u8, expr, "*")) {
        return .address;
    } else if (expr[0] == '\'') {
        return .number;
    } else if (expr[expr.len-1] == '?') {
        return .condition;
    } else if (expr[0] < '0' or expr[0] > '9') {
        return label_types.get(expr) orelse {
            std.debug.print("no such label: {s}\n", .{expr});
            return error.no_such_label;
        };
    } else if (expr[expr.len-1] == 'R') {
        return .register;
    } else if (expr[expr.len-1] == 'P') {
        return .register_pair;
    } else if (expr[expr.len-1] == 'B') {
        return .number;
    } else {
        return .number;
    }
}

fn evalSimpleExpr(expr: []const u8, pc: u12, labels: ?std.StringHashMap(Expression)) !Expression {
    if (std.mem.eql(u8, expr, "*")) {
        return .{.val = pc, .type = .address};
    } else if (expr[0] == '\'') {
        return .{.val = try evalCharExpr(expr), .type = .number};
    } else if (expr[expr.len-1] == '?') {
        const cond = expr[0..expr.len-1];
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
    } else if (expr[0] < '0' or expr[0] > '9') {
        if (labels) |labels_| {
            return labels_.get(expr) orelse {
                std.debug.print("no such label: {s}\n", .{expr});
                return error.no_such_label;
            };
        } else {
            return error.labels_map_not_provided;
        }
    } else if (expr[expr.len-1] == 'R') {
        return .{.val = try std.fmt.parseInt(u4, expr[0..expr.len-1], 10), .type = .register};
    } else if (expr[expr.len-1] == 'P') {
        return .{.val = try std.fmt.parseInt(u3, expr[0..expr.len-1], 10), .type = .register_pair};
    } else if (expr[expr.len-1] == 'B') {
        return .{.val = try std.fmt.parseInt(u64, expr[0..expr.len-1], 2), .type = .number};
    } else {
        return .{.val = try std.fmt.parseInt(u64, expr, 10), .type = .number};
    }
}

fn getExprType(expr: []const u8, label_types: std.StringHashMap(CPUArgType), allocator: std.mem.Allocator) !CPUArgType {
    const tokens = try tokenizeExpr(expr, allocator);
    switch (tokens.len) {
        1 => return getSimpleExprType(expr,  label_types),
        3 => switch (tokens[1].operator) {
            .plus => {
                const term1_type = try getExprType(tokens[0].subexpr, label_types, allocator);
                return term1_type;
            },
            .minus => {
                const term1_type = try getExprType(tokens[0].subexpr, label_types, allocator);
                return term1_type;
            },
            .at => {
                return .number;
            }
        },
        else => {
            return error.wrong_number_of_sub_expressions;
        }
    }
}

fn evalExpr(expr: []const u8, pc: u12, label_values: ?std.StringHashMap(Expression), allocator: std.mem.Allocator) !Expression {
    const tokens = try tokenizeExpr(expr, allocator);
    switch (tokens.len) {
        1 => return evalSimpleExpr(expr, pc, label_values),
        3 => switch (tokens[1].operator) {
            .plus => {
                const term1 = try evalExpr(tokens[0].subexpr, pc, label_values, allocator);
                const term2 = try evalExpr(tokens[2].subexpr, pc, label_values, allocator);
                return .{.val = term1.val+term2.val, .type = term1.type};
            },
            .minus => {
                const term1 = try evalExpr(tokens[0].subexpr, pc, label_values, allocator);
                const term2 = try evalExpr(tokens[2].subexpr, pc, label_values, allocator);
                return .{.val = term1.val-term2.val, .type = term1.type};
            },
            .at => {
                const num = try evalExpr(tokens[0].subexpr, pc, label_values, allocator);
                if (num.type != .number) {
                    return error.nibble_from_non_number;
                }
                const nibble_n = try std.fmt.parseInt(u3, tokens[2].subexpr, 10);
                const shift_amount = @as(u5, nibble_n) * 4;
                return .{.val = (num.val >> shift_amount) & @as(u64, 0xF), .type = .number};
            }
        },
        else => {
            return error.wrong_number_of_sub_expressions;
        }
    }
}

fn getArgMask(arg_t: type, args: []const Expression, spec: InstructionSpec, arg_i: u1, pc: u12) !u16 {
    const arg = args[arg_i];
    const expected_type = spec.arg_types[arg_i];
    if (arg.val > std.math.maxInt(arg_t) and !(arg_t == u8 and arg.type == .address)) {
        return error.arg_too_big;
    }
    if (expected_type == .address and arg_t == u8) {
        const pc_after_instr = pc + 2;
        const jump_to = arg.val;
        if (pc_after_instr & 0xF00 != jump_to & 0xF00) {
            std.debug.print("{s} {any}\n", .{spec.mnemonic, args});
            std.debug.print("8-bit jump across a page boundary; instruction is on address {} (will jump to page {}) and is trying to jump to address {} (page {})\n",
                .{pc, (pc_after_instr & 0xF00) >> 8, jump_to, (jump_to & 0xF00) >> 8});
            return error.eight_bit_jump_across_page_boundary;
        }
    }

    const arg_val: u16 = @as(arg_t, @truncate(arg.val));
    if (arg.type != expected_type) {
        std.debug.print("error in {s} ", .{spec.mnemonic});
        for (args) |a| {
            std.debug.print("{d} ", .{a.val});
        }
        std.debug.print(": expected {s}, got {s} for arg {!}\n", .{@tagName(expected_type), @tagName(arg.type), arg_i});
        return error.type_mismatch;
    }
    return arg_val << spec.arg_extractors[arg_i].shift_amount;
}

fn assembleInstruction(spec: InstructionSpec, args: []const Expression, pc: u12) !OneOrTwoBytes {
    // TODO cleanup once InstructionSpec.arg_sizes is implemented
    const assembled_as_u16 = switch (spec.func) {
        .f_no_args => spec.opcode_mask,
        .f_3 => spec.opcode_mask | try getArgMask(u3, args, spec, 0, pc),
        .f_4 => spec.opcode_mask | try getArgMask(u4, args, spec, 0, pc),
        .f_12 => spec.opcode_mask | try getArgMask(u12, args, spec, 0, pc),
        .f_3_8 => spec.opcode_mask | try getArgMask(u3, args, spec, 0, pc) | try getArgMask(u8, args, spec, 1, pc),
        .f_4_8 => spec.opcode_mask | try getArgMask(u4, args, spec, 0, pc) | try getArgMask(u8, args, spec, 1, pc),
    };
    if (spec.opcode_string.len == 8) {
        return .{ .one = @intCast(assembled_as_u16 >> 8) };
    } else if (spec.opcode_string.len == 16) {
        return .{ .two = assembled_as_u16 };
    } else unreachable;
}

fn getLabelTypes(lines: []ParsedLine, allocator: std.mem.Allocator) !std.StringHashMap(CPUArgType) {
    var labels_to_types = std.StringHashMap(CPUArgType).init(allocator);
    for (lines) |line| {
        switch (line) {
            .label => |label| try labels_to_types.put(label, .address),
            else => doNothing()
        }
    }

    for (lines) |line| {
        switch (line) {
            .equate => |eq_line| {
                try labels_to_types.put(eq_line.name, try getExprType(eq_line.expr, labels_to_types, allocator));
            },
            else => doNothing()
        }
    }

    return labels_to_types;
}

fn getLabels(lines: []TypedLine, allocator: std.mem.Allocator) !std.StringHashMap(Expression) {
    var label_values = std.StringHashMap(Expression).init(allocator);
    var addr: u12 = 0;
    for (lines) |line| {
        switch (line) {
            .label => |label| {
                if (label_values.contains(label)) {
                    return error.label_redefinition;
                } else {
                    try label_values.put(label, .{.val = addr, .type = .address });
                    // std.debug.print("{s} = {}\n", .{label, addr});
                }
            },
            else => doNothing()
        }
        addr = try pcAfterLine(line, addr, allocator);
    }
    addr = 0;
    for (lines) |line| {
        switch (line) {
            .equate => |eq_line| {
                if (label_values.contains(eq_line.name)) {
                    return error.label_redefinition;
                } else {
                    try label_values.put(eq_line.name, try evalExpr(eq_line.expr.str, addr, label_values, allocator));
                }
            },
            else => doNothing()
        }
        addr = try pcAfterLine(line, addr, allocator);
    }
    return label_values;
}

fn numRegsInDest(line: TypedSourceDestLine) !u8 {
    var out: u8 = 0;
    for (line.dest) |expr| {
        if (expr) |expr_| {
            switch (expr_.type) {
                .register => out += 1,
                .register_pair => out += 2,
                else => return error.not_reg_or_reg_pair
            }
        }
    }
    return out;
}

fn getArrowLineReplacement(line: EvaldSourceDestLine, allocator: std.mem.Allocator) ![]ExprsEvaldLine {
    // TODO add solver for cases like 0R 1R 2R -> 1R 2R 3R, and detect invalid cases like 0R 1R -> 1R 0R
    var out = std.ArrayList(ExprsEvaldLine).init(allocator);
    var i: u4 = 0; // LHS number is 64 bits max, so 16 RHS registers max
    while (i < line.dest.len) : (i += 1) {
        if (line.dest[i]) |dest_reg| {
            switch (line.source) {
                .num => |source_num| {
                    const bit_offset: u6 = @intCast((line.dest.len - 1 - i) * 4); // TODO make less perilous (extract into function probably)
                    const nibble = (source_num >> bit_offset) & @as(u64, 0xF);
                    try out.append(.{.instruction = .{
                        .mnemonic = "LDM",
                        .args = try allocator.dupe(Expression, &.{.{.type = .number, .val = nibble}})
                    }});
                },
                .regs => |source_regs| {
                    try out.append(.{.instruction =.{
                        .mnemonic = "LD",
                        .args = try allocator.dupe(Expression, &.{.{.type = .register, .val = source_regs[i]}})}});
                }
            }
            try out.append(.{.instruction = .{
                .mnemonic = "XCH",
                .args = try allocator.dupe(Expression, &.{.{.type = .register, .val = dest_reg}})
            }});
        }
    }

    return try out.toOwnedSlice();
}

fn getAddAssignLineReplacement(line: EvaldSourceDestLine, allocator: std.mem.Allocator) ![]ExprsEvaldLine {
    var out = std.ArrayList(ExprsEvaldLine).init(allocator);
    try out.append(.{ .instruction = .{ .mnemonic = "CLC", .args = try allocator.dupe(Expression, &.{}) } });

    for (0..line.dest.len) |idx| {
        const i = line.dest.len - idx - 1;
        switch (line.source) {
            .num => |increment_value| {
                const shift: u6 = @intCast(idx*4);
                const nibble = (increment_value >> shift) & 0xF;

                try out.append(.{
                    .instruction = .{
                        .mnemonic = "LDM",
                        .args = try allocator.dupe(Expression, &.{.{.val = nibble, .type = .number }})
                    }
                });
            },
            .regs => |increment_by_regs| {
                try out.append(.{
                    .instruction = .{
                        .mnemonic = "LD",
                        .args = try allocator.dupe(Expression, &.{ .{ .val = increment_by_regs[i], .type = .register } })
                    }
                });
            }
        }
        if (line.dest[i]) |dest_reg| {
            try out.append(.{
                .instruction = .{
                    .mnemonic = "ADD",
                    .args = try allocator.dupe(Expression, &.{ .{ .val = dest_reg, .type = .register } })
                }
            });
            try out.append(.{
                .instruction = .{
                    .mnemonic = "XCH",
                    .args = try allocator.dupe(Expression, &.{ .{ .val = dest_reg, .type = .register } })
                }
            });
        } else {
            std.debug.print("Error: Placeholders ('_') are not allowed in an add-assign line.\n", .{});
            return error.dest_register_cannot_be_null;
        }
    }

    return try out.toOwnedSlice();
}

fn getSubAssignLineReplacement(line: EvaldSourceDestLine, allocator: std.mem.Allocator) ![]ExprsEvaldLine {
    var out = std.ArrayList(ExprsEvaldLine).init(allocator);
    // Append 'CLC' instruction at the start
    try out.append(.{ .instruction = .{ .mnemonic = "CLC", .args = try allocator.dupe(Expression, &.{}) } });

    for (0..line.dest.len) |idx| {
        const i = line.dest.len - idx - 1;
        switch (line.source) {
            .num => |decrement_value| {
                const shift: u6 = @intCast(idx*4);
                const nibble = (decrement_value >> shift) & 0xF;

                if (line.dest[i]) |dest_reg| {
                    // Append LDM nibble
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "LDM",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .number, .val = nibble } })
                        }
                    });
                    // Append XCH dest_reg
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "XCH",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .register, .val = dest_reg } })
                        }
                    });
                    // Append SUB dest_reg
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "SUB",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .register, .val = dest_reg } })
                        }
                    });
                    // Append XCH dest_reg
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "XCH",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .register, .val = dest_reg } })
                        }
                    });
                    // Append CMC
                    try out.append(.{ .instruction = .{ .mnemonic = "CMC", .args = try allocator.dupe(Expression, &.{}) } });
                } else {
                    std.debug.print("Error: Placeholders ('_') are not allowed in a sub-assign line.\n", .{});
                    return error.dest_register_cannot_be_null;
                }
            },
            .regs => |decrease_by_regs| {
                if (line.dest[i]) |dest_reg| {
                    const source_reg = decrease_by_regs[i];
                    // Append LD dest_reg
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "LD",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .register, .val = dest_reg } })
                        }
                    });
                    // Append SUB source_reg
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "SUB",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .register, .val = source_reg } })
                        }
                    });
                    // Append XCH dest_reg
                    try out.append(.{
                        .instruction = .{
                            .mnemonic = "XCH",
                            .args = try allocator.dupe(Expression, &.{ .{ .type = .register, .val = dest_reg } })
                        }
                    });
                    // Append CMC
                    try out.append(.{ .instruction = .{ .mnemonic = "CMC", .args = try allocator.dupe(Expression, &.{}) } });
                } else {
                    std.debug.print("Error: Placeholders ('_') are not allowed in a sub-assign line.\n", .{});
                    return error.dest_register_cannot_be_null;
                }
            }
        }
    }

    // Remove the last 'CMC' instruction
    _ = out.pop();
    return try out.toOwnedSlice();
}



fn pcAfterLine(line: TypedLine, pc: u12, allocator: std.mem.Allocator) !u12 {
    switch (line) {
        .instruction => |inst_line| {
            const inst = instructionFromMnemonic(inst_line.mnemonic) orelse return error.invalid_mnemonic;
            return pc + @as(u12, @intCast(inst.opcode_string.len / 8));
        },
        .expr => return pc + 1,
        .origin => |new_pc_expr| {
            const new_pc = evalExpr(new_pc_expr.str, pc, null, allocator) catch |err| switch (err) {
                error.labels_map_not_provided => {
                    std.debug.print("origin expression can't contain a label\n", .{});
                    return error.origin_expression_label;
                },
                else => return err
            };
            if (new_pc.val > std.math.maxInt(u12)) {
                return error.origin_number_too_big;
            }
            return @intCast(new_pc.val);
        },
        .arrow => |sd_line| {
            // LDM, XCH for each register
            return pc + (try numRegsInDest(sd_line)) * 2;
        },
        .add_assign => |sd_line| {
            // CLC at the start, then LDM/LD, ADD, XCH for each register
            return pc + (try numRegsInDest(sd_line))*3 + 1;
        },
        .sub_assign => |sd_line| {
            return switch (sd_line.source) {
                //  CLC at the start, then LDM, XCH, SUB, XCH, CMC for each register, except no CMC at the end
                .num => pc + (try numRegsInDest(sd_line))*5 + 1 - 1,
                // CLC at the start, then LD, SUB, XCH, CMC for each register, except no CMC at the end
                .regs => pc + (try numRegsInDest(sd_line))*4 + 1 - 1,
            };
        },
        .equate => return pc,
        .label => return pc
    }
}

fn replaceSourceDestLines(lines: []ExprsEvaldLine, allocator: std.mem.Allocator) ![]ExprsEvaldLine {
    var out = std.ArrayList(ExprsEvaldLine).init(allocator);

    for (lines) |line| {
        switch (line) {
            .arrow => |arrow_line| {
                try out.appendSlice(try getArrowLineReplacement(arrow_line, allocator));
            },
            .add_assign => |add_assign_line| {
                try out.appendSlice(try getAddAssignLineReplacement(add_assign_line, allocator));
            },
            .sub_assign => |sub_assign_line| {
                try out.appendSlice(try getSubAssignLineReplacement(sub_assign_line, allocator));
            },
            else => try out.append(line)
        }
    }
    return try out.toOwnedSlice();
}

fn getMacroReplacement(name: []const u8, args: []const []const u8, allocator: std.mem.Allocator) ![]ParsedLine {
    if (std.mem.eql(u8, name, "CALL")) {
        if (args.len != 1) return error.not_1_call_argument;
        const out = try std.fmt.allocPrint(allocator,
            \\*+14 -> 8R _ _
            \\JMS PUSH_4_FROM_8R
            \\*+10 -> _ 8R _
            \\JMS PUSH_4_FROM_8R
            \\*+6 -> _ _ 8R
            \\JMS PUSH_4_FROM_8R
            \\JUN {s}
            , .{args[0]}
        );
        return try parseLines(out, allocator);
    } else if (std.mem.eql(u8, name, "LJCN")) {
        // TODO make less horrible
        if (args.len != 2) return error.not_2_ljcn_arguments;
        var inverted_cond: []const u8 = undefined;
        if (std.mem.eql(u8, args[0], "C?")) {
            inverted_cond = "NC?";
        } else if (std.mem.eql(u8, args[0], "NC?")) {
            inverted_cond = "C?";
        } else if (std.mem.eql(u8, args[0], "Z?")) {
            inverted_cond = "NZ?";
        } else if (std.mem.eql(u8, args[0], "NZ?")) {
            inverted_cond = "Z?";
        } else {
            return error.invalid_condition;
        }
        const out = try std.fmt.allocPrint(allocator,
            \\JCN {s} *+4
            \\JUN {s}
        , .{inverted_cond, args[1]});
        return try parseLines(out, allocator);
    } else {
        std.debug.print("no such macro: {s}\n", .{name});
        return error.no_such_macro;
    }
}

fn replaceMacros(lines: []ParsedLine, allocator: std.mem.Allocator) ![]ParsedLine {
    var out = std.ArrayList(ParsedLine).init(allocator);
    for (lines) |line| {
        switch (line) {
            .macro => |macro_line| {
                const replacement_lines = try getMacroReplacement(macro_line.name, macro_line.args, allocator);
                try out.appendSlice(try replaceMacros(replacement_lines, allocator));
            },
            else => try out.append(line)
        }
    }
    return try out.toOwnedSlice();
}

fn getWords(line: []const u8, allocator: std.mem.Allocator) ![]const []const u8 {
    var out = std.ArrayList([]const u8).init(allocator);
    var in_quote = false;
    var after_backslash = false;
    var word_start: usize = 0;

    var i: usize = 0;
    while (i < line.len) {
        const char = line[i];

        if (after_backslash) {
            after_backslash = false;
        } else if (char == '\\') {
            after_backslash = true;
        } else if (char == '\'' and !after_backslash) {
            in_quote = !in_quote;
        } else if (char == '/' and !in_quote) {
            // Start of a comment; ignore the rest of the line
            break;
        } else if (std.ascii.isWhitespace(char) and !in_quote) {
            if (i > word_start) {
                try out.append(line[word_start..i]);
            }
            word_start = i + 1; // Move past the whitespace
        }

        i += 1;
    }
    // Append the last word if any
    if (i > word_start) {
        try out.append(line[word_start..i]);
    }

    return try out.toOwnedSlice();
}

const TypedExpr = struct { str: []const u8, type: CPUArgType };

const UnevaldSourceDestLine = struct { source: []const []const u8, dest: []const []const u8 };
const TypedSourceDestLine = struct {
    source: union(enum) {
        num: TypedExpr,
        regs: []TypedExpr,
    },
    dest: []?TypedExpr,
};

const EvaldSourceDestLine = struct {
    source: union(enum) {
        num: u64,
        regs: []const u4,
    },
    dest: []const ?u4,
};

const ParsedLine = union(enum) {
    instruction: struct { mnemonic: []const u8, args: []const []const u8 },
    macro: struct { name: []const u8, args: []const []const u8 },
    arrow: UnevaldSourceDestLine,
    add_assign: UnevaldSourceDestLine,
    sub_assign: UnevaldSourceDestLine,
    expr: []const u8,
    equate: struct { name: []const u8, expr: []const u8 },
    origin: []const u8,
    label: []const u8
};

const TypedLine = union(enum) {
    instruction: struct { mnemonic: []const u8, args: []const TypedExpr },
    arrow: TypedSourceDestLine,
    add_assign: TypedSourceDestLine,
    sub_assign: TypedSourceDestLine,
    expr: TypedExpr,
    equate: struct { name: []const u8, expr: TypedExpr },
    origin: TypedExpr,
    label: []const u8
};

const ExprsEvaldLine = union(enum) {
    instruction: struct { mnemonic: []const u8, args: []const Expression },
    arrow: EvaldSourceDestLine,
    add_assign: EvaldSourceDestLine,
    sub_assign: EvaldSourceDestLine,
    expr: Expression,
    origin: u12
};

fn parseLine(line: []const u8, allocator: std.mem.Allocator) ![]ParsedLine {
    var out = std.ArrayList(ParsedLine).init(allocator);

    const words = try getWords(line, allocator);

    const label = if (words.len > 0 and words[0][words[0].len - 1] == ',') words[0][0..words[0].len-1] else null;
    const words_without_label = if (label) |_| words[1..] else words;
    if (label) |label_| try out.append(.{.label = label_});
    if (words_without_label.len == 0) {
        return try out.toOwnedSlice();
    }

    for (words_without_label, 0..) |word, i| {
        if (std.mem.eql(u8, word, "->")) {
            try out.append(.{.arrow = .{ .source = words_without_label[0..i], .dest = words_without_label[i+1..] }});
            return try out.toOwnedSlice();
        } else if (std.mem.eql(u8, word, "+=")) {
            try out.append(.{.add_assign = .{ .dest = words_without_label[0..i], .source = words_without_label[i+1..] }});
            return try out.toOwnedSlice();
        } else if (std.mem.eql(u8, word, "-=")) {
            try out.append(.{.sub_assign = .{ .dest = words_without_label[0..i], .source = words_without_label[i+1..] }});
            return try out.toOwnedSlice();
        }
    }

    if (std.mem.eql(u8, words_without_label[0], "=")) {
        if (words_without_label.len != 2) {
            return error.origin_wrong_number_of_expressions;
        }
        try out.append(.{ .origin = words_without_label[1] });
    } else if (words_without_label.len >= 2 and std.mem.eql(u8, words_without_label[1], "=")) {
        if (words_without_label.len != 3) {
            return error.equate_wrong_number_of_expressions;
        }
        try out.append(.{ .equate = .{ .name = words_without_label[0], .expr = words_without_label[2] } });
    } else if (words_without_label[0][0] == '#') {
        try out.append(.{ .macro = .{ .name = words_without_label[0][1..], .args = words_without_label[1..] } });
    } else if (instructionFromMnemonic(words_without_label[0])) |_| {
        try out.append(.{ .instruction = .{ .mnemonic = words_without_label[0], .args = words_without_label[1..] } });
    } else {  // TODO detect e.g. CALL ABC as an instruction (with an invalid mnemonic)
        try out.append(.{ .expr = words_without_label[0] });
    }
    return try out.toOwnedSlice();
}

fn parseLines(input_chars: []const u8, allocator: std.mem.Allocator) ![]ParsedLine {
    var line_iterator = std.mem.tokenizeAny(u8, input_chars, "\n");
    var out = std.ArrayList(ParsedLine).init(allocator);
    while (line_iterator.next()) |line| {
        try out.appendSlice(try parseLine(line, allocator));
    }
    return try out.toOwnedSlice();
}

fn getTypedSourceDest(line: UnevaldSourceDestLine, label_types: std.StringHashMap(CPUArgType), allocator: std.mem.Allocator) !TypedSourceDestLine {
    var source_exprs = std.ArrayList(TypedExpr).init(allocator);
    var dest_exprs = std.ArrayList(?TypedExpr).init(allocator);
    for (line.source) |expr_str| {
        const expr_type = try getExprType(expr_str, label_types, allocator);
        switch (expr_type) {
            .register, .register_pair, .number, .address => try source_exprs.append(.{.str = expr_str, .type = expr_type}),
            else => {
                std.debug.print("invalid type for source in source/dest expression: {!}\n", .{expr_type});
                return error.invalid_source_type;
            }
        }
    }
    for (line.dest) |expr_str| {
        if (std.mem.eql(u8, expr_str, "_")) {
            try dest_exprs.append(null);
        } else {
            const expr_type = try getExprType(expr_str, label_types, allocator);
            switch (expr_type) {
                .register, .register_pair => try dest_exprs.append(.{.str = expr_str, .type = expr_type}),
                else => {
                    std.debug.print("invalid type for dest in source/dest expression: {!}\n", .{expr_type});
                    return error.invalid_dest_type;
                }
            }
        }
    }

    if (source_exprs.items.len == 0 or dest_exprs.items.len == 0) {
        return error.empty_source_or_dest;
    }

    if ((source_exprs.items[0].type == .number or source_exprs.items[0].type == .address) and source_exprs.items.len == 1) {
        return .{.source = .{.num = source_exprs.items[0]}, .dest = dest_exprs.items};
    } else {
        for (source_exprs.items) |expr| {
            if (expr.type != .register and expr.type != .register_pair) {
                std.debug.print("the source expression of a source/dest line must be all registers and/or register pairs, or one number or address\n", .{});
                return error.invalid_source;
            }
        }
        return .{.source = .{.regs = source_exprs.items}, .dest = dest_exprs.items};
    }
}

fn asTypedExpr(expr: []const u8, label_types: std.StringHashMap(CPUArgType), allocator: std.mem.Allocator) !TypedExpr {
    return .{.str = expr, .type = try getExprType(expr, label_types, allocator)};
}

fn getTypedLine(line: ParsedLine, label_types: std.StringHashMap(CPUArgType), allocator: std.mem.Allocator) !TypedLine {
    switch (line) {
        .instruction => |inst_line| {
            var typed_args = std.ArrayList(TypedExpr).init(allocator);
            const spec = instructionFromMnemonic(inst_line.mnemonic) orelse return error.no_such_instruction;
            for (inst_line.args, spec.arg_types) |arg, expected_type| {
                const arg_type = try getExprType(arg, label_types, allocator);
                if (arg_type != expected_type) return error.type_mismatch;
                try typed_args.append(.{.str = arg, .type = arg_type});
            }
            return .{.instruction = .{.mnemonic = inst_line.mnemonic, .args = try typed_args.toOwnedSlice() }};
        },
        .expr => |expr_str| {
            return.{.expr = try asTypedExpr(expr_str, label_types, allocator)};
        },
        .equate => |equate_line| {
            return .{
                .equate = .{
                    .name = equate_line.name,
                    .expr = .{
                        .str = equate_line.expr, .type = try getExprType(equate_line.expr, label_types, allocator)
                    }
                }
            };
        },
        .origin => |new_pc_expr| {
            return .{.origin = try asTypedExpr(new_pc_expr, label_types, allocator)};
        },
        .macro => std.debug.panic("macro was not replaced before evalExprsInLine call\n", .{}),
        .arrow => |sd_line| {
            return .{.arrow = try getTypedSourceDest(sd_line, label_types, allocator)};
        },
        .add_assign => |sd_line| {
            return .{.add_assign = try getTypedSourceDest(sd_line, label_types, allocator)};
        },
        .sub_assign => |sd_line| {
            return .{.sub_assign = try getTypedSourceDest(sd_line, label_types, allocator)};
        },
        .label => |label| {
            return .{.label = label};
        }
    }
}

fn getTypedLines(lines: []ParsedLine, label_types: std.StringHashMap(CPUArgType), allocator: std.mem.Allocator) ![]TypedLine {
    var out = std.ArrayList(TypedLine).init(allocator);
    for (lines) |line| {
        try out.append(try getTypedLine(line, label_types, allocator));
    }
    return try out.toOwnedSlice();
}

fn evalExprs(exprs: []TypedExpr, allocator: std.mem.Allocator) ![]Expression {
    var out = std.ArrayList(Expression).init(allocator);
    for (exprs) |expr| {
        try out.append(try evalExpr(expr.str));
    }
    return try out.toOwnedSlice();
}


fn getEvaldSourceDestLine(line: TypedSourceDestLine, pc: u12, label_values: std.StringHashMap(Expression), allocator: std.mem.Allocator) !EvaldSourceDestLine {
    var evald_dest = std.ArrayList(?u4).init(allocator);

    for (line.dest) |expr| {
        if (expr) |expr_| {
            switch (expr_.type) {
                .register => {
                    const reg_num = (try evalExpr(expr_.str, pc, label_values, allocator)).val;
                    try evald_dest.append(@intCast(reg_num));
                },
                .register_pair => {
                    const pair_num = (try evalExpr(expr_.str, pc, label_values, allocator)).val;
                    try evald_dest.append(@intCast(pair_num * 2));
                    try evald_dest.append(@intCast(pair_num * 2 + 1));
                },
                else => std.debug.panic("line passed to getEvaldSourceDestLine has invalid type(s); this should never happen", .{})
            }
        } else {
            try evald_dest.append(null);
        }
    }

    switch (line.source) {
        .num => |source_num| {
            const evald_source = (try evalExpr(source_num.str, pc, label_values, allocator)).val;
            const n_bits: u6 = @intCast(evald_dest.items.len * 4);
            if (evald_source >= (@as(u64, 1) << n_bits)) {
                return error.source_value_too_large;
            }
            return .{.source = .{.num = evald_source}, .dest = try evald_dest.toOwnedSlice()};
        },
        .regs => |source_regs| {
            var evald_source = std.ArrayList(u4).init(allocator);
            for (source_regs) |expr| {
                switch (expr.type) {
                    .register => {
                        const reg_num = (try evalExpr(expr.str, pc, label_values, allocator)).val;
                        try evald_source.append(@intCast(reg_num));
                    },
                    .register_pair => {
                        const pair_num = (try evalExpr(expr.str, pc, label_values, allocator)).val;
                        try evald_source.append(@intCast(pair_num * 2));
                        try evald_source.append(@intCast(pair_num * 2 + 1));
                    },
                    else => std.debug.panic("line passed to getEvaldSourceDestLine has invalid type(s); this should never happen", .{})
                }
            }
            if (evald_source.items.len != evald_dest.items.len) {
                std.debug.print("source/dest length mismatch\n", .{});
                return error.source_dest_length_mismatch;
            }
            if (evald_source.items.len == 0 or evald_dest.items.len == 0) {
                std.debug.print("source or dest has 0 length\n", .{});
                return error.empty_source_or_dest;
            }
            return .{ .source = .{.regs = try evald_source.toOwnedSlice()}, .dest = try evald_dest.toOwnedSlice()};
        }
    }
}

fn getExprsEvaldLine(line: TypedLine, pc: u12, label_values: std.StringHashMap(Expression), allocator: std.mem.Allocator) !?ExprsEvaldLine {
    switch (line) {
        .add_assign => |sd_line| {
            return .{.add_assign = try getEvaldSourceDestLine(sd_line, pc, label_values, allocator)};
        },
        .sub_assign => |sd_line| {
            return .{.sub_assign = try getEvaldSourceDestLine(sd_line, pc, label_values, allocator)};
        },
        .arrow => |sd_line| {
            return .{.arrow = try getEvaldSourceDestLine(sd_line, pc, label_values, allocator)};
        },
        .equate => return null,
        .expr => |expr| {
            return .{.expr = try evalExpr(expr.str, pc, label_values, allocator)};
        },
        .instruction => |inst_line| {
            var evald_args = std.ArrayList(Expression).init(allocator);
            for (inst_line.args) |arg| {
                try evald_args.append(try evalExpr(arg.str, pc, label_values, allocator));
            }
            return .{.instruction = .{.mnemonic = inst_line.mnemonic, .args = try evald_args.toOwnedSlice()}};
        },
        .origin => |new_pc_expr| {
            return .{.origin = @intCast((try evalExpr(new_pc_expr.str, pc, null, allocator)).val)};
        },
        .label => return null
    }
}

fn getExprsEvaldLines(lines: []TypedLine, label_values: std.StringHashMap(Expression), allocator: std.mem.Allocator) ![]ExprsEvaldLine {
    var out = std.ArrayList(ExprsEvaldLine).init(allocator);
    var addr: u12 = 0;
    for (lines) |line| {
        const fully_parsed_line = try getExprsEvaldLine(line, addr, label_values, allocator);
        if (fully_parsed_line) |nn_fp_line| {
            try out.append(nn_fp_line);
        }
        addr = try pcAfterLine(line, addr, allocator);
    }
    return try out.toOwnedSlice();
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
    const cmd_args = try std.process.argsAlloc(allocator);
    if (cmd_args.len != 3) {
        return error.not_2_args_given;
    }

    const input_chars = blk: {
        const input_file_path = cmd_args[1];
        const input_file = try std.fs.cwd().openFile(input_file_path, .{ .mode = .read_only });
        // assume no more than 4096 lines of code; and no more than 1 comment line per line of code; and no more than 100 average characters per line
        const input_chars = try input_file.readToEndAlloc(allocator, 4096 * 2 * 100);
        break :blk input_chars;
    };
    const lines= blk: {
        const parsed_lines = try parseLines(input_chars, allocator);
        const parsed_lines_macros_replaced = try replaceMacros(parsed_lines, allocator);
        const label_types = try getLabelTypes(parsed_lines_macros_replaced, allocator);
        const typed_lines = try getTypedLines(parsed_lines_macros_replaced, label_types, allocator);
        const label_values = try getLabels(typed_lines, allocator);
        const exprs_evald_lines = try getExprsEvaldLines(typed_lines, label_values, allocator);
        const exprs_evald_lines_arrows_replaced = try replaceSourceDestLines(exprs_evald_lines, allocator);
        break :blk exprs_evald_lines_arrows_replaced;
    };

    var assembled_bytes = std.mem.zeroes([4096]u8);
    var program_counter: u12 = 0;
    for (lines) |line| {
        switch (line) {
            .instruction => |inst_line| {
                const spec = instructionFromMnemonic(inst_line.mnemonic) orelse return error.no_such_instruction;
                const assembled_instruction = try assembleInstruction(spec, inst_line.args, program_counter);
                switch (assembled_instruction) {
                    .one => |inst| {
                        assembled_bytes[program_counter] = inst;
                        program_counter += 1;
                    },
                    .two => |inst| {
                        assembled_bytes[program_counter] = @intCast((inst >> 8) & 0xFF);
                        program_counter += 1;
                        assembled_bytes[program_counter] = @intCast(inst & 0xFF);
                        program_counter += 1;
                    },
                }
            },
            .expr => |expr| {
                if (expr.val > std.math.maxInt(u8)) {
                    return error.standalone_number_too_big;
                }
                assembled_bytes[program_counter] = @intCast(expr.val);
                program_counter += 1;
            },
            .origin => |new_pc| {
                if (new_pc > std.math.maxInt(u12)) {
                    return error.origin_number_too_big;
                }
                program_counter = @intCast(new_pc);
            },
            .arrow => std.debug.panic("arrow was not replaced\n", .{}),
            .add_assign => std.debug.panic("add assign was not replaced\n", .{}),
            .sub_assign => std.debug.panic("sub assign was not replaced\n", .{}),
        }
    }

    const output_file_path = cmd_args[2];
    const output_file = try std.fs.cwd().createFile(output_file_path, .{});
    _ = try output_file.write(&assembled_bytes);
}
