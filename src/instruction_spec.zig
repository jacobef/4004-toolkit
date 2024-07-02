const std = @import("std");
const ArrayList = std.ArrayList;
const cpus = @import("cpu.zig");
const Type = std.builtin.Type;
const Intel4004 = cpus.Intel4004;

const InstructionFunction = union(enum) {
    f_no_args: *const fn (cpu: *Intel4004) void,
    f_3: *const fn (cpu: *Intel4004, arg: u3) void,
    f_4: *const fn (cpu: *Intel4004, arg: u4) void,
    f_12: *const fn (cpu: *Intel4004, arg: u12) void,
    f_3_8: *const fn (cpu: *Intel4004, arg1: u3, arg2: u8) void,
    f_4_8: *const fn (cpu: *Intel4004, arg1: u4, arg2: u8) void,
};

pub const OneOrTwoBytes = union(enum) { one: u8, two: u16 };

pub const InstructionSpec = struct {
    mnemonic: []const u8,
    arg_extractors: []const ArgExtractor,
    opcode_mask: u16,
    opcode_string: []const u8,
    func: InstructionFunction,

    fn init(mnemonic: []const u8, opcode: []const u8, func: anytype) InstructionSpec {
        comptime {
            @setEvalBranchQuota(5000);
            const arg_extractors = getArgExtractors(opcode);
            const arg_count = arg_extractors.len;

            const func_union: InstructionFunction = switch (arg_count) {
                0 => .{ .f_no_args = func },
                1 => blk: {
                    const arg_bits = @popCount(arg_extractors[0].mask);
                    break :blk switch (arg_bits) {
                        3 => .{ .f_3 = func },
                        4 => .{ .f_4 = func },
                        12 => .{ .f_12 = func },
                        else => @compileError("Unsupported argument bit count"),
                    };
                },
                2 => blk: {
                    const arg1_bits = @popCount(arg_extractors[0].mask);
                    const arg2_bits = @popCount(arg_extractors[1].mask);
                    break :blk switch (arg1_bits) {
                        3 => if (arg2_bits == 8) .{ .f_3_8 = func } else @compileError("Second argument must be 8 bits for f_3_8"),
                        4 => if (arg2_bits == 8) .{ .f_4_8 = func } else @compileError("Second argument must be 8 bits for f_4_8"),
                        else => @compileError("First argument must be 3 or 4 bits for two-argument functions"),
                    };
                },
                else => @compileError("Unsupported number of arguments"),
            };

            return InstructionSpec{
                .mnemonic = mnemonic,
                .arg_extractors = arg_extractors,
                .opcode_mask = getOpcodeMask(opcode),
                .opcode_string = opcode,
                .func = func_union,
            };
        }
    }

    pub fn extractArgs(self: InstructionSpec, byte1: u8, byte2: ?u8) []const u16 {
        const opcode: u16 =
            if (byte2) |b2|
            (@as(u16, byte1) << 8) | @as(u16, b2)
        else
            @as(u16, byte1) << 8;

        const extrs = self.arg_extractors;
        switch (self.nArgs()) {
            0 => return &.{},
            1 => {
                return &.{(opcode & extrs[0].mask) >> extrs[0].shift_amount};
            },
            2 => return &.{ (opcode & extrs[0].mask) >> extrs[0].shift_amount, (opcode & extrs[1].mask) >> extrs[1].shift_amount },
            else => std.debug.panic("Only up to 2 arguments are supported", .{}),
        }
    }

    pub fn nArgs(self: InstructionSpec) usize {
        return self.arg_extractors.len;
    }
};

pub const instructions = struct {
    fn execute_nop(cpu: *Intel4004) void {
        _ = cpu;
    }

    fn execute_inc(cpu: *Intel4004, reg: u4) void {
        cpu.index_registers[reg] +%= 1;
    }

    fn execute_fin(cpu: *Intel4004, pair: u3) void {
        cpu.setRegisterPair(pair, cpu.pram.bytes[(cpu.program_counter & 0xF00) | cpu.getRegisterPair(0)]);
    }

    fn add_helper(num1: u4, num2: u4, carry_bit: u1) struct { u4, u1 } {
        const result_before_carry = @addWithOverflow(num1, num2);
        const result = @addWithOverflow(result_before_carry[0], carry_bit);
        return .{ result[0], result_before_carry[1] | result[1] };
    }

    fn execute_add(cpu: *Intel4004, reg: u4) void {
        const result = add_helper(cpu.accumulator, cpu.index_registers[reg], cpu.carry_bit);
        cpu.accumulator = result[0];
        cpu.carry_bit = result[1];
    }

    fn execute_sub(cpu: *Intel4004, reg: u4) void {
        const result = add_helper(cpu.accumulator, ~cpu.index_registers[reg], ~cpu.carry_bit);
        cpu.accumulator = result[0];
        cpu.carry_bit = result[1];
    }

    fn execute_ld(cpu: *Intel4004, reg: u4) void {
        cpu.accumulator = cpu.index_registers[reg];
    }

    fn execute_xch(cpu: *Intel4004, reg: u4) void {
        const old_accumulator: u4 = cpu.accumulator;
        cpu.accumulator = cpu.index_registers[reg];
        cpu.index_registers[reg] = old_accumulator;
    }

    fn execute_clb(cpu: *Intel4004) void {
        cpu.accumulator = 0;
        cpu.carry_bit = 0;
    }

    fn execute_clc(cpu: *Intel4004) void {
        cpu.carry_bit = 0;
    }

    fn execute_iac(cpu: *Intel4004) void {
        cpu.accumulator +%= 1;
    }

    fn execute_cmc(cpu: *Intel4004) void {
        cpu.carry_bit = ~cpu.carry_bit;
    }

    fn execute_cma(cpu: *Intel4004) void {
        cpu.accumulator = ~cpu.accumulator;
    }

    fn execute_ral(cpu: *Intel4004) void {
        const result = @shlWithOverflow(cpu.accumulator, 1);
        cpu.accumulator = result[0] | cpu.carry_bit;
        cpu.carry_bit = result[1];
    }

    fn execute_rar(cpu: *Intel4004) void {
        const acc_low_bit: u1 = @intCast(cpu.accumulator & 0x1);
        cpu.accumulator = (cpu.accumulator >> 1) | (@as(u4, cpu.carry_bit) << 3);
        cpu.carry_bit = acc_low_bit;
    }

    fn execute_tcc(cpu: *Intel4004) void {
        cpu.accumulator = cpu.carry_bit;
        cpu.carry_bit = 0;
    }

    fn execute_dac(cpu: *Intel4004) void {
        cpu.accumulator -%= 1;
    }

    fn execute_tcs(cpu: *Intel4004) void {
        cpu.accumulator = if (cpu.carry_bit == 0) 9 else 10;
        cpu.carry_bit = 0;
    }

    fn execute_stc(cpu: *Intel4004) void {
        cpu.carry_bit = 1;
    }

    fn execute_daa(cpu: *Intel4004) void {
        if (cpu.accumulator > 9 or cpu.carry_bit == 1) {
            const result = @addWithOverflow(cpu.accumulator, 6);
            cpu.accumulator = result[0];
            if (result[1] == 1) {
                cpu.carry_bit = 1;
            }
        }
    }

    fn execute_kbp(cpu: *Intel4004) void {
        const lookup = [16]u4{ 0b0000, 0b0001, 0b0010, 0b0011, 0b0100, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111 };
        cpu.accumulator = lookup[cpu.accumulator];
    }

    fn execute_fim(cpu: *Intel4004, pair: u3, val: u8) void {
        cpu.setRegisterPair(pair, val);
    }

    fn execute_ldm(cpu: *Intel4004, val: u4) void {
        cpu.accumulator = val;
    }

    fn execute_jun(cpu: *Intel4004, addr: u12) void {
        cpu.program_counter = addr;
    }

    fn execute_jin(cpu: *Intel4004, pair: u3) void {
        cpu.program_counter = (cpu.program_counter & 0xF00) | cpu.getRegisterPair(pair);
    }

    fn execute_jcn(cpu: *Intel4004, condition: u4, addr: u8) void {
        const test_cond = condition & 0b0001 == 1 and cpu.test_signal == 0;
        const carry_cond = condition & 0b0010 == 1 and cpu.carry_bit == 1;
        const acc_cond = condition & 0b0100 == 1 and cpu.accumulator == 0;
        const invert = condition & 0b1000 == 1;
        var should_jump = test_cond or carry_cond or acc_cond;
        if (invert) {
            should_jump = !should_jump;
        }
        if (should_jump) {
            cpu.program_counter = (cpu.program_counter & 0xF00) | addr;
        }
    }

    fn execute_isz(cpu: *Intel4004, reg: u4, addr: u8) void {
        cpu.index_registers[reg] +%= 1;
        if (cpu.index_registers[reg] != 0) {
            cpu.program_counter = (cpu.program_counter & 0xF00) | addr;
        }
    }

    fn execute_jms(cpu: *Intel4004, addr: u12) void {
        cpu.pushToStack(cpu.program_counter + 2);
        cpu.program_counter = addr;
    }

    fn execute_bbl(cpu: *Intel4004, val: u4) void {
        cpu.accumulator = val;
        cpu.program_counter = cpu.popFromStack();
    }

    const ins = InstructionSpec.init;
    pub const all = [_]InstructionSpec{
        ins("NOP", "00000000", execute_nop),
        ins("INC", "0110rrrr", execute_inc),
        ins("FIN", "0011ppp0", execute_fin),
        ins("ADD", "1000rrrr", execute_add),
        ins("SUB", "1001rrrr", execute_sub),
        ins("LD", "1010rrrr", execute_ld),
        ins("XCH", "1011rrrr", execute_xch),
        ins("CLB", "11110000", execute_clb),
        ins("CLC", "11110001", execute_clc),
        ins("IAC", "11110010", execute_iac),
        ins("CMC", "11110011", execute_cmc),
        ins("CMA", "11110100", execute_cma),
        ins("RAL", "11110101", execute_ral),
        ins("RAR", "11110110", execute_rar),
        ins("TCC", "11110111", execute_tcc),
        ins("DAC", "11111000", execute_dac),
        ins("TCS", "11111001", execute_tcs),
        ins("STC", "11111010", execute_stc),
        ins("DAA", "11111011", execute_daa),
        ins("KBP", "11111100", execute_kbp),
        ins("FIM", "0010ppp0nnnnnnnn", execute_fim),
        ins("LDM", "1101nnnn", execute_ldm),
        ins("JUN", "0100aaaaaaaaaaaa", execute_jun),
        ins("JIN", "0011ppp1", execute_jin),
        ins("JCN", "0001ccccaaaaaaaa", execute_jcn),
        ins("ISZ", "0111rrrraaaaaaaa", execute_isz),
        ins("JMS", "0101aaaaaaaaaaaa", execute_jms),
        ins("BBL", "1100nnnn", execute_bbl),

        // ins("DCL", "11111101", execute_dcl),
        // ins("SRC", "0010ppp1", execute_src),
        // ins("RDM", "11101001", execute_rdm),
        // ins("RD0", "11101100", execute_rd0),
        // ins("RD1", "11101101", execute_rd1),
        // ins("RD2", "11101110", execute_rd2),
        // ins("RD3", "11101111", execute_rd3),
        // ins("WR0", "11100100", execute_wr0),
        // ins("WR1", "11100101", execute_wr1),
        // ins("WR2", "11100110", execute_wr2),
        // ins("WR3", "11100111", execute_wr3),
        // ins("RDR", "11101010", execute_rdr),
        // ins("ADM", "11101011", execute_adm),
        // ins("WRM", "11100000", execute_wrm),
        // ins("WRR", "11100010", execute_wrr),
        // ins("WMP", "11100001", execute_wmp),
        // ins("SBM", "11101000", execute_sbm),
        // ins("WPM", "11100011", execute_wpm),
    };
};

const OpcodeParseError = error{ invalid_opcode_char, invalid_opcode_length };

fn addItem(comptime T: type, comptime list: []const T, comptime item: T) []const T {
    return list ++ @as([]const T, &.{item});
}

fn splitOpcode(comptime opcode: []const u8) ![]const []const u8 {
    comptime {
        if (opcode.len != 8 and opcode.len != 16) return OpcodeParseError.invalid_opcode_length;

        var opcode_split: []const []const u8 = &.{};

        var i: u8 = 0;
        while (i < opcode.len) {
            const seq_first = i;
            if (opcode[i] >= 'a' and opcode[i] != 'z') {
                while (i < opcode.len and opcode[i] == opcode[seq_first]) {
                    i += 1;
                }
                opcode_split = addItem([]const u8, opcode_split, opcode[seq_first..i]);
            } else if (opcode[i] == '0' or opcode[i] == '1') {
                while (i < opcode.len and (opcode[i] == '0' or opcode[i] == '1')) {
                    i += 1;
                }
                opcode_split = addItem([]const u8, opcode_split, opcode[seq_first..i]);
            } else {
                return .invalid_opcode_char;
            }
        }

        return opcode_split;
    }
}

const CPUArgType = enum { register, register_pair, number, address, condition };

const CPUArgSpec = struct {
    arg_type: CPUArgType,
    n_bits: u4,
};

const ParsedOpcodePart = union(enum) {
    arg: CPUArgSpec,
    bits: []const u1,

    pub fn format(
        self: ParsedOpcodePart,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .arg => |arg| {
                try writer.print("Arg(type={any}, n_bits={})", .{ arg.arg_type, arg.n_bits });
            },
            .bits => |bits| {
                try writer.print("Bits(", .{});
                for (bits) |bit| {
                    try writer.print("{d}", .{bit});
                }
                try writer.print(")", .{});
            },
        }
    }
};

const MAX_OPCODE_LENGTH = 16;

fn getArgLocations(comptime opcode: []const u8) []const [2]u8 {
    comptime {
        const opcode_split = splitOpcode(opcode) catch |err| switch (err) {
            OpcodeParseError.invalid_opcode_char => @compileError("invalid character in opcode"),
            OpcodeParseError.invalid_opcode_length => @compileError("opcode length must be 8 or 16"),
        };
        var out: []const [2]u8 = &.{};
        var bit_index: u8 = 0;

        for (opcode_split) |seq| {
            if (seq[0] == '0' or seq[0] == '1') {
                bit_index += @intCast(seq.len);
            } else {
                out = addItem([2]u8, out, [2]u8{ bit_index, @intCast(seq.len) });
                bit_index += @intCast(seq.len);
            }
        }
        return out;
    }
}

const ArgExtractor = struct {
    mask: u16,
    shift_amount: u4,
};

pub fn getArgExtractors(comptime opcode: []const u8) []const ArgExtractor {
    comptime {
        const parsedOpcode = getArgLocations(opcode);
        var extractors: []const ArgExtractor = &.{};

        for (parsedOpcode) |arg| {
            const bit_index = arg[0];
            const bit_length = arg[1];
            var mask: u16 = 0;

            for (bit_index..(bit_index + bit_length)) |bit_pos| {
                mask |= (1 << (15 - bit_pos));
            }
            const shift_amount: u16 = 15 - (bit_index + bit_length - 1);
            extractors = addItem(ArgExtractor, extractors, .{ .mask = mask, .shift_amount = shift_amount });
        }

        return extractors;
    }
}

pub fn getOpcodeMask(opcode_string: []const u8) u16 {
    comptime {
        const opcode_16 = if (opcode_string.len == 8) opcode_string ++ "00000000" else opcode_string;
        var mask: u16 = 0;
        for (opcode_16, 0..) |bit, i| {
            if (bit == '1') {
                mask |= (1 << 15) >> i;
            }
        }
        return mask;
    }
}

// thanks to sonnet 3.5 for the first draft of this function
pub fn initOpcodeLookup() [256]?InstructionSpec {
    comptime {
        @setEvalBranchQuota(20000);
        var lookup = [_]?InstructionSpec{null} ** 256;

        for (instructions.all) |inst| {
            var mask: u8 = 0;
            var value: u8 = 0;

            for (inst.opcode_string[0..8]) |c| {
                mask <<= 1;
                value <<= 1;

                switch (c) {
                    '0' => {
                        mask |= 1;
                    },
                    '1' => {
                        mask |= 1;
                        value |= 1;
                    },
                    else => {}, // For argument bits (e.g. 'r', 'a'), we don't set the mask bit
                }
            }

            // Fill in all matching opcodes
            for (0..256) |opcode| {
                if ((opcode & mask) == value) {
                    if (lookup[opcode] != null) {
                        @compileError("overlapping opcodes");
                    }
                    lookup[opcode] = inst;
                }
            }
        }

        return lookup;
    }
}

const opcode_lookup = initOpcodeLookup();

pub fn getInstructionSpec(first_8_bits: u8) ?InstructionSpec {
    return opcode_lookup[first_8_bits];
}
