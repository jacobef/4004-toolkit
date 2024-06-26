const std = @import("std");
const time = std.time;
const instructions = @import("instruction_spec.zig");

pub const Intel4004 = struct {
    index_registers: [16]u4,
    program_counter: u12,
    accumulator: u4,
    carry_bit: u1,
    test_signal: u1,
    stack_registers: [3]u12,
    stack_pointer: u2,
    src_address: u8,
    dram: DataRAM,
    pram: ProgramRAM,

    pub fn pushToStack(self: *Intel4004, address: u12) void {
        self.stack_registers[self.stack_pointer] = address;
        self.stack_pointer = if (self.stack_pointer < 2) self.stack_pointer + 1 else 0;
    }

    pub fn popFromStack(self: *Intel4004) u12 {
        self.stack_pointer = if (self.stack_pointer > 0) self.stack_pointer - 1 else 2;
        return self.stack_registers[self.stack_pointer];
    }

    pub fn getRegisterPair(self: Intel4004, pair: u3) u8 {
        const reg1_val: u8 = @intCast(self.index_registers[pair * 2]);
        const reg2_val: u8 = @intCast(self.index_registers[pair * 2 + 1]);
        return (reg1_val << 4) | reg2_val;
    }

    pub fn setRegisterPair(self: *Intel4004, pair: u3, val: u8) void {
        self.index_registers[pair * 2] = @intCast(val >> 4);
        self.index_registers[pair * 2 + 1] = @intCast(val & 0xF);
    }

    pub fn reset(self: *Intel4004) void {
        self.* = std.mem.zeroes(Intel4004);
    }

    pub const DataRAM = struct {
        pub const Register = struct { data: [16]u4, status: [4]u4 };
        banks: [8][4][4]Register,
        ports: [4]u4,
        selected_bank: u3,
    };
    pub const ProgramRAM = struct { bytes: [4096]u8, write_enable: bool, wpm_half_byte: u1 };
    pub const ROM = struct {
        pub const IOLine = struct { in_or_out: enum { in, out }, status: bool };
        bytes: [4096]u8,
        ports: [16][4]IOLine,
    };

    const ExecutionError = error{illegal_instruction_error};

    const clock_speed_hz: u64 = 740_000;
    const ns_per_cycle: u64 = 1_000_000_000 / clock_speed_hz;

    pub fn single_step(self: *Intel4004) !void {
        const byte1 = self.pram.bytes[self.program_counter];
        const inst_spec = instructions.getInstructionSpec(byte1) orelse return ExecutionError.illegal_instruction_error;
        const n_bytes = inst_spec.opcode_string.len / 8;
        const byte2 = if (n_bytes == 2) self.pram.bytes[self.program_counter + 1] else null;
        self.program_counter +%= @intCast(n_bytes);
        const args = inst_spec.extractArgs(byte1, byte2);

        switch (inst_spec.func) {
            .f_no_args => |f| f(self),
            .f_3 => |f| f(self, @intCast(args[0])),
            .f_4 => |f| f(self, @intCast(args[0])),
            .f_12 => |f| f(self, @intCast(args[0])),
            .f_3_8 => |f| f(self, @intCast(args[0]), @intCast(args[1])),
            .f_4_8 => |f| f(self, @intCast(args[0]), @intCast(args[1])),
        }

        // for (0..nops_per_cycle) |_| {
        //     asm volatile ("" ::: "memory");
        // }
    }
};
