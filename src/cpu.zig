const std = @import("std");
const time = std.time;
const instruction_spec = @import("instruction_spec.zig");
const disassembler = @import("disassembler.zig");

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
    rom: ROM,

    pub fn pushToStack(self: *Intel4004, address: u12) void {
        self.stack_registers[self.stack_pointer] = address;
        self.stack_pointer = if (self.stack_pointer < 2) self.stack_pointer + 1 else 0;
    }

    pub fn popFromStack(self: *Intel4004) u12 {
        self.stack_pointer = if (self.stack_pointer > 0) self.stack_pointer - 1 else 2;
        return self.stack_registers[self.stack_pointer];
    }

    pub fn getRegisterPair(self: Intel4004, pair: u3) u8 {
        const reg1_val: u8 = self.index_registers[@as(u4, pair) * 2];
        const reg2_val: u8 = self.index_registers[@as(u4, pair) * 2 + 1];
        return (reg1_val << 4) | reg2_val;
    }

    pub fn setRegisterPair(self: *Intel4004, pair: u3, val: u8) void {
        self.index_registers[@as(u4, pair) * 2] = @intCast(val >> 4);
        self.index_registers[@as(u4, pair) * 2 + 1] = @intCast(val & 0xF);
    }

    pub fn reset(self: *Intel4004) void {
        self.* = std.mem.zeroes(Intel4004);
    }

    pub const DataRAM = struct {
        pub const Register = struct { data: [16]u4, status: [4]u4 };
        pub const Bank = struct { chips: [4][4]Register, ports: [4]u4 };
        banks: [8]Bank,
        ports: [4]u4,
        selected_bank: u3,
    };
    pub const ProgramRAM = struct { bytes: [4096]u8, wpm_half_byte: u1 };
    pub const ROM = struct {
        bytes: [4096]u8,
        ports: [16]u4,
    };

    const clock_speed_hz: u64 = 740_000;
    const ns_per_cycle: u64 = 1_000_000_000 / clock_speed_hz;

    pub fn single_step(self: *Intel4004) !void {
        const start_time = time.nanoTimestamp();

        const byte1 = self.pram.bytes[self.program_counter];
        const inst_spec = instruction_spec.getInstructionSpec(byte1) orelse return error.illegal_instruction_error;
        const n_bytes = inst_spec.opcode_string.len / 8;
        const byte2 = if (n_bytes == 2) self.pram.bytes[self.program_counter + 1] else null;
        self.program_counter +%= @intCast(n_bytes);
        var args_buf: [2]u16 = undefined;
        const args = inst_spec.extractArgs(byte1, byte2, &args_buf);

        switch (inst_spec.func) {
            .f_no_args => |f| f(self),
            .f_3 => |f| f(self, @intCast(args[0])),
            .f_4 => |f| f(self, @intCast(args[0])),
            .f_12 => |f| f(self, @intCast(args[0])),
            .f_3_8 => |f| f(self, @intCast(args[0]), @intCast(args[1])),
            .f_4_8 => |f| f(self, @intCast(args[0]), @intCast(args[1])),
        }

        const end_time = time.nanoTimestamp();
        const expected_time_ns = ns_per_cycle * n_bytes;
        const actual_time_ns = end_time - start_time;
        if (actual_time_ns < expected_time_ns) {
            const remaining_time_ns = expected_time_ns - actual_time_ns;
            busy_wait(remaining_time_ns);
        }
    }

    fn busy_wait(wait_time_ns: i128) void {
        const start = time.nanoTimestamp();
        var now = start;
        while ((now - start) < wait_time_ns) {
            now = time.nanoTimestamp();
        }
    }
};
