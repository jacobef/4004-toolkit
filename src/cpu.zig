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

        const port_configs = [16][4]@TypeOf(self.rom.ports[0].in_or_out[0]){
            .{.in, .in, .in, .in},  // upper 4 bits of character input from keyboard
            .{.in, .in, .in, .in},  // lower 4 bits of character input from keyboard
            .{.in, .in, .in, .in},  // "char ready" input from keyboard
            .{.out, .out, .out, .out},  // "done receiving" output to keyboard
            .{.out, .out, .out, .out},  // upper 4 bits of character output to monitor
            .{.out, .out, .out, .out},  // lower 4 bits of character output to monitor
            .{.out, .out, .out, .out},  // "char ready" output to monitor
            .{.in, .in, .in, .in},  // "done displaying" input from monitor
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
            .{.out, .out, .out, .out},
        };
        for (&self.rom.ports, port_configs) |*port, conf| {
            port.in_or_out = conf;
        }
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
        pub const IOPort = struct { in_or_out: [4]enum { in, out }, status: u4 };
        bytes: [4096]u8,
        ports: [16]IOPort,
    };

    const clock_speed_hz: u64 = 740_000;
    const ns_per_cycle: u64 = 1_000_000_000 / clock_speed_hz;

    pub fn single_step(self: *Intel4004) !void {
        std.debug.print("{s}\n", .{try disassembler.disassemble_instruction(self, self.program_counter, std.heap.page_allocator)});
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

        // for (0..nops_per_cycle) |_| {
        //     asm volatile ("" ::: "memory");
        // }
    }
};
