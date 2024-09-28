const std = @import("std");
const cpu = @import("cpu.zig");

pub const Keyboard = struct {
    char_port_high: *u4,
    char_port_low: *u4,
    char_ready_port: *u4,

    pub fn send_string(self: *Keyboard, str: []u8) void {
        for (str) |char| {
            self.send_char(char);
        }
    }

    pub fn send_char(self: *Keyboard, char: u8) void {
        while (self.char_ready_port.* & 1 == 1) {}
        self.char_port_low.* = @intCast(char & 0xF);
        self.char_port_high.* = @intCast((char & 0xF0) >> 4);
        self.char_ready_port.* |= 1;
    }
};

pub const Monitor = struct {
    char_port_low: *u4,
    char_port_high: *u4,
    char_ready_port: *u4,

    pub fn turn_on(self: *Monitor) !void {
        const stdout = std.io.getStdOut().writer();
        while (true) {
            while (self.char_ready_port.* & 1 == 0) {}

            const char_low: u8 = self.char_port_low.*;
            const char_high: u8 = self.char_port_high.*;
            const char = (char_high << 4) | char_low;
            if (char == std.ascii.control_code.del) {
                try stdout.print("{c} {c}", .{std.ascii.control_code.bs, std.ascii.control_code.bs});
            }
            try stdout.print("{c}", .{char});

            self.char_ready_port.* &= 0b1110;
        }
    }
};
