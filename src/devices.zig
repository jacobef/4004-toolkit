const std = @import("std");
const cpu = @import("cpu.zig");

pub const Keyboard = struct {
    char_port_high: *u4,
    char_port_low: *u4,
    char_ready_port: *u4,
    char_ready_cache: u1 = 0,
    done_receiving_port: *u4,
    done_receiving_cache: u1 = 0,

    pub fn send_string(self: *Keyboard, str: []u8) void {
        for (str) |char| {
            self.send_char(char);
        }
    }

    pub fn send_char(self: *Keyboard, char: u8) void {
        while (@as(u1, @intCast(self.done_receiving_port.* & 1)) == self.done_receiving_cache) {}
        self.done_receiving_cache = @as(u1, @intCast(self.done_receiving_port.* & 1));
        self.char_port_low.* = @intCast(char & 0xF);
        self.char_port_high.* = @intCast((char & 0xF0) >> 4);
        self.char_ready_cache = ~self.char_ready_cache;
        self.char_ready_port.* = (self.char_ready_port.* & 0b1110) | @as(u4, self.char_ready_cache);
    }
};

pub const Monitor = struct {
    char_port_low: *u4,
    char_port_high: *u4,
    char_ready_port: *u4,
    char_ready_cache: u1 = 0,
    done_displaying_port: *u4,
    done_displaying_cache: u1 = 0,

    pub fn turn_on(self: *Monitor) !void {
        while (true) {
            std.time.sleep(10 * std.time.ns_per_ms);
            while (@as(u1, @intCast(self.char_ready_port.* & 1)) == self.char_ready_cache) {}
            self.char_ready_cache = @as(u1, @intCast(self.char_ready_port.* & 1));

            const char_low = @as(u8, @intCast(self.char_port_low.*));
            const char_high = @as(u8, @intCast(self.char_port_high.*));
            const char = (char_high << 4) | char_low;
            if (char == std.ascii.control_code.del) {
                std.debug.print("{c} {c}", .{std.ascii.control_code.bs, std.ascii.control_code.bs});
            }
            std.debug.print("{c}", .{char});

            self.done_displaying_cache = ~self.done_displaying_cache;
            self.done_displaying_port.* = (self.done_displaying_port.* & 0b1110) | @as(u4, self.done_displaying_cache);
        }
    }
};
