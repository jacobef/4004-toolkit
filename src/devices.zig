const std = @import("std");
const cpu = @import("cpu.zig");
const IOPort = cpu.Intel4004.ROM.IOPort;

pub const Keyboard = struct {
    char_port_high: *IOPort,
    char_port_low: *IOPort,
    char_ready_port: *IOPort,
    char_ready_cache: u1 = 0,
    done_receiving_port: *IOPort,
    done_receiving_cache: u1 = 0,

    pub fn send_string(self: *Keyboard, str: []u8) void {
        for (str) |char| {
            self.send_char(char);
        }
    }

    pub fn send_char(self: *Keyboard, char: u8) void {
        // std.debug.print("[KEYBOARD] Waiting for done_receiving\n", .{});
        // Wait for the CPU to say it's done receiving
        while (@as(u1, @intCast(self.done_receiving_port.status & 1)) == self.done_receiving_cache) {}
        self.done_receiving_cache = @as(u1, @intCast(self.done_receiving_port.status & 1));
        // std.debug.print("[KEYBOARD] Saw that done_receiving flipped\n", .{});
        // Send the char
        self.char_port_low.status = @intCast(char & 0xF);
        self.char_port_high.status = @intCast((char & 0xF0) >> 4);
        // std.debug.print("[KEYBOARD] Sent character {}\n", .{char});
        // Flip char_ready
        self.char_ready_cache = ~self.char_ready_cache;
        self.char_ready_port.status = (self.done_receiving_port.status & 0b1110) | @as(u4, self.char_ready_cache);
        // std.debug.print("[KEYBOARD] Flipped char_ready", .{});
    }
};

pub const Monitor = struct {
    char_port_low: *IOPort,
    char_port_high: *IOPort,
    char_ready_port: *IOPort,
    char_ready_cache: u1 = 0,
    done_displaying_port: *IOPort,
    done_displaying_cache: u1 = 0,

    pub fn turn_on(self: *Monitor) !void {
        while (true) {
            std.time.sleep(10 * std.time.ns_per_ms);
            // std.debug.print("[MONITOR] Waiting for char_ready\n", .{});
            while (@as(u1, @intCast(self.char_ready_port.status & 1)) == self.char_ready_cache) {}
            // std.debug.print("[MONITOR] Saw that char_ready flipped\n", .{});
            self.char_ready_cache = @as(u1, @intCast(self.char_ready_port.status & 1));

            // Display the character
            const char_low = @as(u8, @intCast(self.char_port_low.status));
            const char_high = @as(u8, @intCast(self.char_port_high.status));
            const char = (char_high << 4) | char_low;
            if (char == std.ascii.control_code.del) {
                // std.debug.print("del received\n", .{});
                std.debug.print("{c} {c}", .{std.ascii.control_code.bs, std.ascii.control_code.bs});
            }
            std.debug.print("{c}", .{char});
            // std.debug.print("[MONITOR] Displayed {c}\n", .{char});

            // Signal we're done displaying
            self.done_displaying_cache = ~self.done_displaying_cache;
            self.done_displaying_port.status = (self.done_displaying_port.status & 0b1110) | @as(u4, self.done_displaying_cache);
            // std.debug.print("[MONITOR] Flipped done_displaying\n", .{});
        }
    }
};
