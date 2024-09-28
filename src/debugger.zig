const std = @import("std");

pub const TCP_ADDRESS = std.net.Address.initIp4(.{127, 0, 0, 1}, 5005);
pub const MAX_CMD_SIZE = 1024;

fn sendToEmulator(to_emulator: std.net.Stream.Writer, content: []const u8) !void {
    _ = try to_emulator.write(content);
    _ = try to_emulator.write("\n");
}

fn runDebuggerCmd(stdout: std.fs.File.Writer, to_emulator: std.net.Stream.Writer, from_emulator: std.net.Stream.Reader, command: []const u8) !void {
    if (std.mem.eql(u8, command, "s")) {
        try sendToEmulator(to_emulator, "s");
        var buf: [128]u8 = undefined;
        const instr = try from_emulator.readUntilDelimiter(&buf, '\n');
        try stdout.print("{s}\n", .{instr});
    } else if (std.mem.eql(u8, command, "r")) {
        try sendToEmulator(to_emulator, "r");
        while (true) {}
    } else {
        try stdout.print("invalid command\n", .{});
    }
}

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    const stream = try std.net.tcpConnectToAddress(TCP_ADDRESS);
    defer stream.close();
    const from_emulator = stream.reader();
    const to_emulator = stream.writer();
    var last_command: ?[]u8 = null;
    while (true) {
        try stdout.print("> ", .{});
        var buf: [MAX_CMD_SIZE]u8 = undefined;
        const user_input = try stdin.readUntilDelimiter(&buf, '\n');
        if (user_input.len == 0) {
            if (last_command) |last_command_| {
                try runDebuggerCmd(stdout, to_emulator, from_emulator, last_command_);
            } else {
                try stdout.print("no commands have been run yet\n", .{});
            }
        } else {
            try runDebuggerCmd(stdout, to_emulator, from_emulator, user_input);
            if (last_command) |last_command_| allocator.free(last_command_);
            last_command = try allocator.dupe(u8, user_input);
        }
    }
}