const std = @import("std");

pub const TCP_ADDRESS = std.net.Address.initIp4(.{127, 0, 0, 1}, 5005);
pub const MAX_CMD_SIZE = 1024;

fn sendToEmulator(to_emulator: std.net.Stream.Writer, content: []const u8) !void {
    _ = try to_emulator.write(content);
    _ = try to_emulator.write("\n");
}

const StepCPUResult = struct {
    instruction: []const u8,
    new_pc: u12
};

fn stepCPU(to_emulator: std.net.Stream.Writer, from_emulator: std.net.Stream.Reader, allocator: std.mem.Allocator) !StepCPUResult {
    try sendToEmulator(to_emulator, "s");
    const instruction = try from_emulator.readUntilDelimiterAlloc(allocator, '\n', 100);
    var buf: [10]u8 = undefined;
    const new_pc_str = try from_emulator.readUntilDelimiter(&buf, '\n');
    const new_pc = try std.fmt.parseInt(u12, new_pc_str, 10);
    return .{.instruction = instruction, .new_pc = new_pc};
}

fn runDebuggerCmd(stdout: std.fs.File.Writer, to_emulator: std.net.Stream.Writer, from_emulator: std.net.Stream.Reader, command: []const u8, breakpoints: *std.ArrayList(u12), allocator: std.mem.Allocator) !void {
    var cmd_it = std.mem.tokenizeScalar(u8, command, ' ');
    const cmd_name = cmd_it.next().?;
    if (std.mem.eql(u8, cmd_name, "s")) {
        const result = try stepCPU(to_emulator, from_emulator, allocator);
        try stdout.print("instr: {s}\n", .{result.instruction});
        try stdout.print("pc: {d}\n", .{result.new_pc});
    } else if (std.mem.eql(u8, cmd_name, "r")) {
        var result = try stepCPU(to_emulator, from_emulator, allocator);
        while (!std.mem.containsAtLeast(u12, breakpoints.items, 1, &.{result.new_pc})) {
            result = try stepCPU(to_emulator, from_emulator, allocator);
        }
        try stdout.print("reached breakpoint\n", .{});
        try stdout.print("instr: {s}\n", .{result.instruction});
        try stdout.print("pc: {d}\n", .{result.new_pc});
    } else if (std.mem.eql(u8, cmd_name, "b")) {
        const addr_str = cmd_it.next().?;
        const addr = try std.fmt.parseInt(u12, addr_str, 10);
        try breakpoints.append(addr);
    } else if (std.mem.eql(u8, cmd_name, "p")) {
        try sendToEmulator(to_emulator, "p");
        const info_msg = try from_emulator.readUntilDelimiterAlloc(allocator, '\n', 1000);
        try stdout.print("{s}\n", .{info_msg});
    } else if (std.mem.eql(u8, cmd_name, "m")) {
        try sendToEmulator(to_emulator, command);
        const nibble_str = try from_emulator.readUntilDelimiterAlloc(allocator, '\n', 3);
        try stdout.print("{s}\n", .{nibble_str});
    } else if (std.mem.eql(u8, cmd_name, "args")) {
        try sendToEmulator(to_emulator, "args");
        const nibble_str = try from_emulator.readUntilDelimiterAlloc(allocator, '\n', 1000);
        try stdout.print("{s}\n", .{nibble_str});
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
    var breakpoints = std.ArrayList(u12).init(allocator);
    while (true) {
        try stdout.print("> ", .{});
        var buf: [MAX_CMD_SIZE]u8 = undefined;
        const user_input = try stdin.readUntilDelimiter(&buf, '\n');
        if (user_input.len == 0) {
            if (last_command) |last_command_| {
                try runDebuggerCmd(stdout, to_emulator, from_emulator, last_command_, &breakpoints, allocator);
            } else {
                try stdout.print("no commands have been run yet\n", .{});
            }
        } else {
            try runDebuggerCmd(stdout, to_emulator, from_emulator, user_input, &breakpoints, allocator);
            if (last_command) |last_command_| allocator.free(last_command_);
            last_command = try allocator.dupe(u8, user_input);
        }
    }
}