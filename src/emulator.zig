const std = @import("std");
const ArrayList = std.ArrayList;

const cpus = @import("cpu.zig");
const Intel4004 = cpus.Intel4004;

const instruction_spec = @import("instruction_spec.zig");
const InstructionSpec = instruction_spec.InstructionSpec;

const devices = @import("devices.zig");
const Keyboard = devices.Keyboard;
const Monitor = devices.Monitor;

const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("termios.h");
    @cInclude("unistd.h");
});

const lookup = instruction_spec.initOpcodeLookup();

fn set_cbreak_mode() void {
    var new_settings: c.termios = undefined;
    _ = c.tcgetattr(c.STDIN_FILENO, &new_settings);
    new_settings.c_lflag &= ~(@as(c_ulong, c.ICANON) | @as(c_ulong, c.ECHO));
    _ = c.tcsetattr(c.STDIN_FILENO, c.TCSANOW, &new_settings);
}

fn getch() u8 {
    return @intCast(c.getchar());
}

fn start_cpu(intel4004: *Intel4004) !void {
    while (true) {
        try intel4004.single_step();
    }
}

fn queue_to_kb(queue: *std.DoublyLinkedList(u8), kb: *Keyboard, lock: *std.atomic.Value(bool), allocator: std.mem.Allocator) void {
    while (true) {
        if (queue.len != 0) {
            while (lock.cmpxchgWeak(false, true, .acquire, .monotonic) == false) {
                std.atomic.spinLoopHint();
            }
            while (queue.popFirst()) |char| {
                kb.send_char(char.data);
                allocator.destroy(char);
            }
            lock.store(false, .release);
        }
    }
}

fn input_to_queue(queue: *std.DoublyLinkedList(u8), lock: *std.atomic.Value(bool), allocator: std.mem.Allocator) !void {
    while (true) {
        while (lock.cmpxchgWeak(false, true, .acquire, .monotonic) == false) {
            std.atomic.spinLoopHint();
        }

        const char = getch();
        const node = try allocator.create(std.DoublyLinkedList(u8).Node);
        node.* = .{.data = char};
        queue.append(node);

        lock.store(false, .release);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    set_cbreak_mode();

    var intel4004: Intel4004 = undefined;
    intel4004.reset();

    var cmd_args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = cmd_args.next();
    const executable_path = cmd_args.next() orelse return error.NoInputFileGiven;
    const executable_file = try std.fs.cwd().openFile(executable_path, .{ .mode = .read_only });
    _ = try executable_file.readAll(&intel4004.pram.bytes);

    var queue = std.DoublyLinkedList(u8){};

    var keyboard = Keyboard {
        .char_port_high = &intel4004.rom.ports[0],
        .char_port_low = &intel4004.rom.ports[1],
        .char_ready_port = &intel4004.rom.ports[2],
        .done_receiving_port = &intel4004.rom.ports[3],
    };
    var monitor = Monitor {
        .char_port_high = &intel4004.rom.ports[4],
        .char_port_low = &intel4004.rom.ports[5],
        .char_ready_port = &intel4004.rom.ports[6],
        .done_displaying_port = &intel4004.rom.ports[7]
    };
    var lock = std.atomic.Value(bool).init(false);

    const cpu_thread = try std.Thread.spawn(.{}, start_cpu, .{&intel4004});
    _ = try std.Thread.spawn(.{}, devices.Monitor.turn_on, .{&monitor});
    _ = try std.Thread.spawn(.{}, input_to_queue, .{&queue, &lock, allocator});
    _ = try std.Thread.spawn(.{}, queue_to_kb, .{&queue, &keyboard, &lock, allocator});

    cpu_thread.join();
}
