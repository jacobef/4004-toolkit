const std = @import("std");

const cpus = @import("cpu.zig");
const Intel4004 = cpus.Intel4004;

const instruction_spec = @import("instruction_spec.zig");
const InstructionSpec = instruction_spec.InstructionSpec;

const disassembler = @import("disassembler.zig");

const devices = @import("devices.zig");
const Keyboard = devices.Keyboard;
const Monitor = devices.Monitor;

const debugger = @import("debugger.zig");

const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("termios.h");
    @cInclude("unistd.h");
});

const lookup = instruction_spec.initOpcodeLookup();

fn set_cbreak_mode() c.termios {
    var old_settings: c.termios = undefined;
    var new_settings: c.termios = undefined;
    _ = c.tcgetattr(c.STDIN_FILENO, &old_settings);
    _ = c.tcgetattr(c.STDIN_FILENO, &new_settings);
    new_settings.c_lflag &= ~(@as(c_ulong, c.ICANON) | @as(c_ulong, c.ECHO));
    _ = c.tcsetattr(c.STDIN_FILENO, c.TCSANOW, &new_settings);
    return old_settings;
}

fn getch() u8 {
    return @intCast(c.getchar());
}

fn start_cpu(intel4004: *Intel4004) !void {
    while (true) {
        try intel4004.single_step();
    }
}

fn u4s_to_u12(nib1: u4, nib2: u4, nib3: u4) u12 {
    return @as(u12, nib3) | (@as(u12, nib2) << 4) | (@as(u12, nib1) << 8);
}

fn u12_to_u4s(val: u12) struct { u4, u4, u4 } {
    return .{
        @intCast(val >> 8),
        @intCast((val >> 4) & 0xF),
        @intCast(val & 0xF),
    };
}

fn get_mem_nibble(intel4004: *Intel4004, n: u12) u4 {
    const nib1, const nib2, const nib3 = u12_to_u4s(n);
    const bank_n = nib1;
    const chip_n = @divFloor(nib2, 4);
    const reg_n = nib2 % 4;
    const char_n = nib3;
    return intel4004.dram.banks[bank_n].chips[chip_n][reg_n].data[char_n];
}

fn start_cpu_with_debugger(intel4004: *Intel4004, allocator: std.mem.Allocator) !void {
    var server = try debugger.TCP_ADDRESS.listen(.{});
    defer server.deinit();
    std.log.info("waiting for debugger to connect...", .{});
    const stream = (try server.accept()).stream;
    std.log.info("debugger connected", .{});
    const from_debugger = stream.reader();
    const to_debugger = stream.writer();
    while (true) {
        var buf: [debugger.MAX_CMD_SIZE]u8 = undefined;
        const debugger_command = try from_debugger.readUntilDelimiter(&buf, '\n');
        var cmd_it = std.mem.tokenizeScalar(u8, debugger_command, ' ');
        const cmd_name = cmd_it.next().?;

        if (std.mem.eql(u8, cmd_name, "s")) {
            try intel4004.single_step();
            const next_instr = try disassembler.disassemble_instruction(intel4004, intel4004.program_counter, allocator);
            defer allocator.free(next_instr);
            _ = try to_debugger.write(next_instr);
            _ = try to_debugger.write("\n");
            var addr_buf: [4]u8 = undefined;
            const addr_str = try std.fmt.bufPrint(&addr_buf, "{d}", .{intel4004.program_counter});
            _ = try to_debugger.write(addr_str);
            _ = try to_debugger.write("\n");
        } else if (std.mem.eql(u8, cmd_name, "p")) {
            for (0..16) |i| {
                var msg_part_buf: [25]u8 = undefined;
                const msg_part = try std.fmt.bufPrint(&msg_part_buf, "{d}R={d} ", .{ i, intel4004.index_registers[i] });
                _ = try to_debugger.write(msg_part);
            }
            _ = try to_debugger.write("\n");
        } else if (std.mem.eql(u8, cmd_name, "m")) {
            const nib1 = try std.fmt.parseInt(u4, cmd_it.next().?, 10);
            const nib2 = try std.fmt.parseInt(u4, cmd_it.next().?, 10);
            const nib3 = try std.fmt.parseInt(u4, cmd_it.next().?, 10);
            const val = get_mem_nibble(intel4004, u4s_to_u12(nib1, nib2, nib3));
            var val_buf: [2]u8 = undefined;
            const val_str = try std.fmt.bufPrint(&val_buf, "{d}", .{val});
            _ = try to_debugger.write(val_str);
            _ = try to_debugger.write("\n");
        } else if (std.mem.eql(u8, cmd_name, "args")) {
            const argc = intel4004.index_registers[0];
            const argv = u4s_to_u12(intel4004.index_registers[1], intel4004.index_registers[2], intel4004.index_registers[3]);
            for (0..argc) |arg_i| {
                const argv_it: u12 = @intCast(argv + arg_i * 6);
                const arg_start_1 = get_mem_nibble(intel4004, argv_it);
                const arg_start_2 = get_mem_nibble(intel4004, argv_it + 1);
                const arg_start_3 = get_mem_nibble(intel4004, argv_it + 2);
                const arg_start = u4s_to_u12(arg_start_1, arg_start_2, arg_start_3);
                const arg_end_1 = get_mem_nibble(intel4004, argv_it + 3);
                const arg_end_2 = get_mem_nibble(intel4004, argv_it + 4);
                const arg_end_3 = get_mem_nibble(intel4004, argv_it + 5);
                const arg_end = u4s_to_u12(arg_end_1, arg_end_2, arg_end_3);

                var char_addr = arg_start;
                while (char_addr != arg_end) : (char_addr += 2) {
                    const char_nib_1 = get_mem_nibble(intel4004, char_addr);
                    const char_nib_2 = get_mem_nibble(intel4004, char_addr + 1);
                    const char = @as(u8, char_nib_2) | (@as(u8, char_nib_1) << 4);
                    _ = try to_debugger.writeByte(char);
                }
                _ = try to_debugger.writeByte(' ');
            }
            _ = try to_debugger.writeByte('\n');
        } else {
            std.log.err("invalid command sent from debugger", .{});
        }
    }
}

const KeyEntry = struct {
    node: std.DoublyLinkedList.Node,
    data: u8,
};

fn queue_to_kb(queue: *std.DoublyLinkedList, kb: *Keyboard, lock: *std.atomic.Value(bool), allocator: std.mem.Allocator) void {
    while (true) {
        if (queue.first) |_| {
            while (lock.cmpxchgWeak(false, true, .acquire, .monotonic) == false) {
                std.atomic.spinLoopHint();
            }
            while (queue.popFirst()) |n| {
                const entry: *KeyEntry = @fieldParentPtr("node", n);
                kb.send_char(entry.data);
                allocator.destroy(entry);
            }
            lock.store(false, .release);
        }
    }
}

fn input_to_queue(queue: *std.DoublyLinkedList, lock: *std.atomic.Value(bool), allocator: std.mem.Allocator) !void {
    while (true) {
        while (lock.cmpxchgWeak(false, true, .acquire, .monotonic) == false) {
            std.atomic.spinLoopHint();
        }

        const ch = getch();
        const entry = try allocator.create(KeyEntry);
        entry.* = .{ .node = .{}, .data = ch };
        queue.append(&entry.node);

        lock.store(false, .release);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const old_settings = set_cbreak_mode();
    defer _ = c.tcsetattr(c.STDIN_FILENO, c.TCSANOW, &old_settings);

    var intel4004: Intel4004 = undefined;
    intel4004.reset();

    var cmd_args = try std.process.argsWithAllocator(allocator);
    _ = cmd_args.next();
    const executable_path = cmd_args.next() orelse return error.NoInputFileGiven;
    const executable_file = try std.fs.cwd().openFile(executable_path, .{ .mode = .read_only });
    _ = try executable_file.readAll(&intel4004.pram.bytes);

    var queue = std.DoublyLinkedList{};

    var keyboard = Keyboard{
        .char_port_high = &intel4004.rom.ports[0],
        .char_port_low = &intel4004.rom.ports[1],
        .char_ready_port = &intel4004.rom.ports[2],
    };
    var monitor = Monitor{
        .char_port_high = &intel4004.rom.ports[3],
        .char_port_low = &intel4004.rom.ports[4],
        .char_ready_port = &intel4004.rom.ports[5],
    };
    var lock = std.atomic.Value(bool).init(false);

    const cpu_thread = try std.Thread.spawn(.{}, start_cpu, .{&intel4004});
    _ = try std.Thread.spawn(.{}, devices.Monitor.turn_on, .{&monitor});
    _ = try std.Thread.spawn(.{}, input_to_queue, .{ &queue, &lock, allocator });
    _ = try std.Thread.spawn(.{}, queue_to_kb, .{ &queue, &keyboard, &lock, allocator });

    cpu_thread.join();
}
