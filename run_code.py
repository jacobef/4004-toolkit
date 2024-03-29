from textwrap import wrap
from time import sleep, time

import jsonpickle

from assemble import Expression
from disassemble import disas_instr
from log import debug_log
from parse_instruction import *
from cpu import *
from colorama import Fore, Style


def load_machine_code(cpu: Intel4004, code: bytearray):
    code_str = ''.join(format(byte, '08b') for byte in code)
    cpu.memory.program_ram[:len(code_str)] = [bit for byt in [string_to_binary(byte_str) for byte_str in wrap(code_str, 8)] for bit in byt]


def single_step(cpu: Intel4004, values_to_labels: dict[int, str] | None = None, quiet: bool = False):
    # start_time = time()
    # while time() < start_time + 0.001:
    #     pass
    instruction = get_instr(cpu.prgm_cntr, cpu.memory.program_ram)
    args = get_args(instruction, cpu.prgm_cntr, cpu.memory.program_ram)
    if not quiet:
        print(
            f"Executing {Fore.RED}{disas_instr(instruction, args, values_to_labels)}{Style.RESET_ALL} at {Fore.BLUE}{binary_to_int(cpu.prgm_cntr)}{Style.RESET_ALL}")
    cpu.prgm_cntr = addr_of_next_instr(cpu.prgm_cntr, instruction)

    if instruction == NOP:
        pass
    elif instruction == INC:
        res = add_binary(cpu.index_regs[binary_to_int(args[0])], [True], False)
        cpu.index_regs[binary_to_int(args[0])] = res.lower_bits
    elif instruction == ADD:
        reg = cpu.index_regs[binary_to_int(args[0])]
        res = add_binary(cpu.accumulator, reg, cpu.carry_bit)
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == LDM:
        cpu.accumulator = args[0]
    elif instruction == SUB:
        reg = cpu.index_regs[binary_to_int(args[0])]
        res = sub_binary(cpu.accumulator, reg, cpu.carry_bit)
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == LD:
        cpu.accumulator = cpu.index_regs[binary_to_int(args[0])]
    elif instruction == XCH:
        old_reg = cpu.index_regs[binary_to_int(args[0])]
        cpu.index_regs[binary_to_int(args[0])] = cpu.accumulator
        cpu.accumulator = old_reg
    elif instruction == IAC:
        res = add_binary(cpu.accumulator, [True], False)
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == DAC:
        res = sub_binary(cpu.accumulator, [True], False)
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == CLB:
        cpu.accumulator = [False for _ in range(4)]
        cpu.carry_bit = False
    elif instruction == CLC:
        cpu.carry_bit = False
    elif instruction == TCC:
        cpu.accumulator = [False, False, False, cpu.carry_bit]
        cpu.carry_bit = False
    elif instruction == CMC:
        cpu.carry_bit = not cpu.carry_bit
    elif instruction == CMA:
        cpu.accumulator = [not bit for bit in cpu.accumulator]
    elif instruction == JUN:
        cpu.prgm_cntr = args[0]
        addr_int = binary_to_int(args[0])
        if values_to_labels and addr_int in values_to_labels:
            debug_log(f"[CPU] JUN to {values_to_labels[addr_int]}")
    elif instruction == JCN:
        inverted, check_acc_zero, check_carry, check_test = args[0]

        acc_condition_met = binary_to_int(cpu.accumulator) == 0 and check_acc_zero
        carry_condition_met = cpu.carry_bit and check_carry
        test_condition_met = int(cpu.test_signal) == 0 and check_test

        should_jump = acc_condition_met or carry_condition_met or test_condition_met
        if inverted:
            should_jump = not should_jump

        if should_jump:
            cpu.prgm_cntr[4:] = args[1]
            # debug_log(f"[CPU] Condition {args[0]} met (acc={binary_to_int(cpu.accumulator)}, carry={cpu.carry_bit}); jumping")
        # else:
        #     debug_log(f"[CPU] Condition not met (acc={binary_to_int(cpu.accumulator)}, carry={cpu.carry_bit}); not jumping")
    elif instruction == JIN:
        ncntr = cpu.get_register_pair(args[0])
        cpu.prgm_cntr[4:] = ncntr
    elif instruction == FIM:
        cpu.set_register_pair(args[0], args[1])
    elif instruction == FIN:
        addr = cpu.prgm_cntr[:4] + cpu.index_regs[0] + cpu.index_regs[1]
        addr_int = binary_to_int(addr)
        cpu.set_register_pair(args[0], cpu.memory.program_ram[addr_int * 8: addr_int * 8 + 8])
    elif instruction == JMS:
        cpu.stack.push(cpu.prgm_cntr)
        cpu.prgm_cntr = args[0]
    elif instruction == BBL:
        ret_to = cpu.stack.pop()
        cpu.prgm_cntr = ret_to
        cpu.accumulator = args[0]
    elif instruction == DAA:
        if binary_to_int(cpu.accumulator) > 9 or cpu.carry_bit:
            add_res = add_binary(cpu.accumulator, int_to_binary(6, n_digits=4), False)
            cpu.accumulator = add_res.lower_bits
            if add_res.carry:
                cpu.carry_bit = True
    elif instruction == RAL:
        cpu.carry_bit, cpu.accumulator = cpu.accumulator[0], cpu.accumulator[1:] + [cpu.carry_bit]
    elif instruction == RAR:
        cpu.carry_bit, cpu.accumulator = cpu.accumulator[3], [cpu.carry_bit] + cpu.accumulator[:-1]
    elif instruction == KBP:
        n_ones = cpu.accumulator.count(True)
        if n_ones > 1:
            cpu.accumulator = [True, True, True, True]
        elif n_ones == 1:
            cpu.accumulator = int_to_binary(4 - cpu.accumulator.index(True), n_digits=4)
    elif instruction == STC:
        cpu.carry_bit = True
    elif instruction == TCS:
        cpu.accumulator = int_to_binary(10, n_digits=4) if cpu.carry_bit else int_to_binary(9, n_digits=4)
        cpu.carry_bit = False
    elif instruction == ISZ:
        reg_n = binary_to_int(args[0])
        register_contents = add_binary(cpu.index_regs[reg_n], [True], False).lower_bits
        cpu.index_regs[reg_n] = register_contents
        if register_contents != [False, False, False, False]:
            cpu.prgm_cntr[4:] = args[1]
    # RAM instructions
    elif instruction == DCL:
        cpu.memory.selected_bank = binary_to_int(cpu.accumulator[1:])
    elif instruction == SRC:
        cpu.memory.selected_addr = cpu.get_register_pair(args[0]).copy()
    elif instruction == RDM:
        cpu.accumulator = cpu.memory.get_data_ram_char().copy()
    elif instruction == RD0:
        cpu.accumulator = cpu.memory.get_status_ram_chars()[0].copy()
    elif instruction == RD1:
        cpu.accumulator = cpu.memory.get_status_ram_chars()[1].copy()
    elif instruction == RD2:
        cpu.accumulator = cpu.memory.get_status_ram_chars()[2].copy()
    elif instruction == RD3:
        cpu.accumulator = cpu.memory.get_status_ram_chars()[3].copy()
    elif instruction == WR0:
        cpu.memory.get_status_ram_chars()[0] = cpu.accumulator.copy()
    elif instruction == WR1:
        cpu.memory.get_status_ram_chars()[1] = cpu.accumulator.copy()
    elif instruction == WR2:
        cpu.memory.get_status_ram_chars()[2] = cpu.accumulator.copy()
    elif instruction == WR3:
        cpu.memory.get_status_ram_chars()[3] = cpu.accumulator.copy()
    elif instruction == RDR:
        port = cpu.memory.get_romio_port()
        cpu.accumulator = [line.status for line in port.lines]
    elif instruction == ADM:
        added = add_binary(cpu.accumulator, cpu.memory.get_data_ram_char(), cpu.carry_bit)
        cpu.accumulator = added.lower_bits
        cpu.carry_bit = added.carry
    elif instruction == WRM:
        cpu.memory.set_data_ram_char(cpu.accumulator)
    elif instruction == WRR:
        port_n = binary_to_int(cpu.memory.selected_addr[:4])
        if port_n not in (14, 15):
            debug_log(f"[CPU] Writing {binary_to_string(cpu.accumulator)} to port {port_n}")
        cpu.memory.set_romio_port(cpu.accumulator)
    elif instruction == WMP:
        cpu.memory.set_ramo_port(cpu.accumulator)
    elif instruction == SBM:
        result = sub_binary(cpu.accumulator, cpu.memory.get_data_ram_char(), cpu.carry_bit)
        cpu.accumulator = result.lower_bits
        cpu.carry_bit = result.carry
    elif instruction == WPM:
        high_4 = [line.status for line in cpu.memory.rom_ports[15].lines]
        low_8 = cpu.memory.selected_addr
        addr = binary_to_int(high_4 + low_8)
        nibble_start = cpu.memory.wpm_half_byte * 4
        nibble_end = nibble_start + 4

        if cpu.memory.program_ram_write_enable:
            cpu.memory.program_ram[addr*8+nibble_start:addr*8+nibble_end] = cpu.accumulator.copy()
            # debug_log(f"[CPU] Wrote {binary_to_string(cpu.accumulator)} to program RAM address {addr} (nibble {cpu.memory.wpm_half_byte})")
        else:
            to_read = cpu.memory.program_ram[addr * 8 + nibble_start : addr * 8 + nibble_end]
            rom_port_n = 14 if cpu.memory.wpm_half_byte == 0 else 15
            for line, read_bit in zip(cpu.memory.rom_ports[rom_port_n].lines, to_read):
                line.status = read_bit
            debug_log(f"[CPU] Read {binary_to_string(to_read)} from program RAM address {addr} (nibble {cpu.memory.wpm_half_byte})")

        cpu.memory.wpm_half_byte = 0 if cpu.memory.wpm_half_byte == 1 else 1

    # Convenience Instructions: (do not exist, delete when done)
    elif instruction == STOP:
        exit(0)
    if not quiet:
        print("Done")


def turn_on(cpu: Intel4004):
    with open("debug.json", "r") as debug_file:
        labels_to_values: dict[str, int] = jsonpickle.loads(debug_file.read())
    values_to_labels: dict[int, str] = {v: k for k, v in labels_to_values.items()}
    while True:
        single_step(cpu, values_to_labels, quiet=True)
