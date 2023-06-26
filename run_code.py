from time import sleep, time

from disassemble import disas_instr
from log import debug_log
from parse_instruction import *
from cpu import *
from colorama import Fore, Style


def load_machine_code(cpu: Intel4004, code: bytearray):
    code_str = ''.join(format(byte, '08b') for byte in code)
    cpu.memory.rom[:len(code_str)] = string_to_binary(code_str)


def single_step(cpu: Intel4004, quiet: bool = False):
    start_time = time()
    while time() < start_time + 0.001:
        pass

    instruction = get_instr(cpu.prgm_cntr, cpu.memory.rom)
    args = get_args(instruction, cpu.prgm_cntr, cpu.memory.rom)
    if not quiet:
        print(
            f"Executing {Fore.RED}{disas_instr(instruction, args)}{Style.RESET_ALL} at {Fore.BLUE}{binary_to_int(cpu.prgm_cntr)}{Style.RESET_ALL}")
    cpu.prgm_cntr = addr_of_next_instr(cpu.prgm_cntr, instruction)

    if instruction == NOP:
        pass
    elif instruction == INC:
        res = add_binary(cpu.index_regs[binary_to_int(args[0])], [True])
        cpu.index_regs[binary_to_int(args[0])] = res.lower_bits
    elif instruction == ADD:
        reg = cpu.index_regs[binary_to_int(args[0])]
        res = add_binary(cpu.accumulator, reg)
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == LDM:
        cpu.accumulator = args[0]
    elif instruction == SUB:
        reg = cpu.index_regs[binary_to_int(args[0])]
        res = sub_binary(cpu.accumulator, reg)
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == LD:
        cpu.accumulator = cpu.index_regs[binary_to_int(args[0])]
    elif instruction == XCH:
        old_reg = cpu.index_regs[binary_to_int(args[0])]
        cpu.index_regs[binary_to_int(args[0])] = cpu.accumulator
        cpu.accumulator = old_reg
    elif instruction == IAC:
        res = add_binary(cpu.accumulator, [True])
        cpu.accumulator = res.lower_bits
        cpu.carry_bit = res.carry
    elif instruction == DAC:
        res = sub_binary(cpu.accumulator, [True])
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
        cpu.accumulator = complement(cpu.accumulator)
    elif instruction == JUN:
        cpu.prgm_cntr = args[0]
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
    elif instruction == JIN:
        ncntr = cpu.get_register_pair(args[0])
        cpu.prgm_cntr[4:] = ncntr
    elif instruction == FIM:
        cpu.set_register_pair(args[0], args[1])
    elif instruction == FIN:
        addr = cpu.prgm_cntr[:4] + cpu.index_regs[0] + cpu.index_regs[1]
        addr_int = binary_to_int(addr)
        cpu.set_register_pair(args[0], cpu.memory.rom[addr_int * 8: addr_int * 8 + 8])
    elif instruction == JMS:
        cpu.stack.push(cpu.prgm_cntr)
        cpu.prgm_cntr = args[0]
    elif instruction == BBL:
        cpu.prgm_cntr = cpu.stack.pop()
        cpu.accumulator = args[0]
    elif instruction == DAA:
        if binary_to_int(cpu.accumulator) > 9 or cpu.carry_bit:
            add_res = add_binary(cpu.accumulator, int_to_binary(6, n_digits=4))
            cpu.accumulator = add_res.lower_bits
            if add_res.carry:
                cpu.carry_bit = True
    elif instruction == RAL:
        old_carry = cpu.carry_bit
        cpu.carry_bit, cpu.accumulator = cpu.accumulator[0], cpu.accumulator[1:] + [old_carry]
    elif instruction == RAR:
        old_carry = cpu.carry_bit
        cpu.carry_bit, cpu.accumulator = cpu.accumulator[3], [old_carry] + cpu.accumulator[:-1]
    elif instruction == KBP:
        accu = cpu.accumulator
        n_ones = accu.count(True)
        if n_ones > 1:
            cpu.accumulator = [True, True, True, True]
        elif n_ones == 1:
            cpu.accumulator = int_to_binary(4 - accu.index(True), n_digits=4)
    elif instruction == STC:
        cpu.carry_bit = True
    elif instruction == TCS:
        cpu.accumulator = int_to_binary(10, n_digits=4) if cpu.carry_bit else int_to_binary(9, n_digits=4)
        cpu.carry_bit = False
    elif instruction == ISZ:
        reg_n = binary_to_int(args[0])
        register_contents = add_binary(cpu.index_regs[reg_n], [False, False, False, True]).lower_bits
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
        cpu.memory.get_status_ram_chars()[3] = cpu.accumulator
    elif instruction == RDR:
        port = cpu.memory.get_romio_port()
        port_n = binary_to_int(cpu.memory.selected_addr[4:])
        cpu.accumulator = [line.status for line in port.lines]
        if port_n == 0 and cpu.index_regs[4] == int_to_binary(0, n_digits=4) and cpu.accumulator[0] == True:
            debug_log(f"[CPU] Received character separator")

    elif instruction == ADM:
        data_char = cpu.memory.get_data_ram_char().copy()
        added = add_binary(cpu.accumulator, data_char)
        cpu.accumulator = added.lower_bits
        cpu.carry_bit = added.carry
    elif instruction == WRM:
        cpu.memory.set_data_ram_char(cpu.accumulator)
    elif instruction == WRR:
        debug_log(f"[CPU] Writing {binary_to_int(cpu.accumulator)} to port {binary_to_int(cpu.memory.selected_addr[4:])}")
        cpu.memory.set_romio_port(cpu.accumulator)
        debug_log(f"[CPU] Finished writing to port")
    elif instruction == WMP:
        cpu.memory.set_ramo_port(cpu.accumulator)
    elif instruction == SBM:
        result = sub_binary(cpu.accumulator, cpu.memory.get_data_ram_char())
        cpu.accumulator = result.lower_bits
        cpu.carry_bit = result.carry
    elif instruction == WPM:
        high_4 = [line.status for line in cpu.memory.rom_ports[15].lines]
        low_8 = cpu.memory.selected_addr
        addr = binary_to_int(high_4 + low_8)
        nibble_start = cpu.memory.wpm_half_byte * 4
        nibble_end = nibble_start + 4

        if cpu.memory.program_ram_write_enable:
            cpu.memory.program_ram[addr][nibble_start:nibble_end] = cpu.accumulator.copy()
        else:
            cpu.accumulator = cpu.memory.program_ram[addr][nibble_start:nibble_end].copy()

        cpu.memory.wpm_half_byte = 0 if cpu.memory.wpm_half_byte == 1 else 1

    # Convenience Instructions: (do not exist, delete when done)
    elif instruction == STOP:
        exit(0)
    if not quiet:
        print("Done")


def turn_on(cpu: Intel4004):
    while True:
        single_step(cpu, quiet=True)
