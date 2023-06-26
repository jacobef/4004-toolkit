from enum import Enum, auto
from typing import Literal
from dataclasses import dataclass

from binary_utils import *


class InputOrOutput(Enum):
    INPUT = auto()
    OUTPUT = auto()


IO = InputOrOutput


@dataclass
class IOLine:
    io: InputOrOutput
    status: bool


class ROMPort:
    def __init__(self, line_io: list[InputOrOutput]):
        assert (len(line_io) == 4)
        self.lines: list[IOLine] = [IOLine(io, False) for io in line_io]

    def __str__(self):
        out = ""
        for line in self.lines:
            out += binary_to_string([line.status]) + ("i" if line.io == IO.INPUT else "o")
        return out

    def __repr__(self):
        return self.__str__()


class RAMRegister:
    def __init__(self):
        self.data_chars: list[list[bool]] = []
        for _ in range(16):
            self.data_chars.append([False, False, False, False])
        self.status_chars: list[list[bool]] = []
        for _ in range(4):
            self.status_chars.append([False, False, False, False])

    def __str__(self):
        out = ""
        out += "data: "
        for dchr in self.data_chars:
            out += binary_to_string(dchr) + ", "
        out += "status: "
        for schr in self.status_chars:
            out += binary_to_string(schr) + ", "
        return out[:-2]

    def __repr__(self):
        return self.__str__()


class Memory:
    def __init__(self, rom_port_spec: list[list[InputOrOutput]]):
        self.ram_banks: list[list[list[RAMRegister]]] = []
        for _ in range(8):
            bank = []
            for _ in range(4):
                chip = []
                for _ in range(4):
                    chip.append(RAMRegister())
                bank.append(chip)
            self.ram_banks.append(bank)
        self.selected_bank: int = 0
        self.selected_addr: list[bool] = [False] * 8

        self.program_ram: list[list[bool]] = []
        self.program_ram_write_enable: bool = False
        self.wpm_half_byte: Literal[0, 1] = 0
        for _ in range(4096):
            self.program_ram.append([False] * 8)

        self.rom: list[bool] = [False for _ in range(0, 32768)]
        self.rom_ports: list[ROMPort] = []
        for i in range(16):
            self.rom_ports.append(ROMPort(rom_port_spec[i]))

        self.ram_ports: list[list[bool]] = []
        for _ in range(4):
            self.ram_ports.append([False, False, False, False])

    def get_data_ram_char(self) -> list[bool]:
        chip_n = binary_to_int(self.selected_addr[0:2])
        register_n = binary_to_int(self.selected_addr[2:4])
        char_n = binary_to_int(self.selected_addr[4:8])
        return self.ram_banks[self.selected_bank][chip_n][register_n].data_chars[char_n]

    def set_data_ram_char(self, setval: list[bool]) -> None:
        chip_n = binary_to_int(self.selected_addr[0:2])
        register_n = binary_to_int(self.selected_addr[2:4])
        char_n = binary_to_int(self.selected_addr[4:8])
        self.ram_banks[self.selected_bank][chip_n][register_n].data_chars[char_n] = setval.copy()

    def get_status_ram_chars(self) -> list[list[bool]]:
        chip_n = binary_to_int(self.selected_addr[0:2])
        register_n = binary_to_int(self.selected_addr[2:4])
        return self.ram_banks[self.selected_bank][chip_n][register_n].status_chars

    def get_romio_port(self) -> ROMPort:
        port_n = binary_to_int(self.selected_addr[4:])
        return self.rom_ports[port_n]

    def set_romio_port(self, set_to: list[bool]) -> None:
        port_n = binary_to_int(self.selected_addr[4:])
        if port_n == 14 and binary_to_int(set_to) == 1:
            self.program_ram_write_enable = True
        elif port_n == 14 and binary_to_int(set_to) == 0:
            self.program_ram_write_enable = False
        n = 0
        for i in set_to:
            self.rom_ports[port_n].lines[n].status = i
            n += 1

    def set_ramo_port(self, set_to: list[bool]) -> None:
        port_n = binary_to_int(self.selected_addr[:2])
        self.ram_ports[port_n] = set_to

    def __str__(self):
        out = ""
        out += "Banks:" + str(self.ram_banks) + "\n"
        out += "ROM ports:" + str(self.rom_ports) + "\n"
        out += "RAM ports:" + str([binary_to_string(port) for port in self.ram_ports]) + "\n"
        out += "selected bank: " + str(self.selected_bank) + "\n"
        out += "selected address: " + binary_to_string(self.selected_addr)
        return out


class Intel4004:
    class Stack:
        def __init__(self):
            self.regs: list[list[bool]] = [[], [], []]
            for i in range(3):
                for _ in range(12):
                    self.regs[i].append(False)
            self.pointer: int = 0

        def push(self, addr: list[bool]):
            if len(addr) != 12:
                raise Exception("Addresses pushed to the stack must be 12 bits")
            self.regs[self.pointer] = addr.copy()
            if self.pointer == 2:
                self.pointer = 0
            else:
                self.pointer += 1

        def pop(self) -> list[bool]:
            if self.pointer == 0:
                self.pointer = 2
            else:
                self.pointer -= 1
            return self.regs[self.pointer].copy()

    def __init__(self, rom_port_spec: list[list[InputOrOutput]]):
        self.memory = Memory(rom_port_spec)
        self.stack = self.Stack()
        self.index_regs: list[list[bool]] = []
        for _ in range(16):
            self.index_regs.append([False, False, False, False])
        self.prgm_cntr: list[bool] = [False for _ in range(12)]
        self.accumulator: list[bool] = [False for _ in range(4)]
        self.carry_bit: bool = False
        self.test_signal: bool = False

    def get_register_pair(self, pair_n: list[bool]) -> list[bool]:
        return self.index_regs[binary_to_int(pair_n) * 2] + self.index_regs[binary_to_int(pair_n) * 2 + 1]

    def set_register_pair(self, pair_n: list[bool], set_to: list[bool]) -> None:
        assert len(set_to) == 8, "Register pairs can only be set to 8 bit values"
        self.index_regs[binary_to_int(pair_n) * 2] = set_to[:4]
        self.index_regs[binary_to_int(pair_n) * 2 + 1] = set_to[4:]

    def __str__(self):
        out = ""
        out += "Registers:\n"
        for i in range(len(self.index_regs)):
            out += f"R{i}={binary_to_int(self.index_regs[i])}, "
        out += "\n"
        out += "Stack:\n"
        for i in range(len(self.stack.regs)):
            out += str(binary_to_int(self.stack.regs[i]))
            if i == self.stack.pointer:
                out += " <-"
            out += "\n"
        out += "Accumulator: " + str(binary_to_int(self.accumulator)) + "\n"
        out += "Accumulator as binary: " + binary_to_string(self.accumulator) + "\n"
        out += "Carry bit: " + str(self.carry_bit) + "\n"
        # out += "RAM: " + str(self.ram) + "\n"
        return out
