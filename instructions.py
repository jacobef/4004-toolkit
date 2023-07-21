from dataclasses import dataclass
from enum import Enum, auto


@dataclass(frozen=True)
class Instruction:
    opcode: str
    mnemonic: str

    def __post_init__(self):
        if len(self.opcode) not in (8, 16):
            raise Exception("Instructions must be exactly 8 or 16 bits")

    @property
    def n_bytes(self) -> int:
        return len(self.opcode) // 8

    class ArgType(Enum):
        REGISTER = auto()
        REGISTER_PAIR = auto()
        NUMBER = auto()
        ADDRESS = auto()
        CONDITION = auto()

    @property
    def arg_types(self):
        letters_to_types = {'r': self.ArgType.REGISTER, 'p': self.ArgType.REGISTER_PAIR, 'n': self.ArgType.NUMBER,
                            'a': self.ArgType.ADDRESS, 'c': self.ArgType.CONDITION}
        result_redundant = []
        for bit in self.opcode:
            if bit in letters_to_types:
                result_redundant.append(letters_to_types[bit])
        result = []
        last_type = None
        for typ in result_redundant:
            if not last_type or last_type != typ:
                result.append(typ)
            last_type = typ
        return result


NOP = Instruction("00000000", "NOP")
INC = Instruction("0110rrrr", "INC")
ADD = Instruction("1000rrrr", "ADD")
SUB = Instruction("1001rrrr", "SUB")
LD = Instruction("1010rrrr", "LD")
XCH = Instruction("1011rrrr", "XCH")
LDM = Instruction("1101nnnn", "LDM")
IAC = Instruction("11110010", "IAC")
DAC = Instruction("11111000", "DAC")
CLB = Instruction("11110000", "CLB")
CLC = Instruction("11110001", "CLC")
TCC = Instruction("11110111", "TCC")
CMC = Instruction("11110011", "CMC")
BBL = Instruction("1100nnnn", "BBL")
JMS = Instruction("0101aaaaaaaaaaaa", "JMS")
JUN = Instruction("0100aaaaaaaaaaaa", "JUN")
JIN = Instruction("0011ppp1", "JIN")
FIM = Instruction("0010ppp0nnnnnnnn", "FIM")
FIN = Instruction("0011ppp0", "FIN")
JCN = Instruction("0001ccccaaaaaaaa", "JCN")
DAA = Instruction("11111011", "DAA")
CMA = Instruction("11110100", "CMA")
RAL = Instruction("11110101", "RAL")
RAR = Instruction("11110110", "RAR")
KBP = Instruction("11111100", "KBP")
STC = Instruction("11111010", "STC")
TCS = Instruction("11111001", "TCS")
ISZ = Instruction("0111rrrraaaaaaaa", "ISZ")
DCL = Instruction("11111101", "DCL")
SRC = Instruction("0010ppp1", "SRC")
RDM = Instruction("11101001", "RDM")
RD0 = Instruction("11101100", "RD0")
RD1 = Instruction("11101101", "RD1")
RD2 = Instruction("11101110", "RD2")
RD3 = Instruction("11101111", "RD3")
WR0 = Instruction("11100100", "WR0")
WR1 = Instruction("11100101", "WR1")
WR2 = Instruction("11100110", "WR2")
WR3 = Instruction("11100111", "WR3")
RDR = Instruction("11101010", "RDR")
ADM = Instruction("11101011", "ADM")
WRM = Instruction("11100000", "WRM")
WRR = Instruction("11100010", "WRR")
WMP = Instruction("11100001", "WMP")
SBM = Instruction("11101000", "SBM")
WPM = Instruction("11100011", "WPM")
# WPM time to defeat the final boss...
# TODO:
# Add suport for ROM/RAM prgm mem.
# Convenient Instructions: (do not exist, delete when done)
STOP = Instruction("11111111", "STOP")

instructions = (
NOP, INC, ADD, SUB, LD, XCH, LDM, IAC, DAC, CLB, CLC, TCC, CMC, BBL, JMS, JUN, JIN, FIM, FIN, JCN, DAA, CMA, RAL, RAR,
KBP, STC, TCS, ISZ, DCL, SRC, RDM, RD0, RD1, RD2, RD3, WR0, WR1, WR2, WR3, RDR, ADM, WRM, WRR, WMP, SBM, WPM, STOP)


def get_instr_by_mnemonic(mnemonic: str) -> Instruction | None:
    for instr in instructions:
        if instr.mnemonic == mnemonic:
            return instr
    return None
