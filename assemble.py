from instructions import *
from binary_utils import *
from string import ascii_lowercase
from enum import Enum, auto


class GivenArgType(Enum):
    REGISTER = auto()
    REGISTER_PAIR = auto()
    DECIMAL = auto()
    BINARY = auto()
    HEX = auto()
    LABEL = auto()
    CONDITION = auto()


def remove_comment(line: str):
    comment_start_index = len(line)
    for i in range(len(line)):
        if line[i] == "/":
            comment_start_index = i
            break
    return line[:comment_start_index]


def get_label(line: str) -> str | None:
    possible_label = line.split(" ")[0]
    if possible_label[-1] == ",":
        label = possible_label[:-1]
        if label[0] in "1234567890":
            raise Exception("Invalid Label: begins with int insted of char")
        for i in label:
            if not i.isalnum():
                raise Exception(f"Invalid Label: contains {i} when only alphanumeric chars are allowed.")
        return label
    else:
        return None


def get_label_mnemonic_args(line: str) -> (str | None, str, str):
    label = get_label(line)
    if label:
        mnemonic = line.split(" ")[1]
        args = line.split(" ")[2:]
    else:
        mnemonic = line.split(" ")[0]
        args = line.split(" ")[1:]
    return label, mnemonic, args


def get_labels_to_addrs(code: list[str]) -> dict[str, int]:
    labels_to_addrs = {}
    pseudo_pc = 0
    for line in code:
        label, mnemonic, args = get_label_mnemonic_args(line)
        if label:
            labels_to_addrs[label] = pseudo_pc
        instruction = get_instr_by_mnemonic(mnemonic)
        pseudo_pc += instruction.n_bytes
    return labels_to_addrs


def parse_opcode(opcode: str) -> list[str]:
    out = []
    i = 0

    while i < len(opcode):
        c = opcode[i]

        if c in ascii_lowercase:
            placeholder = ""
            while i < len(opcode) and opcode[i] == c:
                placeholder += opcode[i]
                i += 1
            out.append(placeholder)
        elif c == "0":
            out.append("0")
            i += 1
        elif c == "1":
            out.append("1")
            i += 1
        else:
            raise ValueError("invalid character in opcode specification")

    return out


def arg_as_int(arg: str, labels_to_addrs: dict[str, int]) -> int:
    if arg[0] not in "1234567890":
        return labels_to_addrs[arg]
    elif arg[-1] == "B":  # binary
        return int(arg[:-1], 2)
    elif arg[-1] == "H":  # hexadecimal
        return int(arg[:-1], 16)
    elif arg[-1] == "R":  # register
        return int(arg[:-1])
    elif arg[-1] == "P":  # register pair
        return int(arg[:-1])
    else:
        return int(arg)


class ArgCompatibility(Enum):
    OK = auto()
    WARN = auto()
    ERR = auto()


def get_arg_compatibility(given: GivenArgType, expected: Instruction.ArgType) -> ArgCompatibility:
    GAT = GivenArgType
    EAT = Instruction.ArgType  # EAT = expected arg type
    match (given, expected):
        case (GAT.DECIMAL | GAT.BINARY | GAT.HEX, EAT.NUMBER):
            return ArgCompatibility.OK
        case (_, EAT.NUMBER):
            return ArgCompatibility.ERR
        case (GAT.REGISTER, EAT.REGISTER) | (GAT.REGISTER_PAIR, EAT.REGISTER_PAIR):
            return ArgCompatibility.OK
        case (GAT.LABEL, EAT.ADDRESS):
            return ArgCompatibility.OK
        case (GAT.DECIMAL | GAT.BINARY | GAT.HEX, EAT.REGISTER | EAT.REGISTER_PAIR | EAT.ADDRESS):
            return ArgCompatibility.WARN
        case (_, EAT.REGISTER | EAT.REGISTER_PAIR | EAT.ADDRESS):
            return ArgCompatibility.ERR
        case (GAT.BINARY, EAT.CONDITION):
            return ArgCompatibility.OK
        case (GAT.DECIMAL | GAT.HEX, EAT.CONDITION):
            return ArgCompatibility.WARN
        case (_, EAT.CONDITION):
            return ArgCompatibility.ERR
        case _:
            raise Exception("get_arg_compatibility failed to account for this case; this should never happen")


def assemble(filename: str) -> bytearray:
    with open(f"{filename}.4004", "r") as file:
        code = file.read().split("\n")

    code_cleaned = [remove_comment(line).strip() for line in code if remove_comment(line).strip()]
    labels_to_addrs = get_labels_to_addrs(code_cleaned)
    out = ""

    for line in code_cleaned:
        _, mnemonic, args = get_label_mnemonic_args(line)
        instruction = get_instr_by_mnemonic(mnemonic)
        opcode_parsed = parse_opcode(instruction.opcode)

        int_args = [arg_as_int(arg, labels_to_addrs) for arg in args]
        arg_i = 0

        for item in opcode_parsed:
            if item in ("0", "1"):
                out += item
            else:
                arg_expected_n_bits = len(item)
                arg_as_string = format(int_args[arg_i], f"0{arg_expected_n_bits}b")

                if len(arg_as_string) > arg_expected_n_bits:
                    raise ValueError(
                        f"Argument {arg_i} to {instruction.mnemonic} has too many bits; expected {arg_expected_n_bits}, got {len(arg_as_string)}"
                    )

                out += arg_as_string
                arg_i += 1

    out_bytes = bytearray(int(out[i: i + 8], 2) for i in range(0, len(out), 8))

    return out_bytes
