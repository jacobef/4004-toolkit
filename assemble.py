import re
from textwrap import wrap

import jsonpickle

from instructions import *
from binary_utils import *
from string import ascii_lowercase
from enum import Enum, auto

from log import debug_log


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
            raise Exception("Invalid Label: begins with int instead of char")
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


def is_assignment(line: str) -> bool:
    line_split = line.split(" ")
    return len(line_split) == 3 and line_split[1] == "="


def eval_assignment(line: str, pc: int, labels_to_values: dict[str, int]) -> (str, int):
    label = line.split(" ")[0]
    value = expr_as_int(line.split(" ")[2], pc, labels_to_values)
    return label, value


def get_labels_to_values(code: list[str]) -> dict[str, int]:
    labels_to_values = {}
    pseudo_pc = 0
    for line in code:
        if is_assignment(line):
            continue
        elif "->" in line.split(" "):
            macro_expanded = expand_arrow_macro(line, pseudo_pc)
            for expanded_line in macro_expanded:
                pseudo_pc += len(assemble_line(expanded_line, pseudo_pc)) // 8
        else:
            label, mnemonic, args = get_label_mnemonic_args(line)
            if label:
                labels_to_values[label] = pseudo_pc
            instruction = get_instr_by_mnemonic(mnemonic)
            pseudo_pc += instruction.n_bytes if instruction else 1
    return labels_to_values


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
            raise ValueError("Invalid character in opcode specification")

    return out


def get_char_expr_ord(expr: str) -> int:
    if len(expr) < 3 or expr[0] != "'" or expr[-1] != "'":
        raise ValueError(
            f"Invalid character expression \"{expr}\". If you're trying to do a space character, it's '\s'.")
    inner_char = expr[1:-1]
    if inner_char == r"\a":
        return ord("\a")
    elif inner_char == r"\d":
        return 127  # ASCII DEL character
    elif inner_char == r"\n":
        return ord("\n")
    elif inner_char == r"\s":
        return ord(" ")
    elif inner_char == r"\t":
        return ord("\t")
    elif inner_char[0] == "\\" and len(inner_char) > 1:
        raise ValueError(f"Unrecognizedescape sequence \"{inner_char}\" in character expression")
    elif len(inner_char) > 1:
        raise ValueError(f"Character expression \"{expr}\" has more than one character")
    else:
        return ord(inner_char)


def expr_as_int(arg: str, pc: int, labels_to_values: dict[str, int] | None = None) -> int:
    if "+" in arg or "-" in arg:
        term1 = expr_as_int(re.split(r"[+-]", arg)[0], pc, labels_to_values)
        term2 = expr_as_int(re.split(r"[+-]", arg)[1], pc, labels_to_values)
        res = term1 + term2 if "+" in arg else term1 - term2
        return res
    elif arg == "*":
        return pc
    elif arg[0] == "'":
        return get_char_expr_ord(arg)
    elif arg[0] not in "1234567890" and labels_to_values:
        return labels_to_values[arg]
    elif arg[0] not in "1234567890":
        raise ValueError(
            f"Invalid expression \"{arg}\" (might be a label, but function was called without labels_to_values)")
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


def assemble_line(line: str, pseudo_pc: int, labels_to_values: dict[str, int] | None = None) -> str:
    out = ""

    label, mnemonic, args = get_label_mnemonic_args(line)
    instruction = get_instr_by_mnemonic(mnemonic)
    if not instruction:
        return binary_to_string(int_to_binary(expr_as_int(mnemonic, pseudo_pc), n_digits=8))

    opcode_parsed = parse_opcode(instruction.opcode)
    int_args = [expr_as_int(arg, pseudo_pc, labels_to_values) for arg in args]

    if "a" * 8 in opcode_parsed:
        pc_after_instr = int_to_binary(pseudo_pc+2, n_digits=12)
        jump_to = int_to_binary(int_args[1], n_digits=12)
        if pc_after_instr[:4] != jump_to[:4]:
            raise Exception(
                f"8-bit jump across a page boundary; instruction is on address {pseudo_pc} (will jump to page {binary_to_int(pc_after_instr[:4])}) and is trying to jump to address {int_args[1]} (page {binary_to_int(jump_to[:4])})")

    arg_i = 0
    for item in opcode_parsed:
        if item in ("0", "1"):
            out += item
        else:
            arg_expected_n_bits = len(item)
            arg_as_string = format(int_args[arg_i], f"0{arg_expected_n_bits}b")

            if len(arg_as_string) >= arg_expected_n_bits:
                out += arg_as_string[len(arg_as_string) - arg_expected_n_bits:]
            else:
                raise RuntimeError(
                    f"(THIS SHOULD NEVER HAPPEN) Argument {arg_i} to {instruction.mnemonic} has too few bits; expected {arg_expected_n_bits}, got {len(arg_as_string)}")
            arg_i += 1

    return out


def n_nibbles(exprs: list[str]):
    res = 0
    for expr in exprs:
        if expr[-1] == "R":
            res += 1
        elif expr[-1] == "P":
            res += 2
    return res


def expand_arrow_macro(line: str, pc: int) -> list[str]:
    in_strs = []
    out_strs = []
    for expr_str in line.split("->")[0].strip().split(" "):
        if expr_str[-1] == "P":
            in_strs.append(f"{expr_as_int(expr_str, pc)*2}R")
            in_strs.append(f"{expr_as_int(expr_str, pc)*2+1}R")
        elif "*" in expr_str:
            in_strs.append(str(expr_as_int(expr_str, pc)))
        else:
            in_strs.append(expr_str)
    for expr_str in line.split("->")[1].strip().split(" "):
        if expr_str[-1] == "P":
            out_strs.append(f"{expr_as_int(expr_str, pc)*2}R")
            out_strs.append(f"{expr_as_int(expr_str, pc)*2+1}R")
        else:
            out_strs.append(expr_str)

    assert all(expr not in out_strs for expr in in_strs), "In and out can't overlap"
    result = []
    if in_strs[0][-1] in "1234567890":
        assert len(in_strs) == 1, "In number must stand alone"
        in_num = int_to_binary(int(in_strs[0]), n_digits=len(out_strs)*4)
        in_nibbles = wrap(binary_to_string(in_num), 4)
        for nibble, reg in zip(in_nibbles, out_strs):
            if reg != "_":
                result.append(f"LDM {nibble}B")
                result.append(f"XCH {reg}")
    else:
        assert len(in_strs) == len(out_strs), "In and out sizes don't match"
        for in_reg, out_reg in zip(in_strs, out_strs):
            if out_reg != "_":
                result.append(f"LD {in_reg}")
                result.append(f"XCH {out_reg}")
    return result


def assemble(filename: str) -> bytearray:
    with open(f"{filename}.4004", "r") as file:
        code = file.read().split("\n")

    code_cleaned = [remove_comment(line).strip() for line in code if remove_comment(line).strip()]
    code_calls_expanded = []
    for line in code_cleaned:
        if len(line.split(" ")) == 2 and line.split(" ")[0] == "CALL":
            for line2 in f"""

/ Push the first nibble of the return address
*+14 -> 8R _ _
JMS P8R
/ Push the second nibble
*+10 -> _ 8R _
JMS P8R
/ Push the third nibble
*+6 -> _ _ 8R
JMS P8R
/ Jump to the function
JUN {line.split(" ")[1]}
""".split("\n"):
                if remove_comment(line2).strip():
                    code_calls_expanded.append(remove_comment(line2).strip())
        else:
            code_calls_expanded.append(line)

    labels_to_values = get_labels_to_values(code_calls_expanded)
    debug_file = open("debug.json", "w")
    debug_file.write(jsonpickle.dumps(labels_to_values))
    debug_file.close()

    debug_log(f"[ASSEMBLER] address labels: {labels_to_values}")
    out = ""

    for line in code_calls_expanded:
        if is_assignment(line):
            label, value = eval_assignment(line, len(out) // 8, labels_to_values)
            labels_to_values[label] = value
        elif "->" in line.split(" "):
            macro_expanded = expand_arrow_macro(line, len(out) // 8)
            for expanded_line in macro_expanded:
                out += assemble_line(expanded_line, len(out) // 8)
        else:
            out += assemble_line(line, len(out) // 8, labels_to_values)
    out_bytes = bytearray(int(out[i: i + 8], 2) for i in range(0, len(out), 8))

    return out_bytes
