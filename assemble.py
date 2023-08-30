import re
from textwrap import wrap
from typing import Self

import jsonpickle

from instructions import *
from binary_utils import *
from string import ascii_lowercase
from dataclasses import dataclass
import math
from enum import Enum

from log import debug_log


@dataclass(frozen=True, eq=True)
class ExprType:
    name: str
    n_bits: int

    def __str__(self):
        return self.name


class ExprTypes:
    REG = ExprType("register", 4)
    REG_PAIR = ExprType("register pair", 3)
    ADDR8 = ExprType("8-bit address", 8)
    ADDR12 = ExprType("12-bit address", 12)
    CHAR = ExprType("ASCII character", 8)

    @staticmethod
    def NUM(val: int):
        # not sure how to make this less bad
        n_bits = 0 if val == 0 else math.ceil(math.log2(val))
        return ExprType(f"{n_bits}-bit number", n_bits)


@dataclass
class Expression:
    as_int: int | None  # None means it's a placeholder
    type: ExprType

    def as_asm(self):
        if self.as_int is None and self.type == ExprTypes.REG:
            return "_"
        elif self.as_int is None:
            raise Exception("only REG can have placeholders for now (self.val should not be None)")
        assert self.as_int is not None
        if self.type == ExprTypes.REG:
            return str(self.as_int) + "R"
        elif self.type == ExprTypes.REG_PAIR:
            return str(self.as_int) + "P"
        elif self.type in (ExprTypes.ADDR8, ExprTypes.ADDR12):
            raise Exception("No standard way to represent addresses in 4004 assembly")
        elif self.type == ExprTypes.CHAR:
            return "'" + chr(self.as_int) + "'"
        elif self.type == ExprTypes.NUM(self.as_int):  # bleh
            return str(self.as_int)
        else:
            raise AssertionError(f"as_asm did not account for the type {self.type}; this should never happen")

    def __add__(self, other: Self) -> Self:
        ET = ExprTypes
        types = {self.type, other.type}

        if types in [{ET.ADDR8, ET.ADDR12}, {ET.ADDR8, ET.ADDR8}, {ET.ADDR12, ET.ADDR12}]:
            raise Exception("Adding 2 addresses is a bad idea")
        elif ET.REG in types or ET.REG_PAIR in types:
            raise Exception("Arithmetic on registers or register pairs is a bad idea")
        elif types == {ET.CHAR, ET.CHAR}:
            raise Exception("Adding 2 ASCII characters to each other is a bad idea")
        elif (ET.ADDR8 in types or ET.ADDR12 in types) and ET.CHAR in types:
            raise Exception("Adding an ASCII character to an address makes no sense")
        elif types in [{ET.ADDR12, ET.NUM(self.as_int)}, {ET.ADDR12, ET.NUM(other.as_int)}]:
            result_type = ET.ADDR12
        elif types in [{ET.ADDR8, ET.NUM(self.as_int)}, {ET.ADDR8, ET.NUM(other.as_int)}]:
            result_type = ET.ADDR8
        elif types in [{ET.CHAR, ET.NUM(self.as_int)}, {ET.CHAR, ET.NUM(other.as_int)}]:
            result_type = ET.CHAR
        elif types == {ET.NUM(self.as_int), ET.NUM(other.as_int)}:
            result_type = ET.NUM
        else:
            raise AssertionError(f"Expression.__add__ failed to account for the case {types}; this should never happen")

        return Expression(self.as_int + other.as_int, result_type)

    def __sub__(self, other: Self) -> Self:
        ET = ExprTypes

        match [self.type, other.type]:
            case [ET.REG, _] | [_, ET.REG] | [ET.REG_PAIR, _] | [_, ET.REG_PAIR]:
                raise Exception("Arithmetic on registers or register pairs is a bad idea")
            case [ET.ADDR8, _] | [_, ET.ADDR8]:
                raise Exception("Doing subtraction with 8-bit addresses is sketchy; this is an error for now")
            case [ET.ADDR12, ET.ADDR12 | ET.NUM(other.as_int)]:
                result_type = ET.ADDR12
            case [ET.ADDR12, _]:
                raise Exception(f"Subtracting a {other.type} from a 12-bit address is a bad idea")
            case [ET.CHAR, ET.NUM(other.as_int)]:
                result_type = ET.CHAR
            case [ET.CHAR, _]:
                raise Exception(f"Subtracting a {other.type} from an ASCII character makes no sense")
            case [ET.NUM(self.as_int), ET.NUM(other.as_int)]:
                result_type = ET.NUM(self.as_int - other.as_int)
            case [ET.NUM, _]:
                raise Exception(f"Subtracting a {other.type} from a number makes no sense")
            case _:
                raise AssertionError(
                    f"Expression.__add__ failed to account for the case {[self.type, other.type]}; this should never happen")

        if self.as_int - other.as_int < 0:
            raise Exception(f"The subtraction result of {self.as_int}-{other.as_int} is negative")
        return Expression(self.as_int - other.as_int, result_type)


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


def eval_assignment(line: str, pc: int, labels_to_values: dict[str, Expression]) -> tuple[str, Expression]:
    label = line.split(" ")[0]
    value = str_to_expr(line.split(" ")[2], pc, labels_to_values)
    return label, value


def get_labels_to_addrs(code: list[str]) -> dict[str, Expression]:
    labels_to_addrs: dict[str, Expression] = {}
    pseudo_pc = 0
    for line in code:
        if "=" in line.split(" "):
            continue
        elif "->" in line.split(" "):
            pseudo_pc += size_of_arrow_macro(line, pseudo_pc)
        elif "+=" in line.split(" "):
            pseudo_pc += size_of_increasement_macro(line, pseudo_pc)
        elif "-=" in line.split(" "):
            pseudo_pc += size_of_decreasement_macro(line, pseudo_pc)
        else:
            label, mnemonic, args = get_label_mnemonic_args(line)
            if label:
                if label in labels_to_addrs:
                    raise ValueError(f"Duplicate label {label}")
                labels_to_addrs[label] = Expression(pseudo_pc, ExprTypes.ADDR12)
            instruction = get_instr_by_mnemonic(mnemonic)
            pseudo_pc += instruction.n_bytes if instruction else 1
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
            raise ValueError("Invalid character in opcode specification")

    return out


def get_char_expr_ord(expr: str) -> int:
    if len(expr) < 3 or expr[0] != "'" or expr[-1] != "'":
        raise ValueError(
            f"Invalid character expression \"{expr}\". If you're trying to do a space character, it's '\\s'.")
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
        raise ValueError(f"Unrecognized escape sequence \"{inner_char}\" in character expression")
    elif len(inner_char) > 1:
        raise ValueError(f"Character expression \"{expr}\" has more than one character")
    else:
        return ord(inner_char)


class LabelNotFound(Exception):
    pass


def str_to_expr(arg: str, pc: int, labels_to_values: dict[str, Expression] | None = None) -> Expression:
    if "+" in arg or "-" in arg:
        term1 = str_to_expr(re.split(r"[+-]", arg)[0], pc, labels_to_values)
        term2 = str_to_expr(re.split(r"[+-]", arg)[1], pc, labels_to_values)
        res = term1 + term2 if "+" in arg else term1 - term2
        return res
    elif arg == "*":
        return Expression(pc, ExprTypes.ADDR12)
    elif arg == "_":
        return Expression(None, ExprTypes.REG)
    elif arg[0] == "'":
        return Expression(get_char_expr_ord(arg), ExprTypes.CHAR)
    elif arg[0] not in "1234567890" and labels_to_values:
        return labels_to_values[arg]
    elif arg[0] not in "1234567890":
        raise LabelNotFound(
            f"Invalid expression \"{arg}\" (might be a label, but function was called without labels_to_values)")
    elif arg[-1] == "B":
        as_int = int(arg[:-1], 2)
        return Expression(as_int, ExprTypes.NUM(as_int))
    elif arg[-1] == "H":
        as_int = int(arg[:-1], 16)
        return Expression(as_int, ExprTypes.NUM(as_int))
    elif arg[-1] == "R":
        return Expression(int(arg[:-1]), ExprTypes.REG)
    elif arg[-1] == "P":
        return Expression(int(arg[:-1]), ExprTypes.REG_PAIR)
    else:
        return Expression(int(arg), ExprTypes.NUM(int(arg)))


def assemble_line(line: str, pseudo_pc: int, labels_to_values: dict[str, Expression] | None = None) -> str:
    out = ""

    label, mnemonic, args = get_label_mnemonic_args(line)
    instruction = get_instr_by_mnemonic(mnemonic)
    if not instruction:
        return binary_to_string(int_to_binary(str_to_expr(mnemonic, pseudo_pc).as_int, n_digits=8))
    if instruction == SRC:
        pass
    opcode_parsed = parse_opcode(instruction.opcode)
    int_args = [str_to_expr(arg, pseudo_pc, labels_to_values).as_int for arg in args]

    if "a" * 8 in opcode_parsed:
        pc_after_instr = int_to_binary(pseudo_pc + 2, n_digits=12)
        jump_to = int_to_binary(int_args[1], n_digits=12)
        if pc_after_instr[:4] != jump_to[:4]:
            raise Exception(
                f"8-bit jump across a page boundary; instruction ({line}) is on address {pseudo_pc} (will jump to page {binary_to_int(pc_after_instr[:4])}) and is trying to jump to address {int_args[1]} (page {binary_to_int(jump_to[:4])})")

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


def size_of_arrow_macro(line: str, pc: int) -> int:
    result = 0
    for expr in split_into_expr_lists(line, "->", pc)[1]:
        if expr.as_int is None:
            pass  # placeholders won't add extra instructions
        elif expr.type == ExprTypes.REG:
            result += 2  # LDM, XCH
        elif expr.type == ExprTypes.REG_PAIR:
            result += 4  # LDM, XCH, LDM, XCH
    return result


def size_of_increasement_macro(line: str, pc: int) -> int:
    to_increase, _ = split_into_expr_lists(line, "+=", pc)
    to_increase_category, _ = analyze_exprs(to_increase)
    assert to_increase_category == ExprCategory.REGS, "Invalid expression category"
    return 3 * len(convert_to_regs(to_increase)) + 1  # CLC at the start, then LDM, ADD, XCH for each register


def size_of_decreasement_macro(line: str, pc: int) -> int:
    to_decrease, _ = split_into_expr_lists(line, "-=", pc)
    to_decrease_category, _ = analyze_exprs(to_decrease)
    assert to_decrease_category == ExprCategory.REGS, "Invalid expression category"
    return 5 * len(convert_to_regs(to_decrease)) + 1 - 1  # CLC at the start, then LDM, XCH, SUB, XCH, CMC for each register, except no CMC at the end


def split_into_expr_lists(line: str, delimeter: str, pc: int, labels_to_values: dict[str, Expression] | None = None) -> \
tuple[list[Expression], list[Expression]]:
    # worst thing I have ever written
    try:
        first = [str_to_expr(expr_str, pc, labels_to_values) for expr_str in
                 line.split(delimeter)[0].strip().split(" ")]
    except LabelNotFound:
        first = None
    try:
        second = [str_to_expr(expr_str, pc, labels_to_values) for expr_str in
                  line.split(delimeter)[1].strip().split(" ")]
    except LabelNotFound:
        second = None
    return first, second


def expand_increasement_macro(line: str, pc: int, labels_to_values: dict[str, Expression]) -> list[str]:
    to_increase, increase_by = split_into_expr_lists(line, "+=", pc, labels_to_values)
    to_increase_category, _ = analyze_exprs(to_increase)
    increase_by_category, increment_value = analyze_exprs(increase_by)
    if to_increase_category != ExprCategory.REGS or increase_by_category != ExprCategory.NUM or increment_value is None:
        raise Exception(f"Invalid expression categories for {line}")
    regs_to_increase = convert_to_regs(to_increase)

    assembly_instructions = []
    increment_binary = format(increment_value, f'0{4 * len(to_increase)}b').zfill(4 * len(regs_to_increase))
    assembly_instructions.append("CLC")
    for reg, nibble in zip(reversed(regs_to_increase), reversed(wrap(increment_binary, 4))):
        assembly_instructions.append(f"LDM {nibble}B")
        assembly_instructions.append(f"ADD {reg}R")
        assembly_instructions.append(f"XCH {reg}R")

    return assembly_instructions


def expand_decreasement_macro(line: str, pc: int, labels_to_values: dict[str, Expression]) -> list[str]:
    to_increase, increase_by = split_into_expr_lists(line, "-=", pc, labels_to_values)
    to_increase_category, _ = analyze_exprs(to_increase)
    increase_by_category, increment_value = analyze_exprs(increase_by)
    if to_increase_category != ExprCategory.REGS or increase_by_category != ExprCategory.NUM or increment_value is None:
        raise Exception(f"Invalid expression categories for {line}")
    regs_to_increase = convert_to_regs(to_increase)

    assembly_instructions = []
    increment_binary = format(increment_value, f'0{4 * len(to_increase)}b').zfill(4 * len(regs_to_increase))
    assembly_instructions.append("CLC")
    for reg, nibble in zip(reversed(regs_to_increase), reversed(wrap(increment_binary, 4))):
        assembly_instructions.append(f"LDM {nibble}B")
        assembly_instructions.append(f"XCH {reg}R")
        assembly_instructions.append(f"SUB {reg}R")
        assembly_instructions.append(f"XCH {reg}R")
        assembly_instructions.append("CMC")

    return assembly_instructions[:-1]


class ExprCategory(Enum):
    REGS = "Registers and/or Register Pairs"
    NUM = "Single Number"
    OTHER = "Other"


def analyze_exprs(exprs: list[Expression]) -> tuple[ExprCategory, int | None]:
    if len(exprs) == 1 and exprs[0].type not in [ExprTypes.REG, ExprTypes.REG_PAIR]:
        return ExprCategory.NUM, exprs[0].as_int
    elif all(expr.type in [ExprTypes.REG, ExprTypes.REG_PAIR] for expr in exprs):
        return ExprCategory.REGS, None
    else:
        return ExprCategory.OTHER, None


def convert_to_regs(exprs: list[Expression]) -> list[int | None]:
    regs: list[int | None] = []
    for expr in exprs:
        if expr.type == ExprTypes.REG_PAIR:
            regs.append(expr.as_int * 2)
            regs.append(expr.as_int * 2 + 1)
        elif expr.type == ExprTypes.REG:
            regs.append(expr.as_int)
        else:
            raise ValueError("Expression must represent registers or register pairs")
    return regs


def expand_num_to_regs(in_num: int, out_regs: list[int]) -> list[str]:
    result = []
    num_bin = int_to_binary(in_num, n_digits=len(out_regs) * 4)
    in_nibbles = wrap(binary_to_string(num_bin), 4)
    for nibble, reg in zip(in_nibbles, out_regs):
        if reg is not None:
            result.append(f"LDM {nibble}B")
            result.append(f"XCH {reg}R")
    return result


def expand_regs_to_regs(in_regs: list[int | None], out_regs: list[int]) -> list[str]:
    result = []
    assert len(in_regs) == len(in_regs), "In and out sizes don't match"
    for in_reg, out_reg in zip(in_regs, out_regs):
        if out_reg is not None:
            result.append(f"LD {in_reg}R")
            result.append(f"XCH {out_reg}R")
    return result


def expand_arrow_macro(line: str, pc: int, labels_to_values: dict[str, Expression]) -> list[str]:
    in_exprs, out_exprs = split_into_expr_lists(line, "->", pc, labels_to_values)
    in_type, in_num = analyze_exprs(in_exprs)
    out_type, _ = analyze_exprs(out_exprs)

    if in_type == ExprCategory.OTHER or out_type != ExprCategory.REGS:
        raise ValueError(f"Invalid expressions in arrow macro \"{line}\" (in_type={in_type}, out_type={out_type})")

    out_regs = convert_to_regs(out_exprs)

    if in_type == ExprCategory.NUM:
        return expand_num_to_regs(in_num, out_regs)
    elif in_type == ExprCategory.REGS:
        in_regs = convert_to_regs(in_exprs)
        return expand_regs_to_regs(in_regs, out_regs)
    else:
        raise Exception("Unexpected expression type; this should never happen")


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

    labels_to_values = get_labels_to_addrs(code_calls_expanded)
    debug_file = open("debug.json", "w")
    debug_file.write(jsonpickle.dumps({l: v.as_int for l, v in labels_to_values.items()}))
    debug_file.close()

    debug_log(f"[ASSEMBLER] Address labels: {labels_to_values}")
    out = ""

    for line in code_calls_expanded:
        if "=" in line.split(" "):
            label, value = eval_assignment(line, len(out) // 8, labels_to_values)
            labels_to_values[label] = value
        elif "->" in line.split(" "):
            macro_expanded = expand_arrow_macro(line, len(out) // 8, labels_to_values)
            for expanded_line in macro_expanded:
                out += assemble_line(expanded_line, len(out) // 8)
        elif "+=" in line.split(" "):
            macro_expanded = expand_increasement_macro(line, len(out) // 8, labels_to_values)
            for expanded_line in macro_expanded:
                out += assemble_line(expanded_line, len(out) // 8)
        elif "-=" in line.split(" "):
            macro_expanded = expand_decreasement_macro(line, len(out) // 8, labels_to_values)
            for expanded_line in macro_expanded:
                out += assemble_line(expanded_line, len(out) // 8)
        else:
            out += assemble_line(line, len(out) // 8, labels_to_values)
    out_bytes = bytearray(int(out[i: i + 8], 2) for i in range(0, len(out), 8))

    return out_bytes
