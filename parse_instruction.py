from instructions import *
from binary_utils import *
from string import ascii_lowercase


def get_instr(prgm_cntr: list[bool], prgm_mem: list[bool]) -> Instruction:
    int_pc = binary_to_int(prgm_cntr)
    first_8 = binary_to_string(prgm_mem[int_pc * 8: int_pc * 8 + 8])
    instruction = None
    for instr in instructions:
        matches = True
        for op_bit, code_bit in zip(instr.opcode[:8], first_8):
            if op_bit in ("0", "1") and op_bit != code_bit:
                matches = False
        if matches:
            instruction = instr
            break
    if not instruction:
        raise Exception(f"Could not find any instructions matching the bits {first_8}")
    return instruction


def get_args(instruction: Instruction, prgm_cntr: list[bool], prgm_mem: list[bool]) -> list[list[bool]]:
    args: list[list[bool]] = []
    arg: list[bool] = []
    i = 0

    while i < len(instruction.opcode):
        bit_n = binary_to_int(prgm_cntr) * 8 + i
        if instruction.opcode[i] in ascii_lowercase and i != 0 and instruction.opcode[i] != instruction.opcode[i - 1] and len(arg) > 0:
            args.append(arg)
            arg = [prgm_mem[bit_n]]
        elif instruction.opcode[i] in ascii_lowercase:
            arg.append(prgm_mem[bit_n])
        i += 1

    if len(arg) > 0:
        args.append(arg)

    return args


def addr_of_next_instr(prgm_cntr: list[bool], current_instr: Instruction) -> list[bool]:
    return add_binary(prgm_cntr, int_to_binary(current_instr.n_bytes, n_digits=12), False).lower_bits
