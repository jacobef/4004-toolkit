from cpu import *
from parse_instruction import *
from colorama import Fore, Style


# register pair: bold yellow, Register: yellow regular
# adress: blue [DONE]ish
# Nums: green [DONE]ish
# condition: magenta
# instruction: red [DONE]
def disas_instr(instr: Instruction, args: list[list[bool]]):
    ArgType = Instruction.ArgType
    out = instr.mnemonic + " "
    for arg, arg_type in zip(args, instr.arg_types):
        if arg_type == ArgType.ADDRESS:
            out += Fore.BLUE
        elif arg_type == ArgType.REGISTER:
            out += Fore.YELLOW
        elif arg_type == ArgType.REGISTER_PAIR:
            out += Style.BRIGHT
            out += Fore.YELLOW
        elif arg_type == ArgType.NUMBER:
            out += Fore.GREEN
        elif arg_type == ArgType.CONDITION:
            out += Fore.MAGENTA
        out += str(binary_to_int(arg)) + " "
        out += Style.NORMAL
    return out.rstrip() + Style.RESET_ALL


@dataclass
class DisassemblyLine:
    addr: list[bool]
    instr: Instruction
    args: list[list[bool]]


def disassemble(cpu: Intel4004, look_ahead: int = 5, look_behind: int | None = None) -> str:
    if look_behind is None:
        look_behind = look_ahead

    pseudo_prgm_cntr = int_to_binary(0, n_digits=12)
    instructions_after_pc = 0
    lines = []
    past_pc = False

    while instructions_after_pc < look_ahead:
        instruction = get_instr(pseudo_prgm_cntr, cpu.memory.rom)
        args = get_args(instruction, pseudo_prgm_cntr, cpu.memory.rom)

        line = f"{Fore.BLUE}{binary_to_int(pseudo_prgm_cntr)}{Style.RESET_ALL}: "
        line += f"{Fore.RED}{disas_instr(instruction, args)}{Style.RESET_ALL}"

        if past_pc:
            instructions_after_pc += 1
        elif pseudo_prgm_cntr == cpu.prgm_cntr:
            line += " <- about to execute"
            past_pc = True

        pseudo_prgm_cntr = addr_of_next_instr(pseudo_prgm_cntr, instruction)
        lines.append(line)

    lines_lower_bound = max(0, len(lines) - 1 - look_ahead - look_behind)
    return "\n".join(lines[lines_lower_bound:])
