from cpu import *
from parse_instruction import *
from colorama import Fore, Style


# register pair: bold yellow, Register: yellow regular
# adress: blue [DONE]ish
# Nums: green [DONE]ish
# condition: magenta
# instruction: red [DONE]
def disas_instr(instr: Instruction, args: list[list[bool]], values_to_labels: dict[int, str] | None = None):
    ArgType = Instruction.ArgType
    out = instr.mnemonic + " "
    for arg, arg_type in zip(args, instr.arg_types):
        clarify_real_value = False
        arg_colors = {ArgType.ADDRESS: Fore.BLUE, ArgType.CONDITION: Fore.MAGENTA, ArgType.REGISTER: Fore.RED, ArgType.REGISTER_PAIR: Fore.YELLOW, ArgType.NUMBER: Fore.GREEN}
        if arg_type == ArgType.CONDITION:
            arg_val_str = binary_to_string(arg)
        elif values_to_labels and binary_to_int(arg) in values_to_labels and arg_type == ArgType.ADDRESS and len(arg) == 12:
            arg_val_str = values_to_labels[binary_to_int(arg)]
            clarify_real_value = True
        else:
            arg_val_str = str(binary_to_int(arg))

        if arg_type == ArgType.REGISTER:
            suffix = "R"
        elif arg_type == ArgType.REGISTER_PAIR:
            suffix = "P"
        elif arg_type == ArgType.CONDITION:
            suffix = "B"
        else:
            suffix = ""

        out += arg_colors[arg_type] + arg_val_str + suffix + (f"(={binary_to_int(arg)})" if clarify_real_value else "") + " " + Style.RESET_ALL
    return out.rstrip() + Style.RESET_ALL


@dataclass
class DisassemblyLine:
    addr: list[bool]
    instr: Instruction
    args: list[list[bool]]


def disassemble(cpu: Intel4004, values_to_labels: dict[int, str], look_ahead: int = 5, look_behind: int | None = None) -> str:
    if look_behind is None:
        look_behind = look_ahead

    pseudo_pc = int_to_binary(0, n_digits=12)
    instructions_after_pc = 0
    lines = []
    past_pc = False

    while instructions_after_pc < look_ahead:
        instruction = get_instr(pseudo_pc, cpu.memory.program_ram)
        args = get_args(instruction, pseudo_pc, cpu.memory.program_ram)

        pc_int = binary_to_int(pseudo_pc)
        label_mark = f"({values_to_labels[pc_int]})" if pc_int in values_to_labels else ""
        line = f"{Fore.BLUE}{pc_int}{label_mark}{Style.RESET_ALL}: "
        line += f"{Fore.RED}{disas_instr(instruction, args, values_to_labels)}{Style.RESET_ALL} "

        if past_pc:
            instructions_after_pc += 1
        elif pseudo_pc == cpu.prgm_cntr:
            line += " <- about to execute"
            past_pc = True

        pseudo_pc = addr_of_next_instr(pseudo_pc, instruction)
        lines.append(line)

    lines_lower_bound = max(0, len(lines) - 1 - look_ahead - look_behind)
    return "\n".join(lines[lines_lower_bound:])
