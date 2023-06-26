import os

from assemble import assemble
from devices import Keyboard, Monitor
from run_code import *
from disassemble import disassemble
from cpu import *
from colorama import Fore, Back, Style
from threading import Thread
from getkey import getkey


def setup_ports():
    I, O = IO.INPUT, IO.OUTPUT
    ports = [
        [I, I, I, I],  # upper 4 bits of character input from keyboard
        [I, I, I, I],  # lower 4 bits of character input from keyboard
        [O, O, O, O],  # upper 4 bits of character output to monitor
        [O, O, O, O],  # lower 4 bits of character output to monitor
        [I, I, I, I],
        [I, I, I, I],
        [I, I, I, I],
        [I, I, I, I],
        [O, O, O, O],
        [O, O, O, O],
        [O, O, O, O],
        [O, O, O, O],
        [O, O, O, O],
        [O, O, O, O],
        [O, O, O, O],
        [O, O, O, O],
    ]
    del I, O
    return ports


def main():
    machine_code = assemble("main")
    f = open("main.4004out", "wb")
    f.write(machine_code)
    f.close()
    cpu = Intel4004(setup_ports())
    keyboard = Keyboard(write_to=(cpu.memory.rom_ports[0], cpu.memory.rom_ports[1]))

    monitor = Monitor(read_from=(cpu.memory.rom_ports[2], cpu.memory.rom_ports[3]))
    monitor_thread = Thread(target=monitor.turn_on)
    monitor_thread.start()
    print("Warming up the monitor... (yes really)")
    sleep(0.3)

    load_machine_code(cpu, machine_code)
    cpu_thread = Thread(target=lambda: turn_on(cpu))
    cpu_thread.start()

    queue = []
    Thread(target=lambda: queue_to_kb(queue, keyboard)).start()
    while True:
        inp = getkey()
        queue.insert(0, inp)
        # if inp == "q":
        #     monitor.on = False
        #     sleep(0.5)
        #     os._exit(0)


def queue_to_kb(queue: list[str], kb: Keyboard):
    while True:
        if queue:
            kb.send_string(queue.pop())


def start_debug(cpu):
    last_cmd = None
    while True:
        cmd = input(">>> " + Fore.BLUE)
        print(Style.RESET_ALL)
        if cmd == "":
            cmd = last_cmd
        if cmd == "s":
            single_step(cpu)
        elif cmd == "r":
            while True:
                single_step(cpu)
        elif cmd == "p":
            print(cpu)
        elif cmd == "pkb":
            print(cpu.memory.rom_ports[0].lines + cpu.memory.rom_ports[1].lines)
        elif cmd == "pm":
            print(cpu.memory.rom_ports[2].lines + cpu.memory.rom_ports[3].lines)
        elif cmd == "d":
            print(disassemble(cpu, 5, 5))
        elif cmd == "sp":
            single_step(cpu)
            print(cpu)
        elif cmd == "sd":
            single_step(cpu)
            print(disassemble(cpu, 5, 5))
        elif cmd == "q":
            quit()
        elif cmd == "a":
            machine_code = assemble("main")
            f = open("main.4004out", "wb")
            f.write(machine_code)
            f.close()
            cpu.prgm_cntr = [False for _ in range(12)]
            load_machine_code(cpu, machine_code)
            print("Re-assembled")
        elif cmd == "help":
            print(
                "Type s to run one line, sd to run one line and then dissasemble, r to run all of the code, p to print CPU state, d to disassemble the assembly, cs to get the CPU specs, sp to step and print the CPU state, a to re-assemble to code, pdf to get the PDFs with helpful info, hit the enter key to repeat the last command, or q to quit")
        else:
            print(
                "Invalid command;\nType s to run one line, sd to run one line and then dissasemble, r to run all of the code, p to print CPU state, d to disassemble the assembly, cs to get the CPU specs, sp to step and print the CPU state, a to re-assemble to code, pdf to get the PDFs with helpful info, hit the enter key to repeat the last command, or q to quit")
        last_cmd = cmd


def cpu_badge(cpu_name):
    line1 = " " * (len(cpu_name) + 4)
    line3 = " " * (len(cpu_name) + 4)
    line2 = "  " + cpu_name + "  "
    return Back.BLACK + Fore.WHITE + line1 + "\n" + line2 + "\n" + line3 + Style.RESET_ALL


print(Back.BLACK + Fore.YELLOW + "██" + Style.RESET_ALL + Back.BLACK + Fore.WHITE + "   C" + Style.RESET_ALL)
print(Back.BLACK + Fore.YELLOW + "  ██" + Style.RESET_ALL + Back.BLACK + Fore.WHITE + " P" + Style.RESET_ALL)
print(Back.BLACK + Fore.YELLOW + "██" + Style.RESET_ALL + Back.BLACK + Fore.WHITE + "   U" + Style.RESET_ALL)
print("======")
print(cpu_badge("Intel 4004"))

main()

# =====DO NOT DELETE=====
# 4004 and 4040 PDFs:
# PDF 1: http://bitsavers.trailing-edge.com/components/intel/MCS4/MCS-4_Assembly_Language_Programming_Manual_Dec73.pdf
# PDF 2: http://datasheets.chipdb.org/Intel/MCS-4/datashts/intel-4004.pdf
# PDF 3: https://en.wikichip.org/w/images/9/9b/MCS-4_Manual.pdf
# 4040 PDFs:
# PDF 1: http://datasheets.chipdb.org/Intel/MCS-40/4040.pdf
# PDF 2: http://bitsavers.informatik.uni-stuttgart.de/components/intel/MCS40/MCS-40_Users_Manual_Nov74.pdf
# call the hardware simulator spyce ("spyce")
