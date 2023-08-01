import os
from typing import Callable, Any

from assemble import assemble, GivenArgType
from devices import Monitor, Keyboard
from run_code import *
from disassemble import disassemble
from cpu import *
from colorama import Fore, Back, Style
from threading import Thread
import curses


def setup_ports():
    I, O = IO.INPUT, IO.OUTPUT
    ports = [
        [I, I, I, I],  # upper 4 bits of character input from keyboard
        [I, I, I, I],  # lower 4 bits of character input from keyboard
        [I, I, I, I],  # "char ready" input from keyboard
        [O, O, O, O],  # "done receiving" output to keyboard
        [O, O, O, O],  # upper 4 bits of character output to monitor
        [O, O, O, O],  # lower 4 bits of character output to monitor
        [O, O, O, O],  # "char ready" output to monitor
        [I, I, I, I],  # "done displaying" input from monitor
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
    stdscr = curses.initscr()
    curses.cbreak()

    machine_code = assemble("main")
    f = open("main.4004out", "wb")
    f.write(machine_code)
    f.close()
    cpu = Intel4004(setup_ports())

    keyboard = Keyboard(char_lines=(*(cpu.memory.rom_ports[0].lines[1:]), *(cpu.memory.rom_ports[1].lines)),
                        char_ready_line=cpu.memory.rom_ports[2].lines[3],
                        done_receiving_line=cpu.memory.rom_ports[3].lines[3])

    monitor = Monitor(char_lines=(*(cpu.memory.rom_ports[4].lines[1:]), *(cpu.memory.rom_ports[5].lines)),
                      char_ready_line=cpu.memory.rom_ports[6].lines[3],
                      done_displaying_line=cpu.memory.rom_ports[7].lines[3])
    start_thread(monitor.turn_on)

    load_machine_code(cpu, machine_code)
    start_thread(lambda: turn_on(cpu))

    queue = []
    start_thread(lambda: queue_to_kb(queue, keyboard))

    def keys_to_queue():
        while True:
            key = chr(stdscr.getch())
            if key:
                queue.insert(0, key)
    run_with_cleanup(keys_to_queue)


def start_thread(fn: Callable[[], Any]):
    Thread(target=lambda: run_with_cleanup(fn)).start()


def run_with_cleanup(fn: Callable[[], Any]):
    try:
        fn()
    except Exception as e:
        debug_log(f"[MAIN] Caught exception")
        curses.echo()
        curses.nocbreak()
        curses.endwin()
        raise e
    finally:
        debug_log(f"[MAIN] Caught exception")
        curses.echo()
        curses.nocbreak()
        curses.endwin()


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
        elif cmd == "pp":
            print(cpu.memory.rom_ports)
        elif cmd == "d":
            print(disassemble(cpu, 10, 10))
        elif cmd == "sp":
            single_step(cpu)
            print(cpu)
        elif cmd == "sd":
            single_step(cpu)
            print(disassemble(cpu, 10, 10))
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
        elif cmd.split(" ")[0] == "c":
            while binary_to_int(cpu.prgm_cntr) != int(cmd.split(" ")[1]):
                single_step(cpu)
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
