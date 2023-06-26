import time
from time import sleep
from multiprocessing.connection import Client
from cpu import ROMPort, IOLine, InputOrOutput, Intel4004
from binary_utils import int_to_binary, binary_to_int
from dataclasses import dataclass

from log import debug_log


class Keyboard:
    def __init__(self, write_to: tuple[ROMPort, ROMPort]):
        assert ((line.io == InputOrOutput.INPUT for line in port.lines) for port in write_to)
        self.write_to = write_to

    def send_char_now(self, char: str):
        binary_char: list[bool] = int_to_binary(ord(char), n_digits=8)
        for line, char_bit in zip(self.write_to[0].lines + self.write_to[1].lines, binary_char):
            line.status = char_bit

    def send_char_sep_now(self):
        self.send_char_now("\200")

    def send_string(self, string: str):
        for char in string:
            debug_log(f"[KEYBOARD] Sending character separator")
            self.send_char_sep_now()
            sleep(0.15)
            debug_log(f"[KEYBOARD] Sending character \"{char}\"")
            self.send_char_now(char)
            sleep(0.15)


class Monitor:
    def __init__(self, read_from: tuple[ROMPort, ROMPort]):
        assert ((line.io == InputOrOutput.OUTPUT for line in port.lines) for port in read_from)
        self.read_from = read_from
        self.expecting_char = False
        self.on = False

    def csep_bit_set(self) -> bool:
        return self.read_from[0].lines[0].status

    def get_char(self) -> str:
        return chr(binary_to_int(
            [line.status for line in self.read_from[0].lines + self.read_from[1].lines]
        ))

    def turn_on(self):
        conn = Client(("localhost", 6000))
        self.on = True
        debug_log(f"[MONITOR] Warming up...")
        conn.send(f"This message is to warm the monitor up.{self.csep_bit_set()}{self.expecting_char}{self.get_char()}\n")
        while self.on:
            if self.csep_bit_set() and not self.expecting_char:
                debug_log(f"[MONITOR] Received character separator")
                self.expecting_char = True
            elif not self.csep_bit_set() and self.expecting_char:
                debug_log(f"[MONITOR] Displaying \"{self.get_char()}\"")
                conn.send(self.get_char())
                self.expecting_char = False
                debug_log(f"[MONITOR] Finished displaying \"{self.get_char()}\"")
        conn.close()
