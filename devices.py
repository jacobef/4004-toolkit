import time
from enum import Enum, auto
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
            # The CPU processes character separators much faster than normal characters,
            # because it only has to send normal characters to the monitor
            sleep(0.03)
            debug_log(f"[KEYBOARD] Sending character \"{char}\" ({ord(char)})")
            self.send_char_now(char)
            sleep(0.1)


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
        conn.send(f"This message is to warm the monitor up.\n")
        while self.on:
            if self.csep_bit_set() and not self.expecting_char:
                debug_log(f"[MONITOR] Received character separator")
                self.expecting_char = True
            elif not self.csep_bit_set() and self.expecting_char:
                char = self.get_char()
                debug_log(f"[MONITOR] Displaying \"{char}\" ({ord(char)})")
                conn.send(char)
                self.expecting_char = False
                debug_log(f"[MONITOR] Finished displaying \"{char}\" ({ord(char)})")
        conn.close()


class WaitFor(Enum):
    CHAR_READY = auto()
    DISPLAY_ACKNOWLEDGE = auto()


class BetterMonitor:
    def __init__(self, char_lines: tuple[IOLine, IOLine, IOLine, IOLine, IOLine, IOLine, IOLine],
                 char_ready_line: IOLine,
                 flips_when_done_displaying_line: IOLine, flips_when_display_acknowledged_line: IOLine):
        assert (line.io == InputOrOutput.OUTPUT for line in char_lines)
        assert char_ready_line.io == InputOrOutput.OUTPUT
        assert flips_when_done_displaying_line.io == InputOrOutput.INPUT
        assert flips_when_display_acknowledged_line.io == InputOrOutput.OUTPUT
        self.char_lines = char_lines
        self.char_ready_line = char_ready_line
        self.flips_when_done_displaying_line = flips_when_done_displaying_line
        self.flips_when_done_displaying_cache = flips_when_done_displaying_line.status
        self.flips_when_display_acknowledged_line = flips_when_display_acknowledged_line
        self.flips_when_display_acknowledged_cache = flips_when_display_acknowledged_line.status

        self.wait_for = WaitFor.CHAR_READY
        self.conn = Client(("localhost", 6000))

    def turn_on(self):
        while True:
            if self.wait_for == WaitFor.CHAR_READY:
                self.wait_for_char_ready()
            elif self.wait_for == WaitFor.DISPLAY_ACKNOWLEDGE:
                self.wait_for_display_acknowledge()

    def wait_for_char_ready(self):
        while not self.char_ready_line.status:
            pass
        # Display the character
        char = chr(binary_to_int([line.status for line in self.char_lines]))
        self.conn.send(char)
        # Signal we're done displaying and wait for acknowledgment
        self.flips_when_done_displaying_cache = not self.flips_when_done_displaying_cache
        self.flips_when_done_displaying_line.status = self.flips_when_done_displaying_cache
        self.wait_for = WaitFor.DISPLAY_ACKNOWLEDGE

    def wait_for_display_acknowledge(self):
        while self.flips_when_display_acknowledged_line.status == self.flips_when_display_acknowledged_cache:
            pass
        self.flips_when_display_acknowledged_cache = self.flips_when_display_acknowledged_line.status
        self.wait_for = WaitFor.CHAR_READY


class BetterMonitorTester:
    def __init__(self):
        self.char_lines = tuple(IOLine(InputOrOutput.OUTPUT, False) for _ in range(7))
        self.char_ready_line = IOLine(InputOrOutput.OUTPUT, False)
        self.flips_when_done_displaying_line = IOLine(InputOrOutput.INPUT, False)
        self.flips_when_done_displaying_cache = False
        self.flips_when_display_acknowledged_line = IOLine(InputOrOutput.OUTPUT, False)
        self.flips_when_display_acknowledged_cache = False

    def send_msg(self, msg: str):
        for char in msg:
            self.send_char(char)

    def send_char(self, char: str):
        char_bin = int_to_binary(ord(char), n_digits=7)
        for bit, line in zip(char_bin, self.char_lines):
            line.status = bit
        self.char_ready_line.status = True
        self.wait_for_done_displaying()

    def wait_for_done_displaying(self):
        while self.flips_when_done_displaying_line.status == self.flips_when_done_displaying_cache:
            pass
        self.flips_when_done_displaying_cache = self.flips_when_done_displaying_line.status
        self.char_ready_line.status = False
        self.flips_when_display_acknowledged_cache = not self.flips_when_display_acknowledged_cache
        self.flips_when_display_acknowledged_line.status = self.flips_when_display_acknowledged_cache
