from multiprocessing.connection import Client
from time import sleep

from cpu import IOLine, InputOrOutput
from binary_utils import int_to_binary, binary_to_int

from log import debug_log


class Monitor:
    def __init__(self, char_lines: tuple[IOLine, IOLine, IOLine, IOLine, IOLine, IOLine, IOLine],
                 char_ready_line: IOLine,
                 done_displaying_line: IOLine):
        assert (line.io == InputOrOutput.OUTPUT for line in char_lines)
        assert char_ready_line.io == InputOrOutput.OUTPUT
        assert done_displaying_line.io == InputOrOutput.INPUT
        self.char_lines = char_lines
        self.char_ready_line = char_ready_line
        self.char_ready_line_cache = False
        self.done_displaying_line = done_displaying_line
        self.done_displaying_cache = done_displaying_line.status

        self.conn = Client(("localhost", 6000))

    def turn_on(self):
        while True:
            sleep(0.01)
            debug_log("[MONITOR] Waiting for char_ready")
            while self.char_ready_line.status == self.char_ready_line_cache:
                pass
            debug_log("[MONITOR] Saw that char_ready flipped")
            self.char_ready_line_cache = self.char_ready_line.status
            # Display the character
            char = chr(binary_to_int([line.status for line in self.char_lines]))
            self.conn.send(char)
            debug_log(f"[MONITOR] Displayed {char}")
            # Signal we're done displaying
            self.done_displaying_cache = not self.done_displaying_cache
            self.done_displaying_line.status = self.done_displaying_cache
            debug_log("[MONITOR] Flipped done_displaying")


class Keyboard:
    def __init__(self, char_lines: tuple[IOLine, IOLine, IOLine, IOLine, IOLine, IOLine, IOLine],
                 char_ready_line: IOLine, done_receiving_line: IOLine):
        self.char_lines = char_lines
        self.char_ready_line = char_ready_line
        self.char_ready_line_cache = False
        self.done_receiving_line = done_receiving_line
        self.done_receiving_cache = False

    def send_string(self, string: str):
        for char in string:
            self.send_char(char)

    def send_char(self, char: str):
        debug_log(f"[KEYBOARD] Waiting for done_receiving")
        # Wait for the CPU to say it's done receiving
        while self.done_receiving_line.status == self.done_receiving_cache:
            pass
        self.done_receiving_cache = self.done_receiving_line.status
        debug_log(f"[KEYBOARD] Saw that done_receiving flipped")
        # Send the char
        char_bin = int_to_binary(ord(char), n_digits=7)
        for bit, line in zip(char_bin, self.char_lines):
            line.status = bit
        debug_log(f"[KEYBOARD] Sent character {char}")
        # Flip char_ready
        self.char_ready_line_cache = not self.char_ready_line_cache
        self.char_ready_line.status = self.char_ready_line_cache
        debug_log(f"[KEYBOARD] Flipped char_ready")
