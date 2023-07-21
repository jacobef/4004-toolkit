from time import time
from typing import Literal

initial_time = time()
Logger = open("log.txt", "w")
indent = 0


def debug_log(message: str, indent_change: Literal[-1, 0, 1] = 0):
    global indent
    Logger.write("\t" * indent + f"[{'%5f' % (time() - initial_time)}] {message}\n")
    indent += indent_change
    Logger.flush()
