from time import time

initial_time = time()
def debug_log(message: str):
    print(f"[{'%5f' % (time() - initial_time)}] {message}")