from multiprocessing.connection import Listener

address = ('localhost', 6001)
listener = Listener(address)
conn = listener.accept()
while True:
    msg = conn.recv()
    if msg == "\177":  # ASCII DEL character
        print("\b \b", end="", flush=True)
    else:
        print(msg, end="", flush=True)
