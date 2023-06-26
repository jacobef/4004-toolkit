from multiprocessing.connection import Listener

address = ('localhost', 6000)
listener = Listener(address)
conn = listener.accept()
while True:
    msg = conn.recv()
    if msg == "\177": # ASCII DEL character
        print("\b \b", end="")
    else:
        print(msg, end="")
listener.close()
