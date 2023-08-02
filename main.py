import subprocess
import time

subprocess.Popen(["python3.11", "monitor_monitor.py"])
time.sleep(0.2)
subprocess.call(["python3.11", "everything_else_driver.py"], stdout=subprocess.DEVNULL)