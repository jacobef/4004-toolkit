import subprocess
import time

subprocess.Popen(["venv/bin/python", "monitor_monitor.py"])
time.sleep(0.2)
subprocess.call(["venv/bin/python", "everything_else_driver.py"], stdout=subprocess.DEVNULL)
