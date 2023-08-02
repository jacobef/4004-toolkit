#!/bin/bash

# Start monitor_monitor.py in the background
python3 monitor_monitor.py &

# Start everything_else_driver.py in the foreground
python3 everything_else_driver.py

# Wait for all background processes to finish
wait

