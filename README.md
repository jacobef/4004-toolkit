# RCPU
## What is RCPU?
RCPU is a importable Python package with multiple pre-built CPU emulators. Basically, just a really cool emulator with a fancy debugger.
## Installation
All you have to do is import it into your repl (use the extentions list specified below for each CPU):
```python
import rcpu
rcpu.setup("Intel 4004")
rcpu.compile("main.4004")
```
## Debugger commands
The commands for the debugger are:
* `s` to run one line
* `sd` to run one line and then dissasemble
* `r` to run all of the code
* `p` to print CPU state
* `d` to disassemble the assembly (the out file)
* `cs` to get the CPU specs
* `sp` to step and print the CPU state
* `a` to re-assemble to code (if you changed you code halfway through)
* `pdf` to get the PDF for the CPU
* <kbd>Enter key</kbd> to repeat the last command
* `q` to quit.
<br>
To access the debugger, do:
```python
import rcpu
rcpu.compile("main.4004")
rcpu.debug("main.4004out")
```
## Coding the CPU
**Warning: You need to know the specific arcatecture of the CPU you are using to code it, and the instruction set. Their PDFs can be found with the command `pdf`**<br>
The file `main.4004` **should** contain your code, and the code below will compile your code into `main.4004out`:
```python
rcpu.compile("main.4004")
```
## Running the code
All you have to do is type
```python
rcpu.run("main.4004out")
```
# Adding on the spyce module
I have no idea what to say, as `spyce` does not exist