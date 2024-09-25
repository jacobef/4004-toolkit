## What this is

This is a set of tools for Intel 4004 development. Currently, it has an assembler (with lots of convenience features relative to standard Intel 4004 assembly) and an emulator. main.4004 is a WIP program whose eventual goal is to become a shell. Currently, it just interfaces with the virtual keyboard and monitor, and "echo" works, but that's it. 

This project isn't ready for other people to use yet, but feel free to try it out in its current state if you're interested!

## Usage

`zig build` builds the assembler and emulator, which will then be in `zig-out/bin`. You can run the assembler with `4004-assembler assembly_path output_executable_path`, and run the emulator with `4004-emulator executable_path`. If you just want to build the project, and assemble and execute main.4004, run:

```
zig build
zig-out/bin/4004-assembler 4004-asm/main.4004 4004-asm/main.4004out
zig-out/bin/4004-emulator 4004-asm/main.4004out
```



