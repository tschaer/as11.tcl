# as11.tcl
A work-alike re-implementation of Motorola's 1990's-era AS11.EXE DOS assembler for the 68HC11

# Files
- as11.tcl - the assembler. Written with Tcl 8.5.
- asembler.doc - the original Motorola AS11.EXE documentation
- monitor01.asm - a 6811 assembly language file used for testing

# Usage
$ tclsh as11.tcl filename.asm [-l s]

The -l option prints a listing to stdout. The -s option prints the symbol table. Save by redirecting to a file.

The output is an S19 file which can be used by simulators, programmers, or downloaded to a development board running BUFFALO.

# Status
This is alpha code. Not rigorously tested.
