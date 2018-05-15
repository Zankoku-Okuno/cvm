## Goals

> a simple virtual machine suitable as a target for compilers of systems-level languages, preferably with low overhead
This entails several things, so the rest of this section is all about detailing those consequences.

### Unnecessary Features

I am not trying to build a simulator.
I can take inspiration from real hardware, but there's no reason to spend extra effort to model physical hardware within the machine.
This especially means any form of pipelining or superscalar architecture.

There's no need to have a virtualized OS.
In particular, I don't need to implement anything like page tables.
It might be interesting to build an OS within the VM to test a language's ability to implement OSes.
However, there doesn't need to be any facility for building secure OSes: privilege rings, page tables, &c.
If it's just a test of a simple OS, the test processes should just "play nice".

Likewise, the VM doesn't need a simulated bootloader.
The VM exports a C API, and normal languages should be able to use that interface to boot the VM into a process.

In real hardware, register-register RISC architectures are great and fast and cool.
In software, the sequence of memcpy, operation, memcpy is unnecessarily slow.
Therefore, instructions should remain memory-to-memory as much as possible, and "registers" (if any) should be used for process-local state only.
Similarly, there's no need to write the tightest loops (e.g. memcpy) in the VM: complex instructions can move those loops into native code.
There may also be some performance improvement by allowing complex addressing modes, but I'm not sure if the improvement from native address arithmetic actually wins against the slowdown of complex decoding, especially since both algorithms are O(1).

### Leverage Native

In order to speed up dereferencing, I want to leverage C pointers to implement VM pointers.
Given that software should be secure by default (and in a VM especially so), VM pointers will technically be implemented differently.
While the data format should be the same as on native, reading and writing through pointers will involve bounds-checking.


## Code Standards

I'd like to separate the `.h`-file idea into three:
  * Exports (traditional `include`s) --- `include/*.h`
  * Prototypes (only and always included by the associated .c file) --- `src/*.proto.h`
  * Inline implementations --- `src/*.inl.h`


## Quality Check

Some things will vary between machines (e.g. size of addresses, data sizes supported by the ALU).
I want to be able to cross-compile.
Therefore, I will need to audit for all of these variables and record them in a descriptive block at the head of each binary.

Are all arithmetic operations safe?


## To-do

  * [ ] Machine Design
      * [ ] Memory Architecture
      * IO/FFI Architecture
      * Instruction Set
      * Instruction Binary Format
  * Interpreter
      * Core Data Structures
          * [ ] Global Memory
          * [x] Register Files (per-processor)
          * I/O Ports (unified interface for all external communication)
          * [ ] Machine Metadata
          * [ ] Interrupts
              * I/O
              * Interprocessor
              * Timer
              * Debugging
              * Synchronous (error conditions)
      * Core Algorithms
          * Moving Memory
          * Arithmetic/Logic
          * Read/Write I/O Ports
      * Debugging
          * Dump Image
          * Breakpoints
          * Single-Step
          * Live Probing
      * C Foreign Function Interface
      * Loading and Linking
  * Assembler and Disassembler (after binary format designed)