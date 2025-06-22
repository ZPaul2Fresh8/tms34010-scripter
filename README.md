# TMS34010 Scripter User Guide

This Visual Studio Code extension provides powerful language support for the Texas Instruments TMS34010 processor, transforming VS Code into a rich development environment for assembly scripting. It goes far beyond basic syntax coloring to provide in-depth code validation, intelligent autocompletion, and easy code navigation.

## Core Features

This extension is designed to help you write code faster and with fewer errors. It understands the relationships between your files, symbols, and instructions.

### 1. Intelligent Code Completion (Autocomplete)

The extension anticipates what you are trying to write and offers context-sensitive suggestions to speed up your workflow.

* **Instructions**: Start typing on a new line and a list of all known TMS34010 instructions will appear.
* **Directives**: Type a period (`.`) to see a list of all supported assembler directives like `.include`, `.sect`, `.equ`, and `.bss`.
* **Labels**: Type the at-symbol (`@`) to get a filtered list of all code labels, perfect for writing absolute address operands. When you select a label, the `@` is automatically removed.
* **Operands**: After typing an instruction and a space, the extension will suggest valid operands, such as registers or labels, based on what the instruction expects.
* **Indirect Addressing**: Type an asterisk (`*`) to get a list of registers valid for indirect addressing.

### 2. In-depth Diagnostics and Linting

The linter analyzes your entire project, including all `.include`'d files, to find errors before you even try to assemble the code. It checks for:

* **Invalid Syntax**: Unrecognized instructions or directives.
* **Operand Errors**: Incorrect number or type of operands for instructions.
* **Symbol Errors**:
    * **Duplicate Symbols**: Detects when a label or `.equ` symbol has been defined more than once. It correctly ignores re-definitions inside `.if`/`.else` blocks and allows re-definition of `.set` variables.
    * **Undefined Symbols**: Flags labels that are used but never defined.
* **File Path Errors**: Warns you if a file specified in an `.include` directive cannot be found.
* **Section Conflicts**: Detects when a named section is incorrectly defined with both `.sect` and `.usect`.

### 3. Hover-to-Learn

Simply hover your mouse over a piece of code to get more information:

* **Instructions**: See the instruction's syntax, a description of what it does, and its opcode format.
* **Registers**: Confirm the name of a register.
* **Symbols**: See the value of an `.equ` or `.set` alias, or the size of a `.bss` section.

### 4. Code Navigation (Go to Definition)

Quickly navigate your codebase using `Ctrl+Click` (or your platform's equivalent).

* **Symbols**: Click on a label, variable, or constant to jump directly to the line where it is defined (`label:`, `.equ`, `.bss`).
* **Global Symbols**: The extension correctly links `.global` declarations with their definitions. You can `Ctrl+Click` the `.global` entry to jump to the label definition, and `Ctrl+Click` the label to jump back to its `.global` declaration.
* **Included Files**: `Ctrl+Click` on the filename in an `.include` directive to open that file instantly.

## How to Use the Extension

1.  **Writing Code**: Start typing an instruction or directive. Use the `Tab` or `Enter` key to accept an autocomplete suggestion. When you need to specify a label for a jump or an address, type `@` to see a filtered list of available code labels.

2.  **Checking for Errors**: As you type, red and yellow squiggly lines will appear under potential issues. Hover over them to see a description of the error. You can also open the **Problems** panel in VS Code (`Ctrl+Shift+M`) to see a list of all errors and warnings across your entire project.

3.  **Exploring Code**: When working with an existing codebase, use `Ctrl+Click` on any symbol or `.include` directive to quickly navigate and understand the project structure. Use the hover feature to get quick reminders about instruction syntax or symbol values.

## Supported Syntax

### Data Types

* **Decimal**: Standard base-10 numbers (e.g., `10`, `-256`).
* **Hexadecimal**: Can be written with an `h` suffix or a `>` prefix (e.g., `40h`, `>40`, `0x40`).
* **Binary**: Written with a `b` suffix (e.g., `10101010b`).

### Directives

The extension supports a wide range of standard assembler directives.

| Directive | Description | Example |
| :--- | :--- | :--- |
| **`.equ`** | Assigns a permanent, constant value to a symbol. | `V_BLANK .equ 13h` |
| **`.set`** | Assigns a value to a symbol that can be changed later. | `current_pal .set B0` |
| **`.include`** | Includes the contents of another file. | `.include "equates.asm"` |
| **`.bss`** | Reserves a block of uninitialized memory space for a variable. | `stack_top .bss 1024` |
| **`.global`** | Declares a symbol as being accessible from other files. | `.global _main` |
| **`.sect`** | Assembles code or data into a separate, named section. | `.sect "init_code"` |
| **`.usect`** | Reserves space for variables in a separate, uninitialized section. | `my_vars .usect "vars", 512` |