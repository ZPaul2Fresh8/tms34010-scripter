# TMS34010 Scripter README

This is an extension for Visual Studio Code for syntax validation for the Texas Instruments TI34010 processor.

## Features

    A basic syntax checker with some assistance in writing script with autocomplete.

    - Autocomplete
    - Directive Support
    - Datatype


## Known Issues

MMTM and MMFM no yet implemented as well as the less popular pixel blitter instructions.

## Release Notes

 Non-release.
 
 ### 1.0.0

Work In Progress
 
## Syntax

 ### Directives
**Equates**<br>
Used to assign a static value, these cannot be changed.<br>
*Example* `FLAG_A  .equ 1`

**Sets**<br>
Much like equates, used to set variables but unlike equates these can re-assigned at any time.<br>
*Example:* `myAlias .set A0` This is give register **A0** the alias of **myAlias**

**Inclusions**<br>
Use this at the top of your code to make use of equates, labels and other data types from the specified file.<br>
*Example:* `.include test.asm`
