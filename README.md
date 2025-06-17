# TMS34010 Scripter README

This is an extension for Visual Studio Code for syntax validation for the Texas Instruments TI34010 processor.

## Features

A basic syntax checker with some assistance in writing script with autocomplete.

Supported Directives:
> .include, .set, .equ

## Requirements

Just this extension and some TMS34010 assembly code!

## Extension Settings

Include if your extension adds any VS Code settings through the `contributes.configuration` extension point.

For example:

This extension contributes the following settings:

* `myExtension.enable`: Enable/disable this extension.
* `myExtension.thing`: Set to `blah` to do something.

## Known Issues

Jumps to labels not complete. Currently labels are defined by their name and a colon as a postfix. Thinking about changing this to NOT have the colon to follow existing code sources which use a tabbing to indicate code from labels which start at column 0.
MMTM and MMFM no yet implemented as well as the less popular pixel blitter instructions.

## Release Notes

Non-release.

### 1.0.0

Work In Progress
