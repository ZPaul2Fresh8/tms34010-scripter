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

WIP

---

## Following extension guidelines

Ensure that you've read through the extensions guidelines and follow the best practices for creating your extension.

* [Extension Guidelines](https://code.visualstudio.com/api/references/extension-guidelines)

## Working with Markdown

You can author your README using Visual Studio Code. Here are some useful editor keyboard shortcuts:

* Split the editor (`Cmd+\` on macOS or `Ctrl+\` on Windows and Linux).
* Toggle preview (`Shift+Cmd+V` on macOS or `Shift+Ctrl+V` on Windows and Linux).
* Press `Ctrl+Space` (Windows, Linux, macOS) to see a list of Markdown snippets.

## For more information

* [Visual Studio Code's Markdown Support](http://code.visualstudio.com/docs/languages/markdown)
* [Markdown Syntax Reference](https://help.github.com/articles/markdown-basics/)

**Enjoy!**