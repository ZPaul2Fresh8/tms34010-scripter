import * as vscode from 'vscode';

// A comprehensive list of known TMS34010 mnemonics.
const KNOWN_INSTRUCTIONS = new Set([
    // General (some might be re-specified by tables)
    'MOVE', 'MOVB', 'MOVBI', 'SETF', 'TEST',
    // Arithmetic, Logical, Compare - Table 1
    'ABS', 'ADD', 'ADDC', 'ADDI', 'ADDK', 'ADDXY',
    'AND', 'ANDI', 'ANDN', 'ANDNI', 'BTST', 'CLR', 'CLRC',
    'CMP', 'CMPI', 'CMPXY', 'DEC',
    // Arithmetic, Logical, Compare - Table 2 (Continued)
    'DIVS', 'DIVU', 'LMO', 'MODS', 'MODU', 'MPYS', 'MPYU',
    'NEG', 'NEGB', 'NOT', 'OR', 'ORI', 'SETC', 'SEXT',
    'SUB', 'SUBB', 'SUBI', 'SUBK', 'SUBXY', 'XOR', 'XORI', 'ZEXT',
    // Move Instructions
    'MMFM', 'MMTM', 'MOVI', 'MOVK', 'MOVX', 'MOVY',
    // Shifts/Rotates
    'RL', 'SLA', 'SLL', 'SRA', 'SRL',
    // Program Control
    'CALL', 'CALLA', 'CALLR', 'RETI', 'RETS',
    'DSJ', 'DSJEQ', 'DSJNE', 'DSJS',      // Decrement and Skip Jump
    'JUMP',                                // Indirect Jump via register

    // Specific Conditional Jumps (Relative) - JR + Condition
    'JRP', 'JRLS', 'JRLT', 'JRLE', 'JREQ', 'JRNE', 'JRGT', 'JRGE',
    'JRHI', 'JRCC', /* Standard Carry Clear */ 'JRCS', /* Standard Carry Set */
    'JRVC', /* Standard Overflow Clear */ 'JRVS', /* Standard Overflow Set */
    'JRPL', 'JRMI', /* Standard Minus */
    'JRUC', /* Standard Unconditional */ 'JR', /* Alias for JRUC */
    // User-specified or additional variants
    'JRN',   // Jump Relative if Negative (uses N/MI condition)
    'JRNN',  // Jump Relative if Not Negative (uses NN/UC condition)
    'JRC',   // Jump Relative if Condition C (user-defined code for C)
    'JRNC',  // Jump Relative if Condition NC (user-defined code for NC)
    'JRZ',   // Jump Relative if Zero (alias for JREQ)
    'JRNZ',  // Jump Relative if Not Zero (alias for JRNE)

    // Specific Conditional Jumps (Absolute) - JA + Condition
    'JAP', 'JALS', 'JALT', 'JALE', 'JAEQ', 'JANE', 'JAGT', 'JAGE',
    'JAHI', 'JACC', /* Carry Clear Absolute */ 'JACS', /* Carry Set Absolute */
    'JAVC', /* Overflow Clear Absolute */ 'JAVS', /* Overflow Set Absolute */
    'JAPL', 'JAMI', 'JAUC', /* Unconditional Absolute */

    // Stack
    'PUSHST', 'POPST',
    // Graphics Instructions
    'PIXBLT', 'FILL', 'LINE', 'DRAV', 'PIXT', 'CPW', 'CVXYL',
    // Control
    'EXGF', 'EXGPC', 'GETPC', 'GETST', 'NOP', 'PUTST', 'REV', 'TRAP', 'DINT', 'EINT', 'EMU'
]);

// A list of valid registers
const TMS34010_REGISTERS = new Set([
    // A-File (General Purpose & Stack Pointer)
    'A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7',
    'A8', 'A9', 'A10', 'A11', 'A12', 'A13', 'A14', 'SP', // A15 is SP
    // B-File (Graphics & Frame Pointer)
    'B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7',
    'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'FP', // B15 is FP
    // Status Register and Program Counter
    'ST', 'PC',
    // I/O Registers (Partial List)
    'IOSTAT', 'CTRL1', 'CTRL2', 'HSTADR', 'HSTDATA', 'HSTCTL', 'INTPEND', 'INTENB',
    'DPYCTL', 'DPYSTRT', 'DPYADR', 'VCOUNT', 'HCOUNT',
    'PFILL', 'PLINE', 'CONVSP', 'CONVDP', 'PSIZE', 'PMOVE',
    'SADDR', 'SCOUNT', 'DADDR', 'DCOUNT', 'OFFSET', 'WINDOW', 'WSTART', 'WEND',
    'DYDX', 'COLOR0', 'COLOR1'
]);

// A comprehensive list of common assembler directives
const KNOWN_DIRECTIVES = new Set([
    '.set', '.equ', '.word', '.long', '.string', '.asciiz', '.byte', '.field', '.sint', '.float',
    '.sect', '.bss', '.text', '.data', '.align', '.space',
    '.global', '.globl', // Ensure both are present
    '.end', '.org', '.include',
    '.def', '.ref', '.newblock', '.cdef', '.endcdef', '.clink', '.cstruct', '.endstruct',
    '.struct', '.union', '.tag', '.eval', '.emsg', '.wmsg', '.fclist', '.fcnolist',
    '.drlist', '.drnolist', '.mlist', 'mnolist', '.sslist', '.ssnolist', '.var',
    '.label', '.version', '.length', '.width', '.option',
    '.if', '.else', '.endif' // Added conditional directives
]);

// Descriptions for each opcode to be used in hover tooltips
const TMS34010_OPCODE_DESCRIPTIONS: Record<string, string> = {
    // Arithmetic, Logical, Compare Instructions
    'ABS': 'ABS Rd: Store absolute value. Opcode: 0000 0011 100R DDDD (0380h base)',
    'ADD': 'ADD Rs, Rd: Add registers. Opcode: 0100 000S SSSR DDDD (4000h base)',
    'ADDC': 'ADDC Rs, Rd: Add registers with carry. Opcode: 0100 001S SSSR DDDD (4200h base)',
    'ADDI': `ADDI IW,Rd / ADDI IL,Rd: Add immediate.\n  IW: 0000 1011 000R DDDD (0B00h base) + IW (direct).\n  IL: 0000 1011 001R DDDD (0B20h base) + IL (direct).`,
    'ADDK': 'ADDK K, Rd: Add constant (5 bits, K=1..31 encoded directly, K=32 encoded as 0). Opcode: 0001 00KK KKKR DDDD (1000h base)',
    'ADDXY': 'ADDXY Rs, Rd: Add registers in XY mode. Opcode: 1110 000S SSSR DDDD (E000h base)',
    'AND': 'AND Rs, Rd: AND registers. Opcode: 0101 000S SSSR DDDD (5000h base)',
    'ANDI': `ANDI IL, Rd: AND immediate (32 bits). Opcode: 0000 1011 100R DDDD (0B80h base) + IL (direct).`,
    'ANDN': 'ANDN Rs, Rd: AND register with complement. Opcode: 0101 001S SSSR DDDD (5200h base)',
    'ANDNI': `ANDNI IL, Rd: AND not immediate (32 bits). Opcode: 0000 1011 100R DDDD (0B80h base) + IL (direct).`,
    'BTST': `BTST K,Rd / BTST Rs,Rd: Test register bit.\n  K,Rd (K=0..31): 0000 0111 01~K KKKR DDDD (0740h base, ~K is 1's complement of K in opcode).\n  Rs,Rd: 0100 101S SSSR DDDD (4A00h base).`,
    'CLR': 'CLR Rd: Clear register. Opcode: 0101 0110 0R DDDD (5600h base, R is Rd.file)',
    'CLRC': 'CLRC: Clear carry. Opcode: 0000 0011 0010 0000 (0320h)',
    'CMP': 'CMP Rs, Rd: Compare registers. Opcode: 0100 100S SSSR DDDD (4800h base)',
    'CMPI': `CMPI IW,Rd / CMPI IL,Rd: Compare immediate.\n  IW: 0000 0010 1101 0R DDDD (02D0h base) + IW' (1's complement).\n  IL: 0000 0010 1111 0R DDDD (02F0h base) + IL' (1's complement of LSBs, then 1's complement of MSBs).`,
    'CMPXY': 'CMPXY Rs, Rd: Compare X and Y halves of registers. Opcode: 1110 010S SSSR DDDD (E400h base)',
    'DEC': 'DEC Rd: Decrement register. Opcode: 0001 0100 001R DDDD (1420h base)',
    'DIVS': 'DIVS Rs, Rd: Divide registers signed. Opcode: 0101 100S SSSR DDDD (5800h base)',
    'DIVU': 'DIVU Rs, Rd: Divide registers unsigned. Opcode: 0101 101S SSSR DDDD (5A00h base)',
    'LMO': 'LMO Rs, Rd: Leftmost one. Opcode: 0110 101S SSSR DDDD (6A00h base)',
    'MODS': 'MODS Rs, Rd: Modulus signed. Opcode: 0110 110S SSSR DDDD (6C00h base)',
    'MODU': 'MODU Rs, Rd: Modulus unsigned. Opcode: 0110 111S SSSR DDDD (6E00h base)',
    'MPYS': 'MPYS Rs, Rd: Multiply registers (signed). Opcode: 0101 110S SSSR DDDD (5C00h base)',
    'MPYU': 'MPYU Rs, Rd: Multiply registers (unsigned). Opcode: 0101 111S SSSR DDDD (5E00h base)',
    'NEG': 'NEG Rd: Negate register. Opcode: 0000 0011 101R DDDD (03A0h base)',
    'NEGB': 'NEGB Rd: Negate register with borrow. Opcode: 0000 0011 110R DDDD (03C0h base)',
    'NOT': 'NOT Rd: Complement register. Opcode: 0000 0011 111R DDDD (03E0h base)',
    'OR': 'OR Rs, Rd: OR registers. Opcode: 0101 010S SSSR DDDD (5400h base)',
    'ORI': `ORI IL, Rd: OR immediate (32 bits). Opcode: 0000 1011 101R DDDD (0BA0h base) + IL (direct).`,
    'SETC': 'SETC: Set carry. Opcode: 0000 1101 1110 0000 (0DE0h)',
    'SEXT': 'SEXT Rd [,F]: Sign extend. F=0 for FS0, F=1 for FS1. Opcode: 0000 1101 F100 000R DDDD (0D40h base).',
    'SUB': 'SUB Rs, Rd: Subtract registers. Opcode: 0100 010S SSSR DDDD (4400h base)',
    'SUBB': 'SUBB Rs, Rd: Subtract registers with borrow. Opcode: 0100 011S SSSR DDDD (4600h base)',
    'SUBI': `SUBI IW,Rd / SUBI IL,Rd: Subtract immediate.\n  IW: 0000 0011 0101 0R DDDD (0350h base) + IW' (1's complement).\n  IL: 0000 0011 0111 0R DDDD (0370h base) + IL' (1's complement).`,
    'SUBK': 'SUBK K, Rd: Subtract constant (5 bits, K=1..31 encoded directly, K=32 encoded as 0). Opcode: 0001 01KK KKKR DDDD (1400h base)',
    'SUBXY': 'SUBXY Rs, Rd: Subtract registers in XY mode. Opcode: 1110 001S SSSR DDDD (E200h base)',
    'XOR': 'XOR Rs, Rd: Exclusive OR registers. Opcode: 0101 011S SSSR DDDD (5600h base)',
    'XORI': `XORI IL, Rd: Exclusive OR immediate (32 bits). Opcode: 0000 1011 1101 0R DDDD (0BB0h base) + IL (direct).`,
    'ZEXT': 'ZEXT Rd [,F]: Zero extend. F=0 for FS0, F=1 for FS1. Opcode: 0000 1101 F100 001R DDDD (0D40h base | (1<<5)).',
    // Move Instructions
    'MMFM': 'MMFM Rs, [List]: Move multiple from memory. Opcode: 0000 1001 101R DDDD (09A0h base) + ListMask',
    'MMTM': 'MMTM Rs, [List]: Move multiple to memory. Opcode: 0000 1001 100R DDDD (0980h base) + ListMask',
    'MOVB': `Move Byte (various addressing modes)`,
    'MOVE': `Move Word/Field (various addressing modes)`,
    'MOVI': `MOVI IW,Rd / MOVI IL,Rd: Move immediate.\n  IW: 0000 1001 110R DDDD (09C0h base) + IW.\n  IL: 0000 1001 111R DDDD (09E0h base) + IL.`,
    'MOVK': 'MOVK K,Rd: Move constant (5 bits, K=1..31, K=32 as 0). Opcode: 0001 10KK KKKR DDDD (1800h base)',
    'MOVX': 'MOVX Rs,Rd: Move X half of register. Opcode: 1110 11MS SSSR DDDD (EC00h base)',
    'MOVY': 'MOVY Rs,Rd: Move Y half of register. Opcode: 1110 11MS SSSR DDDD (EE00h base)',
    // Graphics Instructions
    'CPW': 'CPW Rs, Rd: Compare point to window. Opcode: 1110 011S SSSR DDDD (E600h base).',
    'CVXYL': 'CVXYL Rs, Rd: Convert XY address to linear address. Opcode: 1110 100S SSSR DDDD (E800h base).',
    'DRAV': 'DRAV Rs, Rd: Draw and advance. Opcode: 1111 011S SSSR DDDD (F600h base).',
    'FILL': `FILL L | FILL XY: Fill array.\n  L: Opcode 0000 1111 1100 0000 (0FC0h).\n  XY: Opcode 0000 1111 1110 0000 (0FE0h).`,
    'LINE': `LINE [0|1]: Line draw.\n  0: Opcode 0000 1101 1111 0001 1010 (0DF1Ah).\n  1: Opcode 0000 1101 1111 1001 1010 (0DF9Ah).`,
    'PIXBLT': `Pixel Block Transfer (multiple modes)`,
    'PIXT': `Pixel Transfer (multiple modes)`,
    // Program Control
    'CALL': 'CALL Rs: Call subroutine indirect. Opcode: 0000 1001 001R DDDD (0920h base).',
    'CALLA': 'CALLA Address: Call subroutine absolute. Opcode: 0000 1101 0101 1111 (0D5Fh) + Address32.',
    'CALLR': 'CALLR Address: Call subroutine relative. Opcode: 0000 1101 0011 1111 (0D3Fh) + Address16.',
    'DINT': 'DINT: Disable interrupts. Opcode: 0000 0011 0110 0000 (0360h).',
    'EINT': 'EINT: Enable interrupts. Opcode: 0000 1101 0110 0000 (0D60h).',
    'EMU': 'EMU: Initiate emulation. Opcode: 0000 0001 0000 0000 (0100h).',
    'EXGF': 'EXGF Rd, F: Exchange field size. Opcode: 1101 01F1 F00R DDDD (D2xR base).',
    'EXGPC': 'EXGPC Rd: Exchange program counter with register. Opcode: 0000 0001 001R DDDD (0120h base).',
    'GETPC': 'GETPC Rd: Get program counter. Opcode: 0000 0001 010R DDDD (0140h base).',
    'GETST': 'GETST Rd: Get status register. Opcode: 0000 0001 100R DDDD (0180h base).',
    'NOP': 'NOP: No operation. Opcode: 0000 0011 0000 0000 (0300h).',
    'POPST': 'POPST: Pop status register from stack. Opcode: 0000 0001 1100 0000 (01C0h).',
    'PUSHST': 'PUSHST: Push status register onto stack. Opcode: 0000 0001 1110 0000 (01E0h).',
    'PUTST': 'PUTST Rs: Copy register into status. Opcode: 0000 0001 101R DDDD (01A0h base).',
    'RETI': 'RETI: Return from interrupt. Opcode: 0000 1001 0100 0000 (0940h).',
    'RETS': 'RETS [N]: Return from subroutine (N=skip count). Opcode: 0000 1001 011N NNNN (0960h base).',
    'REV': 'REV Rd: Get TMS34010 revision level. Opcode: 0000 0000 001R DDDD (0020h base).',
    'TRAP': 'TRAP N: Software interrupt. Opcode: 0000 1001 000N NNNN (0900h base).',
    // Jump Instructions
    'DSJ': 'DSJ Rd, Address: Decrement and skip jump. Opcode: 0000 1101 1000 DDDD (0D80h base) + Addr16.',
    'JUMP': 'JUMP Rs: Jump indirect via A-file register. Opcode: 0000 0001 0110 DDDD (0160h base).',
    // Shift Instructions
    'RL': `RL K,Rd / RL Rs,Rd: Rotate left.`,
    'SLA': `SLA K,Rd / SLA Rs,Rd: Shift left arithmetic.`,
    'SLL': `SLL K,Rd / SLL Rs,Rd: Shift left logical.`,
    'SRA': `SRA K,Rd / SRA Rs,Rd: Shift right arithmetic.`,
    'SRL': `SRL K,Rd / SRL Rs,Rd: Shift right logical.`,
};


const DIAGNOSTIC_COLLECTION = vscode.languages.createDiagnosticCollection('tms34010');

export function activate(context: vscode.ExtensionContext) {
    // Add a hover provider to show information about instructions
    context.subscriptions.push(
        vscode.languages.registerHoverProvider('tms-assembly', {
            provideHover(document, position, token) {
                const range = document.getWordRangeAtPosition(position);
                if (!range) {
                    return null;
                }
                const word = document.getText(range).toUpperCase();

                // Check for instruction description
                if (TMS34010_OPCODE_DESCRIPTIONS[word]) {
                    const content = new vscode.MarkdownString();
                    content.appendCodeblock(TMS34010_OPCODE_DESCRIPTIONS[word], 'plaintext');
                    return new vscode.Hover(content, range);
                }

                // Check for register
                if (TMS34010_REGISTERS.has(word)) {
                     const content = new vscode.MarkdownString(`**Register:** ${word}`);
                     return new vscode.Hover(content, range);
                }

                return null;
            }
        })
    );


    // Initial check of the active document
    if (vscode.window.activeTextEditor) {
        updateDiagnostics(vscode.window.activeTextEditor.document, DIAGNOSTIC_COLLECTION);
    }

    // Add listeners for document changes
    context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(editor => {
        if (editor) {
            updateDiagnostics(editor.document, DIAGNOSTIC_COLLECTION);
        }
    }));
    context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(event => {
        updateDiagnostics(event.document, DIAGNOSTIC_COLLECTION);
    }));
    context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(doc => {
        DIAGNOSTIC_COLLECTION.delete(doc.uri);
    }));
}

function updateDiagnostics(doc: vscode.TextDocument, collection: vscode.DiagnosticCollection): void {
    if (doc.languageId !== 'tms-assembly') {
        return;
    }

    const diagnostics: vscode.Diagnostic[] = [];
    const definedLabels = new Map<string, vscode.Range>();
    const definedEquates = new Map<string, vscode.Range>();

    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const text = line.text;
        const trimmedText = text.trim();

        // Skip empty lines and comments
        if (trimmedText.length === 0 || trimmedText.startsWith(';')) {
            continue;
        }

        // Check for label definitions. A label is a word at the beginning of a line followed by a colon.
        const labelMatch = text.match(/^\s*([a-zA-Z_][a-zA-Z0-9_]*):/);
        if (labelMatch) {
            const labelName = labelMatch[1].toUpperCase();
            const labelRange = new vscode.Range(lineIndex, text.indexOf(labelMatch[1]), lineIndex, text.indexOf(labelMatch[1]) + labelMatch[1].length);
            
            if (definedLabels.has(labelName)) {
                // Found a duplicate label
                const originalRange = definedLabels.get(labelName)!;
                const diagnostic = new vscode.Diagnostic(
                    labelRange,
                    `Duplicate label definition: '${labelMatch[1]}'`,
                    vscode.DiagnosticSeverity.Error
                );
                diagnostic.relatedInformation = [
                    new vscode.DiagnosticRelatedInformation(new vscode.Location(doc.uri, originalRange), 'First defined here')
                ];
                diagnostics.push(diagnostic);
            } else {
                definedLabels.set(labelName, labelRange);
            }
        }

        // Check for equate definitions. NAME .equ VALUE
        const equateMatch = trimmedText.match(/^([a-zA-Z_][a-zA-Z0-9_]+)\s+\.(equ|set)\s+/i);
        if (equateMatch) {
            const equateName = equateMatch[1].toUpperCase();
            const equateRange = new vscode.Range(lineIndex, text.indexOf(equateMatch[1]), lineIndex, text.indexOf(equateMatch[1]) + equateMatch[1].length);

            if (definedEquates.has(equateName)) {
                // Found a duplicate equate
                const originalRange = definedEquates.get(equateName)!;
                const diagnostic = new vscode.Diagnostic(
                    equateRange,
                    `Duplicate equate definition: '${equateMatch[1]}'`,
                    vscode.DiagnosticSeverity.Error
                );
                diagnostic.relatedInformation = [
                    new vscode.DiagnosticRelatedInformation(new vscode.Location(doc.uri, originalRange), 'First defined here')
                ];
                diagnostics.push(diagnostic);
            } else {
                definedEquates.set(equateName, equateRange);
            }
            // This line is an equate definition, so we don't need to check for instructions
            continue;
        }
        
        // Now check for instructions on the same line or other lines
        // Adjust regex to find the first word, ignoring a label at the start
        const instructionMatch = trimmedText.replace(/^([a-zA-Z_][a-zA-Z0-9_]*):/, '').trim().match(/^([a-zA-Z0-9\._]+)/);

        if (!instructionMatch) {
            continue; // Line might just have a label
        }

        const instruction = instructionMatch[1].toUpperCase();
        
        // Check if the instruction is known
        if (!KNOWN_INSTRUCTIONS.has(instruction) && !KNOWN_DIRECTIVES.has(instruction.toLowerCase())) {
            const range = new vscode.Range(lineIndex, text.indexOf(instructionMatch[1]), lineIndex, text.indexOf(instructionMatch[1]) + instructionMatch[1].length);
            const diagnostic = new vscode.Diagnostic(
                range,
                `Unrecognized instruction or directive: '${instructionMatch[1]}'`,
                vscode.DiagnosticSeverity.Error
            );
            diagnostics.push(diagnostic);
        }
    }
    collection.set(doc.uri, diagnostics);
}

export function deactivate() {}
