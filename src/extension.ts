import * as vscode from 'vscode';

// Define operand types for validation
enum OperandType {
    Register,
    Immediate,
    Constant,
    Address,
    Label,
    RegisterOrConstant,
    RegisterOrLabel,
    Addressable, // Represents a register or a memory address
    None
}

// Define the structure for an instruction's syntax rule
interface InstructionRule {
    operands: OperandType[];
    syntax: string;
    opcode: string;
    hasOptionalFieldSize?: boolean;
    requireSameRegisterPage?: boolean;
}

// --- Validation Helper Functions ---

const A_REGISTERS = new Set(['A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 'A12', 'A13', 'A14', 'SP']);
const B_REGISTERS = new Set(['B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'FP']);
const OTHER_REGISTERS = new Set(['ST', 'PC', 'IOSTAT', 'CTRL1', 'CTRL2', 'HSTADR', 'HSTDATA', 'HSTCTL', 'INTPEND', 'INTENB', 'DPYCTL', 'DPYSTRT', 'DPYADR', 'VCOUNT', 'HCOUNT', 'PFILL', 'PLINE', 'CONVSP', 'CONVDP', 'PSIZE', 'PMOVE', 'SADDR', 'SCOUNT', 'DADDR', 'DCOUNT', 'OFFSET', 'WINDOW', 'WSTART', 'WEND', 'DYDX', 'COLOR0', 'COLOR1']);
const TMS34010_REGISTERS = new Set([...A_REGISTERS, ...B_REGISTERS, ...OTHER_REGISTERS]);

const isRegister = (op: string): boolean => TMS34010_REGISTERS.has(op.toUpperCase());

const isConstant = (op: string): boolean => {
    let value = op.toUpperCase();
    if (value.startsWith('#')) {
        value = value.substring(1);
    }
    
    // Hex with 'h' suffix (can be negative)
    if (/^-?[0-9A-F]+H$/.test(value)) return true;
    
    // Hex with '>' prefix (cannot be negative)
    if (/^>[0-9A-F]+$/.test(value)) return true;
    
    // Binary with 'b' prefix (cannot be negative)
    if (/^B[01]+$/.test(value)) return true;
    
    // Decimal (can be negative)
    if (/^-?[0-9]+$/.test(value)) return true;
    
    return false;
};

// Checks if a string has the format of a label (doesn't check if defined)
const isLabelFormat = (op: string): boolean => {
    return /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(op) && !isRegister(op);
};

const isAddress = (op: string): boolean => {
    // Absolute address: @>FFFF, @label
    if (op.startsWith('@')) {
        const subOperand = op.substring(1);
        return isLabelFormat(subOperand) || isConstant(subOperand);
    }
    // Indirect address: *A0, -*A0, *A0+, *A0(10), *A0(LABEL)
    const indirectMatch = op.match(/^(\*|-)?(A[0-9]{1,2}|B[0-9]{1,2}|SP|FP)(\(.+\))?(\+)?$/i);
    return !!indirectMatch;
};


// A structured map of instructions and their validation rules
const INSTRUCTION_RULES: Map<string, InstructionRule> = new Map([
    // Arithmetic, Logical, Compare
    ['ABS',   { operands: [OperandType.Register], syntax: "ABS Rd", opcode: "0000 0011 100R DDDD" }],
    ['ADD',   { operands: [OperandType.Register, OperandType.Register], syntax: "ADD Rs, Rd", opcode: "0100 000S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['ADDC',  { operands: [OperandType.Register, OperandType.Register], syntax: "ADDC Rs, Rd", opcode: "0100 001S SSSR DDDD", requireSameRegisterPage: true }],
    ['ADDI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "ADDI IW/IL, Rd", opcode: "IW: 0000 1011 000R DDDD\nIL: 0000 1011 001R DDDD", hasOptionalFieldSize: true }],
    ['ADDK',  { operands: [OperandType.Constant, OperandType.Register], syntax: "ADDK K, Rd", opcode: "0001 00KK KKKR DDDD", hasOptionalFieldSize: true }],
    ['ADDXY', { operands: [OperandType.Register, OperandType.Register], syntax: "ADDXY Rs, Rd", opcode: "1110 000S SSSR DDDD", requireSameRegisterPage: true }],
    ['AND',   { operands: [OperandType.Register, OperandType.Register], syntax: "AND Rs, Rd", opcode: "0101 000S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['ANDI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "ANDI IL, Rd", opcode: "0000 1011 100R DDDD", hasOptionalFieldSize: true }],
    ['ANDN',  { operands: [OperandType.Register, OperandType.Register], syntax: "ANDN Rs, Rd", opcode: "0101 001S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['ANDNI', { operands: [OperandType.Immediate, OperandType.Register], syntax: "ANDNI IL, Rd", opcode: "0000 1011 100R DDDD", hasOptionalFieldSize: true }],
    ['BTST',  { operands: [OperandType.RegisterOrConstant, OperandType.Register], syntax: "BTST K/Rs, Rd", opcode: "K: 0000 0111 01~K KKKR DDDD\nRs: 0100 101S SSSR DDDD" }],
    ['CLR',   { operands: [OperandType.Register], syntax: "CLR Rd", opcode: "0101 0110 0R DDDD", hasOptionalFieldSize: true }],
    ['CLRC',  { operands: [], syntax: "CLRC", opcode: "0000 0011 0010 0000" }],
    ['CMP',   { operands: [OperandType.Register, OperandType.Register], syntax: "CMP Rs, Rd", opcode: "0100 100S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['CMPI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "CMPI IW/IL, Rd", opcode: "IW: 0000 0010 1101 0R DDDD\nIL: 0000 0010 1111 0R DDDD", hasOptionalFieldSize: true }],
    ['CMPXY', { operands: [OperandType.Register, OperandType.Register], syntax: "CMPXY Rs, Rd", opcode: "1110 010S SSSR DDDD", requireSameRegisterPage: true }],
    ['DEC',   { operands: [OperandType.Register], syntax: "DEC Rd", opcode: "0001 0100 001R DDDD" }],
    ['DIVS',  { operands: [OperandType.Register, OperandType.Register], syntax: "DIVS Rs, Rd", opcode: "0101 100S SSSR DDDD", requireSameRegisterPage: true }],
    ['DIVU',  { operands: [OperandType.Register, OperandType.Register], syntax: "DIVU Rs, Rd", opcode: "0101 101S SSSR DDDD", requireSameRegisterPage: true }],
    ['LMO',   { operands: [OperandType.Register, OperandType.Register], syntax: "LMO Rs, Rd", opcode: "0110 101S SSSR DDDD", requireSameRegisterPage: true }],
    ['MODS',  { operands: [OperandType.Register, OperandType.Register], syntax: "MODS Rs, Rd", opcode: "0110 110S SSSR DDDD", requireSameRegisterPage: true }],
    ['MODU',  { operands: [OperandType.Register, OperandType.Register], syntax: "MODU Rs, Rd", opcode: "0110 111S SSSR DDDD", requireSameRegisterPage: true }],
    ['MPYS',  { operands: [OperandType.Register, OperandType.Register], syntax: "MPYS Rs, Rd", opcode: "0101 110S SSSR DDDD", requireSameRegisterPage: true }],
    ['MPYU',  { operands: [OperandType.Register, OperandType.Register], syntax: "MPYU Rs, Rd", opcode: "0101 111S SSSR DDDD", requireSameRegisterPage: true }],
    ['NEG',   { operands: [OperandType.Register], syntax: "NEG Rd", opcode: "0000 0011 101R DDDD", hasOptionalFieldSize: true }],
    ['NEGB',  { operands: [OperandType.Register], syntax: "NEGB Rd", opcode: "0000 0011 110R DDDD" }],
    ['NOT',   { operands: [OperandType.Register], syntax: "NOT Rd", opcode: "0000 0011 111R DDDD", hasOptionalFieldSize: true }],
    ['OR',    { operands: [OperandType.Register, OperandType.Register], syntax: "OR Rs, Rd", opcode: "0101 010S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['ORI',   { operands: [OperandType.Immediate, OperandType.Register], syntax: "ORI IL, Rd", opcode: "0000 1011 101R DDDD", hasOptionalFieldSize: true }],
    ['SETC',  { operands: [], syntax: "SETC", opcode: "0000 1101 1110 0000" }],
    ['SEXT',  { operands: [OperandType.Register], syntax: "SEXT Rd, F", opcode: "0000 1101 F100 000R DDDD", hasOptionalFieldSize: true }],
    ['SUB',   { operands: [OperandType.Register, OperandType.Register], syntax: "SUB Rs, Rd", opcode: "0100 010S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['SUBB',  { operands: [OperandType.Register, OperandType.Register], syntax: "SUBB Rs, Rd", opcode: "0100 011S SSSR DDDD", requireSameRegisterPage: true }],
    ['SUBI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "SUBI IW/IL, Rd", opcode: "IW: 0000 0011 0101 0R DDDD\nIL: 0000 0011 0111 0R DDDD", hasOptionalFieldSize: true }],
    ['SUBK',  { operands: [OperandType.Constant, OperandType.Register], syntax: "SUBK K, Rd", opcode: "0001 01KK KKKR DDDD", hasOptionalFieldSize: true }],
    ['SUBXY', { operands: [OperandType.Register, OperandType.Register], syntax: "SUBXY Rs, Rd", opcode: "1110 001S SSSR DDDD", requireSameRegisterPage: true }],
    ['XOR',   { operands: [OperandType.Register, OperandType.Register], syntax: "XOR Rs, Rd", opcode: "0101 011S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true }],
    ['XORI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "XORI IL, Rd", opcode: "0000 1011 1101 0R DDDD", hasOptionalFieldSize: true }],
    ['ZEXT',  { operands: [OperandType.Register], syntax: "ZEXT Rd, F", opcode: "0000 1101 F100 001R DDDD", hasOptionalFieldSize: true }],
    ['MOVE',  { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "MOVE src, dest", opcode: "(various)", hasOptionalFieldSize: true }],
    ['MMFM',  { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "MMFM Rs, [List]", opcode: "0000 1001 101R DDDD" }],
    ['MMTM',  { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "MMTM Rs, [List]", opcode: "0000 1001 100R DDDD" }],
    ['MOVB',  { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "MOVB src, dest", opcode: "(various)" }],
    ['MOVI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "MOVI IW/IL, Rd", opcode: "IW: 0000 1001 110R DDDD\nIL: 0000 1001 111R DDDD" }],
    ['MOVK',  { operands: [OperandType.Constant, OperandType.Register], syntax: "MOVK K, Rd", opcode: "0001 10KK KKKR DDDD" }],
    ['MOVX',  { operands: [OperandType.Register, OperandType.Register], syntax: "MOVX Rs, Rd", opcode: "1110 11MS SSSR DDDD", requireSameRegisterPage: true }],
    ['MOVY',  { operands: [OperandType.Register, OperandType.Register], syntax: "MOVY Rs, Rd", opcode: "1110 11MS SSSR DDDD", requireSameRegisterPage: true }],
    ['CPW',    { operands: [OperandType.Register, OperandType.Register], syntax: "CPW Rs, Rd", opcode: "1110 011S SSSR DDDD", requireSameRegisterPage: true }],
    ['CVXYL',  { operands: [OperandType.Register, OperandType.Register], syntax: "CVXYL Rs, Rd", opcode: "1110 100S SSSR DDDD", requireSameRegisterPage: true }],
    ['DRAV',   { operands: [OperandType.Register, OperandType.Register], syntax: "DRAV Rs, Rd", opcode: "1111 011S SSSR DDDD", requireSameRegisterPage: true }],
    ['FILL',   { operands: [OperandType.Label], syntax: "FILL L | FILL XY", opcode: "L: 0000 1111 1100 0000\nXY: 0000 1111 1110 0000" }],
    ['LINE',   { operands: [], syntax: "LINE [0|1]", opcode: "0: 0DF1Ah\n1: 0DF9Ah" }],
    ['PIXBLT', { operands: [OperandType.Label, OperandType.Label], syntax: "PIXBLT mode, mode", opcode: "(various)" }],
    ['PIXT',   { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "PIXT src, dest", opcode: "(various)" }],
    ['CALL',  { operands: [OperandType.RegisterOrLabel], syntax: "CALL Rs | CALL Label", opcode: "0000 1001 001R DDDD" }],
    ['CALLA', { operands: [OperandType.Label], syntax: "CALLA Address", opcode: "0000 1101 0101 1111" }],
    ['CALLR', { operands: [OperandType.Label], syntax: "CALLR Address", opcode: "0000 1101 0011 1111" }],
    ['DINT',  { operands: [], syntax: "DINT", opcode: "0000 0011 0110 0000" }],
    ['EINT',  { operands: [], syntax: "EINT", opcode: "0000 1101 0110 0000" }],
    ['EMU',   { operands: [], syntax: "EMU", opcode: "0000 0001 0000 0000" }],
    ['EXGF',  { operands: [OperandType.Register, OperandType.Constant], syntax: "EXGF Rd, F", opcode: "1101 01F1 F00R DDDD" }],
    ['EXGPC', { operands: [OperandType.Register], syntax: "EXGPC Rd", opcode: "0000 0001 001R DDDD" }],
    ['GETPC', { operands: [OperandType.Register], syntax: "GETPC Rd", opcode: "0000 0001 010R DDDD" }],
    ['GETST', { operands: [OperandType.Register], syntax: "GETST Rd", opcode: "0000 0001 100R DDDD" }],
    ['NOP',   { operands: [], syntax: "NOP", opcode: "0000 0011 0000 0000" }],
    ['POPST', { operands: [], syntax: "POPST", opcode: "0000 0001 1100 0000" }],
    ['PUSHST',{ operands: [], syntax: "PUSHST", opcode: "0000 0001 1110 0000" }],
    ['PUTST', { operands: [OperandType.Register], syntax: "PUTST Rs", opcode: "0000 0001 101R DDDD" }],
    ['RETI',  { operands: [], syntax: "RETI", opcode: "0000 1001 0100 0000" }],
    ['RETS',  { operands: [OperandType.Constant], syntax: "RETS [N]", opcode: "0000 1001 011N NNNN" }],
    ['REV',   { operands: [OperandType.Register], syntax: "REV Rd", opcode: "0000 0000 001R DDDD" }],
    ['SETF',  { operands: [OperandType.Constant, OperandType.Constant, OperandType.Constant], syntax: "SETF FS, FE, F", opcode: "0000 0111 01(FS)(FE)(F)0 0000" }],
    ['TRAP',  { operands: [OperandType.Constant], syntax: "TRAP N", opcode: "0000 1001 000N NNNN" }],
    ['DSJ',   { operands: [OperandType.Register, OperandType.Label], syntax: "DSJ Rd, Address", opcode: "0000 1101 1000 DDDD" }],
    ['DSJEQ', { operands: [OperandType.Register, OperandType.Label], syntax: "DSJEQ Rd, Address", opcode: "0000 1101 1010 DDDD" }],
    ['DSJNE', { operands: [OperandType.Register, OperandType.Label], syntax: "DSJNE Rd, Address", opcode: "0000 1101 1100 DDDD" }],
    ['DSJS',  { operands: [OperandType.Register, OperandType.Label], syntax: "DSJS Rd, Address", opcode: "0011 1Dxx xxx0 DDDD" }],
    ['JUMP',  { operands: [OperandType.Register], syntax: "JUMP Rs", opcode: "0000 0001 011R SSSS" }],
    ...['JRP', 'JRLS', 'JRLT', 'JRLE', 'JREQ', 'JRNE', 'JRGT', 'JRGE', 'JRHI', 'JRCC', 'JRCS', 'JRVC', 'JRVS', 'JRPL', 'JRMI', 'JRUC'].map(j => [j, {operands: [OperandType.Label], syntax: `${j} Address`, opcode: "1100 cccc oooooooo"}] as [string, InstructionRule]),
    ['JR', {operands: [OperandType.Label], syntax: `JR Address`, opcode: "1100 cccc oooooooo"}],
    ...['JAP', 'JALS', 'JALT', 'JALE', 'JAEQ', 'JANE', 'JAGT', 'JAGE', 'JAHI', 'JACC', 'JACS', 'JAVC', 'JAVS', 'JAPL', 'JAMI', 'JAUC'].map(j => [j, {operands: [OperandType.Label], syntax: `${j} Address`, opcode: "1100 cccc 10000000"}] as [string, InstructionRule]),
    ['JA', {operands: [OperandType.Label], syntax: `JA Address`, opcode: "1100 cccc 10000000"}],
    ...['RL', 'SLA', 'SLL', 'SRA', 'SRL'].map(s => [s, {operands: [OperandType.RegisterOrConstant, OperandType.Register], syntax: `${s} K/Rs, Rd`, opcode: `K: 001x xxKK KKK0 DDDD\nRs: 0110 xx0S SSSR DDDD`, hasOptionalFieldSize: true, requireSameRegisterPage: true }] as [string, InstructionRule])
]);

const KNOWN_INSTRUCTIONS = new Set(INSTRUCTION_RULES.keys());

const KNOWN_DIRECTIVES = new Set([
    '.set', '.equ', '.word', '.long', '.string', '.asciiz', '.byte', '.field', '.sint', '.float',
    '.sect', '.bss', '.text', '.data', '.align', '.space',
    '.global', '.globl',
    '.end', '.org', 
    '.def', '.ref', '.newblock', '.cdef', 'endcdef', '.clink', '.cstruct', '.endstruct',
    '.struct', '.union', '.tag', '.eval', '.emsg', '.wmsg', '.fclist', '.fcnolist',
    '.drlist', '.drnolist', '.mlist', '.mnolist', '.sslist', '.ssnolist', '.var',
    '.label', '.version', '.length', '.width', 'option',
    '.if', '.else', '.endif', '.include'
]);

const DIAGNOSTIC_COLLECTION = vscode.languages.createDiagnosticCollection('tms34010');

export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.languages.registerHoverProvider('tms-assembly', {
            provideHover(document, position, token) {
                const range = document.getWordRangeAtPosition(position);
                if (!range) { return null; }
                const word = document.getText(range).toUpperCase();
                
                if (INSTRUCTION_RULES.has(word)) {
                    const rule = INSTRUCTION_RULES.get(word)!;
                    const content = new vscode.MarkdownString(`**${word}**\n\n*${rule.syntax}*`);
                    content.appendCodeblock(rule.opcode, 'plaintext');
                    return new vscode.Hover(content, range);
                }

                if (TMS34010_REGISTERS.has(word)) {
                     const content = new vscode.MarkdownString(`**Register:** ${word}`);
                     return new vscode.Hover(content, range);
                }

                return null;
            }
        })
    );
    
    if (vscode.window.activeTextEditor) {
        updateDiagnostics(vscode.window.activeTextEditor.document, DIAGNOSTIC_COLLECTION);
    }
    context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(editor => editor && updateDiagnostics(editor.document, DIAGNOSTIC_COLLECTION)));
    context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(event => updateDiagnostics(event.document, DIAGNOSTIC_COLLECTION)));
    context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(doc => DIAGNOSTIC_COLLECTION.delete(doc.uri)));
}

function updateDiagnostics(doc: vscode.TextDocument, collection: vscode.DiagnosticCollection): void {
    if (doc.languageId !== 'tms-assembly') {
        return;
    }

    const diagnostics: vscode.Diagnostic[] = [];
    const definedSymbols = new Map<string, vscode.Range>();
    
    // First pass: find all symbol definitions
    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const text = line.text;
        
        const symbolRange = (symbol: string) => {
            const index = text.indexOf(symbol);
            return index >= 0 ? new vscode.Range(lineIndex, index, lineIndex, index + symbol.length) : null;
        };

        const labelMatch = text.match(/^\s*([a-zA-Z_][a-zA-Z0-9_]*):/);
        if (labelMatch) {
            const labelName = labelMatch[1].toUpperCase();
            const currentRange = symbolRange(labelName);
            if (currentRange) {
                if (definedSymbols.has(labelName)) {
                    const originalRange = definedSymbols.get(labelName)!;
                    const diagnostic = new vscode.Diagnostic(currentRange, `Duplicate symbol definition: '${labelMatch[1]}'`, vscode.DiagnosticSeverity.Error);
                    diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(doc.uri, originalRange), 'First defined here')];
                    diagnostics.push(diagnostic);
                } else {
                    definedSymbols.set(labelName, currentRange);
                }
            }
        }
        
        const equateMatch = text.trim().match(/^([a-zA-Z_][a-zA-Z0-9_]+)\s+\.(equ|set)\s+/i);
        if (equateMatch) {
            const equateName = equateMatch[1].toUpperCase();
            const currentRange = symbolRange(equateName);
            if (currentRange) {
                if (definedSymbols.has(equateName)) {
                    const originalRange = definedSymbols.get(equateName)!;
                    const diagnostic = new vscode.Diagnostic(currentRange, `Duplicate symbol definition: '${equateMatch[1]}'`, vscode.DiagnosticSeverity.Error);
                    diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(doc.uri, originalRange), 'First defined here')];
                    diagnostics.push(diagnostic);
                } else {
                    definedSymbols.set(equateName, currentRange);
                }
            }
        }
    }

    // Second pass: validate instructions
    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        let lineWithoutComment = line.text.split(';')[0];
        let text = lineWithoutComment.trim();
        
        if (text.length === 0) { continue; }
        
        text = text.replace(/^\s*([a-zA-Z_][a-zA-Z0-9_]*):/, '').trim();
        if (text.length === 0) { continue; }

        if (text.match(/^[a-zA-Z_][a-zA-Z0-9_]+\s+\.(equ|set)\s+/i)) {
             continue;
        }

        const parts = text.split(/\s+/);
        const mnemonic = parts[0].toUpperCase();
        
        // Special validation for MOVE with predecrement addressing
        const movePreDecMatch = text.match(/^(MOVE)\s+([a-zA-Z0-9]+)\s*,\s*(-\*[a-zA-Z0-9]+)\s*,\s*(\d+)$/i);
        if (movePreDecMatch) {
            const [, , rs, dest, field] = movePreDecMatch;
            if (!isRegister(rs)) {
                 const range = new vscode.Range(lineIndex, lineWithoutComment.indexOf(rs), lineIndex, lineWithoutComment.indexOf(rs) + rs.length);
                 diagnostics.push(new vscode.Diagnostic(range, `Invalid source register '${rs}' for MOVE.`, vscode.DiagnosticSeverity.Error));
            }
            const fieldVal = parseInt(field, 10);
            if (fieldVal < 0 || fieldVal > 31) {
                const range = new vscode.Range(lineIndex, lineWithoutComment.lastIndexOf(field), lineIndex, lineWithoutComment.lastIndexOf(field) + field.length);
                diagnostics.push(new vscode.Diagnostic(range, `Invalid Field Size. Must be between 0 and 31.`, vscode.DiagnosticSeverity.Error));
            }
            continue; // Skip other validation for this specific MOVE form
        }
        
        if (KNOWN_INSTRUCTIONS.has(mnemonic)) {
            const rule = INSTRUCTION_RULES.get(mnemonic)!;
            let operandStr = parts.slice(1).join(' ');
            
            if (rule.hasOptionalFieldSize) {
                const fieldSizeMatch = operandStr.match(/(.*),\s*(\d+)$/);
                if (fieldSizeMatch) {
                    operandStr = fieldSizeMatch[1];
                    const fieldSize = parseInt(fieldSizeMatch[2], 10);
                    if (fieldSize < 0 || fieldSize > 31) {
                         let fsStartIndex = lineWithoutComment.lastIndexOf(fieldSizeMatch[2]);
                         if (fsStartIndex === -1) fsStartIndex = line.firstNonWhitespaceCharacterIndex;
                         const range = new vscode.Range(lineIndex, fsStartIndex, lineIndex, fsStartIndex + fieldSizeMatch[2].length);
                         diagnostics.push(new vscode.Diagnostic(range, `Invalid Field Size. Must be between 0 and 31.`, vscode.DiagnosticSeverity.Error));
                    }
                }
            }
            
            const specifierMatch = operandStr.match(/(.*),\s*([WL])$/i);
            if (specifierMatch) {
                operandStr = specifierMatch[1];
            }
            
            const operands = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
            
            if (rule.operands.length !== operands.length) {
                 if (!(rule.operands.length - 1 === operands.length && rule.operands[rule.operands.length-1] === OperandType.Constant)) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, lineWithoutComment.trimRight().length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid operand count for ${mnemonic}. Expected ${rule.operands.length}, but got ${operands.length}.`, vscode.DiagnosticSeverity.Error));
                    continue;
                }
            }
            
            if (rule.requireSameRegisterPage && operands.length === 2) {
                const reg1 = operands[0].toUpperCase();
                const reg2 = operands[1].toUpperCase();

                if ((A_REGISTERS.has(reg1) && B_REGISTERS.has(reg2)) || (B_REGISTERS.has(reg1) && A_REGISTERS.has(reg2))) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, lineWithoutComment.trimRight().length);
                    diagnostics.push(new vscode.Diagnostic(range, `${mnemonic} requires both registers to be in the same file (A or B).`, vscode.DiagnosticSeverity.Error));
                }
            }

            for (let i = 0; i < operands.length; i++) {
                const operandValue = operands[i];
                const expectedType = rule.operands[i];
                let isValid = false;

                const checkLabel = (op: string) => isLabelFormat(op) && definedSymbols.has(op.toUpperCase());

                switch (expectedType) {
                    case OperandType.Register:
                        isValid = isRegister(operandValue);
                        break;
                    case OperandType.Immediate:
                        isValid = isConstant(operandValue) || checkLabel(operandValue);
                        break;
                    case OperandType.Constant:
                        isValid = isConstant(operandValue);
                        break;
                    case OperandType.Label:
                        isValid = checkLabel(operandValue);
                        if (isLabelFormat(operandValue) && !isValid) {
                            const range = new vscode.Range(lineIndex, lineWithoutComment.indexOf(operandValue), lineIndex, lineWithoutComment.indexOf(operandValue) + operandValue.length);
                            diagnostics.push(new vscode.Diagnostic(range, `Undefined label: '${operandValue}'`, vscode.DiagnosticSeverity.Error));
                            continue; // Skip generic error
                        }
                        break;
                    case OperandType.Address:
                        isValid = isAddress(operandValue);
                        break;
                    case OperandType.Addressable:
                        isValid = isRegister(operandValue) || isAddress(operandValue);
                        break;
                    case OperandType.RegisterOrConstant:
                        isValid = isRegister(operandValue) || isConstant(operandValue);
                        break;
                    case OperandType.RegisterOrLabel:
                        isValid = isRegister(operandValue) || checkLabel(operandValue);
                        break;
                }
                
                if (!isValid) {
                    let operandStartIndex = lineWithoutComment.lastIndexOf(operandValue);
                    if (operandStartIndex === -1) { operandStartIndex = line.firstNonWhitespaceCharacterIndex; }
                    const range = new vscode.Range(lineIndex, operandStartIndex, lineIndex, operandStartIndex + operandValue.length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid type for operand ${i + 1} of ${mnemonic}.`, vscode.DiagnosticSeverity.Error));
                } else if (isAddress(operandValue)) {
                    const offsetMatch = operandValue.match(/\((.+)\)/);
                    if (offsetMatch) {
                        const offsetValue = offsetMatch[1];
                        if (!isConstant(offsetValue) && !definedSymbols.has(offsetValue.toUpperCase())) {
                            const offsetStartIndex = lineWithoutComment.indexOf(offsetValue);
                            const range = new vscode.Range(lineIndex, offsetStartIndex, lineIndex, offsetStartIndex + offsetValue.length);
                            diagnostics.push(new vscode.Diagnostic(range, `Undefined symbol used in offset: '${offsetValue}'`, vscode.DiagnosticSeverity.Error));
                        }
                    }
                    const atMatch = operandValue.match(/@([a-zA-Z_][a-zA-Z0-9_]*)/);
                    if(atMatch && !isConstant(atMatch[1]) && !definedSymbols.has(atMatch[1].toUpperCase())) {
                        const atStartIndex = lineWithoutComment.indexOf(atMatch[1]);
                        const range = new vscode.Range(lineIndex, atStartIndex, lineIndex, atStartIndex + atMatch[1].length);
                        diagnostics.push(new vscode.Diagnostic(range, `Undefined symbol in address: '${atMatch[1]}'`, vscode.DiagnosticSeverity.Error));
                    }
                }
            }
        } else if (!KNOWN_DIRECTIVES.has(mnemonic.toLowerCase())) {
            let mnemonicStartIndex = lineWithoutComment.indexOf(parts[0]);
             if (mnemonicStartIndex === -1) { mnemonicStartIndex = line.firstNonWhitespaceCharacterIndex; }
            const range = new vscode.Range(lineIndex, mnemonicStartIndex, lineIndex, mnemonicStartIndex + parts[0].length);
            diagnostics.push(new vscode.Diagnostic(range, `Unrecognized instruction or directive: '${parts[0]}'`, vscode.DiagnosticSeverity.Error));
        }
    }
    
    collection.set(doc.uri, diagnostics);
}

export function deactivate() {}
