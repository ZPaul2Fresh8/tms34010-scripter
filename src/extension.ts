import * as vscode from 'vscode';
import * as path from 'path';

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
    Flag,        // Represents a 0 or 1 flag
    FillMode,    // For FILL L or FILL XY
    PixbltMode,  // For PIXBLT operands L, XY, or B
    RegisterList, // For MMTM/MMFM
    None
}

type SymbolInfo = {
    range: vscode.Range;
    value: string | null;
    type: 'label' | 'equ' | 'set';
};

// Define the structure for an instruction's syntax rule
interface InstructionRule {
    operands: OperandType[];
    syntax: string;
    opcode: string;
    description: string;
    hasOptionalFieldSize?: boolean;
    requireSameRegisterPage?: boolean;
    minOperands?: number;
}

// --- Validation Helper Functions ---

const A_REGISTERS_ORDERED = ['A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 'A12', 'A13', 'A14', 'SP'];
const B_REGISTERS_ORDERED = ['B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'FP'];

const A_REGISTERS = new Set(A_REGISTERS_ORDERED);
const B_REGISTERS = new Set(B_REGISTERS_ORDERED);
const OTHER_REGISTERS = new Set(['ST', 'PC', 'IOSTAT', 'CTRL1', 'CTRL2', 'HSTADR', 'HSTDATA', 'HSTCTL', 'INTPEND', 'INTENB', 'DPYCTL', 'DPYSTRT', 'DPYADR', 'VCOUNT', 'HCOUNT', 'PFILL', 'PLINE', 'CONVSP', 'CONVDP', 'PSIZE', 'PMOVE', 'SADDR', 'SCOUNT', 'DADDR', 'DCOUNT', 'OFFSET', 'WINDOW', 'WSTART', 'WEND', 'DYDX', 'COLOR0', 'COLOR1']);
const TMS34010_REGISTERS = new Set([...A_REGISTERS, ...B_REGISTERS, ...OTHER_REGISTERS]);

const isRegister = (op: string): boolean => TMS34010_REGISTERS.has(op.toUpperCase());

const isConstant = (op: string): boolean => {
    let value = op.toUpperCase();
    if (value.startsWith('#')) {
        value = value.substring(1);
    }

    // Ensure constant is within range 0-31
    if (parseInt(value) > 31) return false;

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

const isImmediate = (op: string): boolean => {
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

const isFlag = (op: string): boolean => op === '0' || op === '1';

const isFillMode = (op: string): boolean => {
    const upperOp = op.toUpperCase();
    return upperOp === 'L' || upperOp === 'XY';
};

const isPixbltMode = (op: string): boolean => {
    const upperOp = op.toUpperCase();
    return upperOp === 'L' || upperOp === 'XY' || upperOp === 'B';
};

// Checks if a string has the format of a label (doesn't check if defined)
const isLabelFormat = (op: string): boolean => {
    return /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(op) && !isRegister(op);
};

const isAddress = (op: string): boolean => {
    op = op.toUpperCase();
    // Absolute address: @>FFFF, @label
    if (op.startsWith('@')) {
        const subOperand = op.substring(1);
        return isLabelFormat(subOperand) || isImmediate(subOperand);
    }
    
    // Pre-decrement: -*A0
    if (op.startsWith('-*')) {
        const reg = op.substring(2);
        return A_REGISTERS.has(reg) || B_REGISTERS.has(reg);
    }
    
    if (op.startsWith('*')) {
        // Post-increment: *A0+
        if (op.endsWith('+')) {
            const reg = op.substring(1, op.length - 1);
            return A_REGISTERS.has(reg) || B_REGISTERS.has(reg);
        }
        
        // With offset: *A0(10) or *A0(MY_LABEL)
        const offsetMatch = op.match(/^\*(A[0-9]{1,2}|B[0-9]{1,2}|SP|FP)\((.+)\)$/);
        if (offsetMatch) {
            return true; 
        }
        
        // Simple indirect: *A0
        const reg = op.substring(1);
        return A_REGISTERS.has(reg) || B_REGISTERS.has(reg);
    }
    
    return false;
};


// A structured map of instructions and their validation rules
const INSTRUCTION_RULES: Map<string, InstructionRule> = new Map([
    // Arithmetic, Logical, Compare
    ['ABS',   { operands: [OperandType.Register], syntax: "ABS Rd", opcode: "0000 0011 100R DDDD", description: "Store absolute value of a register." }],
    ['ADD',   { operands: [OperandType.Register, OperandType.Register], syntax: "ADD Rs, Rd", opcode: "0100 000S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Add source register to destination register." }],
    ['ADDC',  { operands: [OperandType.Register, OperandType.Register], syntax: "ADDC Rs, Rd", opcode: "0100 001S SSSR DDDD", requireSameRegisterPage: true, description: "Add registers with carry." }],
    ['ADDI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "ADDI IW/IL, Rd", opcode: "IW: 0000 1011 000R DDDD\nIL: 0000 1011 001R DDDD", hasOptionalFieldSize: true, description: "Add immediate value to register." }],
    ['ADDK',  { operands: [OperandType.Constant, OperandType.Register], syntax: "ADDK K, Rd", opcode: "0001 00KK KKKR DDDD", hasOptionalFieldSize: true, description: "Add constant (1-32) to register." }],
    ['ADDXY', { operands: [OperandType.Register, OperandType.Register], syntax: "ADDXY Rs, Rd", opcode: "1110 000S SSSR DDDD", requireSameRegisterPage: true, description: "Add corresponding X and Y halves of two registers." }],
    ['AND',   { operands: [OperandType.Register, OperandType.Register], syntax: "AND Rs, Rd", opcode: "0101 000S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Logical AND of two registers." }],
    ['ANDI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "ANDI IL, Rd", opcode: "0000 1011 100R DDDD", hasOptionalFieldSize: true, description: "Logical AND of immediate value and register." }],
    ['ANDN',  { operands: [OperandType.Register, OperandType.Register], syntax: "ANDN Rs, Rd", opcode: "0101 001S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Logical AND of NOT source and destination." }],
    ['ANDNI', { operands: [OperandType.Immediate, OperandType.Register], syntax: "ANDNI IL, Rd", opcode: "0000 1011 100R DDDD", hasOptionalFieldSize: true, description: "Logical AND of NOT immediate value and destination." }],
    ['BTST',  { operands: [OperandType.RegisterOrConstant, OperandType.Register], syntax: "BTST K/Rs, Rd", opcode: "K: 0000 0111 01~K KKKR DDDD\nRs: 0100 101S SSSR DDDD", description: "Test a bit of a register." }],
    ['CLR',   { operands: [OperandType.Register], syntax: "CLR Rd", opcode: "0101 0110 0R DDDD", hasOptionalFieldSize: true, description: "Clear a register to zero." }],
    ['CLRC',  { operands: [], syntax: "CLRC", opcode: "0000 0011 0010 0000", description: "Clear the Carry (C) bit in the status register." }],
    ['CMP',   { operands: [OperandType.Register, OperandType.Register], syntax: "CMP Rs, Rd", opcode: "0100 100S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Compare two registers." }],
    ['CMPI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "CMPI IW/IL, Rd", opcode: "IW: 0000 0010 1101 0R DDDD\nIL: 0000 0010 1111 0R DDDD", hasOptionalFieldSize: true, description: "Compare register with immediate value." }],
    ['CMPXY', { operands: [OperandType.Register, OperandType.Register], syntax: "CMPXY Rs, Rd", opcode: "1110 010S SSSR DDDD", requireSameRegisterPage: true, description: "Compare X and Y halves of two registers." }],
    ['DEC',   { operands: [OperandType.Register], syntax: "DEC Rd", opcode: "0001 0100 001R DDDD", description: "Decrement a register." }],
    ['DIVS',  { operands: [OperandType.Register, OperandType.Register], syntax: "DIVS Rs, Rd", opcode: "0101 100S SSSR DDDD", requireSameRegisterPage: true, description: "Signed divide." }],
    ['DIVU',  { operands: [OperandType.Register, OperandType.Register], syntax: "DIVU Rs, Rd", opcode: "0101 101S SSSR DDDD", requireSameRegisterPage: true, description: "Unsigned divide." }],
    ['LMO',   { operands: [OperandType.Register, OperandType.Register], syntax: "LMO Rs, Rd", opcode: "0110 101S SSSR DDDD", requireSameRegisterPage: true, description: "Find leftmost one." }],
    ['MODS',  { operands: [OperandType.Register, OperandType.Register], syntax: "MODS Rs, Rd", opcode: "0110 110S SSSR DDDD", requireSameRegisterPage: true, description: "Signed modulo." }],
    ['MODU',  { operands: [OperandType.Register, OperandType.Register], syntax: "MODU Rs, Rd", opcode: "0110 111S SSSR DDDD", requireSameRegisterPage: true, description: "Unsigned modulo." }],
    ['MPYS',  { operands: [OperandType.Register, OperandType.Register], syntax: "MPYS Rs, Rd", opcode: "0101 110S SSSR DDDD", requireSameRegisterPage: true, description: "Signed multiply." }],
    ['MPYU',  { operands: [OperandType.Register, OperandType.Register], syntax: "MPYU Rs, Rd", opcode: "0101 111S SSSR DDDD", requireSameRegisterPage: true, description: "Unsigned multiply." }],
    ['NEG',   { operands: [OperandType.Register], syntax: "NEG Rd", opcode: "0000 0011 101R DDDD", hasOptionalFieldSize: true, description: "Negate a register." }],
    ['NEGB',  { operands: [OperandType.Register], syntax: "NEGB Rd", opcode: "0000 0011 110R DDDD", description: "Negate register with borrow." }],
    ['NOT',   { operands: [OperandType.Register], syntax: "NOT Rd", opcode: "0000 0011 111R DDDD", hasOptionalFieldSize: true, description: "Logical NOT of a register." }],
    ['OR',    { operands: [OperandType.Register, OperandType.Register], syntax: "OR Rs, Rd", opcode: "0101 010S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Logical OR of two registers." }],
    ['ORI',   { operands: [OperandType.Immediate, OperandType.Register], syntax: "ORI IL, Rd", opcode: "0000 1011 101R DDDD", hasOptionalFieldSize: true, description: "Logical OR of immediate value and register." }],
    ['SETC',  { operands: [], syntax: "SETC", opcode: "0000 1101 1110 0000", description: "Set the Carry (C) bit in the status register." }],
    ['SEXT',  { operands: [OperandType.Register, OperandType.Flag], syntax: "SEXT Rd, F", opcode: "0000 1101 F100 000R DDDD", minOperands: 1, description: "Sign extend a field within a register." }],
    ['SUB',   { operands: [OperandType.Register, OperandType.Register], syntax: "SUB Rs, Rd", opcode: "0100 010S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Subtract source register from destination." }],
    ['SUBB',  { operands: [OperandType.Register, OperandType.Register], syntax: "SUBB Rs, Rd", opcode: "0100 011S SSSR DDDD", requireSameRegisterPage: true, description: "Subtract registers with borrow." }],
    ['SUBI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "SUBI IW/IL, Rd", opcode: "IW: 0000 0011 0101 0R DDDD\nIL: 0000 0011 0111 0R DDDD", hasOptionalFieldSize: true, description: "Subtract immediate value from register." }],
    ['SUBK',  { operands: [OperandType.Constant, OperandType.Register], syntax: "SUBK K, Rd", opcode: "0001 01KK KKKR DDDD", hasOptionalFieldSize: true, description: "Subtract constant (1-32) from register." }],
    ['SUBXY', { operands: [OperandType.Register, OperandType.Register], syntax: "SUBXY Rs, Rd", opcode: "1110 001S SSSR DDDD", requireSameRegisterPage: true, description: "Subtract corresponding X and Y halves of two registers." }],
    ['XOR',   { operands: [OperandType.Register, OperandType.Register], syntax: "XOR Rs, Rd", opcode: "0101 011S SSSR DDDD", hasOptionalFieldSize: true, requireSameRegisterPage: true, description: "Logical XOR of two registers." }],
    ['XORI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "XORI IL, Rd", opcode: "0000 1011 1101 0R DDDD", hasOptionalFieldSize: true, description: "Logical XOR of immediate value and register." }],
    ['ZEXT',  { operands: [OperandType.Register, OperandType.Flag], syntax: "ZEXT Rd, F", opcode: "0000 1101 F100 001R DDDD", minOperands: 1, description: "Zero extend a field within a register." }],
    ['MOVE',  { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "MOVE src, dest", opcode: "(various)", hasOptionalFieldSize: true, description: "Move data between registers and/or memory." }],
    ['MMFM',  { operands: [OperandType.Register, OperandType.RegisterList], syntax: "MMFM Rp, register_list", opcode: "0000 1001 101R DDDD", description: "Move multiple registers from memory." }],
    ['MMTM',  { operands: [OperandType.Register, OperandType.RegisterList], syntax: "MMTM Rp, register_list", opcode: "0000 1001 100R DDDD", description: "Move multiple registers to memory." }],
    ['MOVB',  { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "MOVB src, dest", opcode: "(various)", description: "Move a byte between registers and/or memory." }],
    ['MOVI',  { operands: [OperandType.Immediate, OperandType.Register], syntax: "MOVI IW/IL, Rd", hasOptionalFieldSize:true, opcode: "IW: 0000 1001 110R DDDD\nIL: 0000 1001 111R DDDD", description: "Move an immediate value into a register." }],
    ['MOVK',  { operands: [OperandType.Constant, OperandType.Register], syntax: "MOVK K, Rd", opcode: "0001 10KK KKKR DDDD", description: "Move a constant (1-32) into a register." }],
    ['MOVX',  { operands: [OperandType.Register, OperandType.Register], syntax: "MOVX Rs, Rd", opcode: "1110 11MS SSSR DDDD", requireSameRegisterPage: true, description: "Move the X-half of a register." }],
    ['MOVY',  { operands: [OperandType.Register, OperandType.Register], syntax: "MOVY Rs, Rd", opcode: "1110 11MS SSSR DDDD", requireSameRegisterPage: true, description: "Move the Y-half of a register." }],
    ['CPW',    { operands: [OperandType.Register, OperandType.Register], syntax: "CPW Rs, Rd", opcode: "1110 011S SSSR DDDD", requireSameRegisterPage: true, description: "Compare point to window." }],
    ['CVXYL',  { operands: [OperandType.Register, OperandType.Register], syntax: "CVXYL Rs, Rd", opcode: "1110 100S SSSR DDDD", requireSameRegisterPage: true, description: "Convert XY address to linear address." }],
    ['DRAV',   { operands: [OperandType.Register, OperandType.Register], syntax: "DRAV Rs, Rd", opcode: "1111 011S SSSR DDDD", requireSameRegisterPage: true, description: "Draw and advance." }],
    ['FILL',   { operands: [OperandType.FillMode], syntax: "FILL L | FILL XY", opcode: "L: 0000 1111 1100 0000\nXY: 0000 1111 1110 0000", description: "Fill a pixel array." }],
    ['LINE',   { operands: [OperandType.Flag], syntax: "LINE [0|1]", opcode: "0: 0DF1Ah\n1: 0DF9Ah", minOperands: 0, description: "Initiate a line draw operation." }],
    ['PIXBLT', { operands: [OperandType.PixbltMode, OperandType.PixbltMode], syntax: "PIXBLT mode, mode", opcode: "(various)", description: "Pixel Block Transfer." }],
    ['PIXT',   { operands: [OperandType.Addressable, OperandType.Addressable], syntax: "PIXT src, dest", opcode: "(various)", description: "Pixel Transfer." }],
    ['CALL',  { operands: [OperandType.RegisterOrLabel], syntax: "CALL Rs | CALL Label", opcode: "0000 1001 001R DDDD", description: "Call a subroutine." }],
    ['CALLA', { operands: [OperandType.Label], syntax: "CALLA Address", opcode: "0000 1101 0101 1111", description: "Call subroutine at an absolute address." }],
    ['CALLR', { operands: [OperandType.Label], syntax: "CALLR Address", opcode: "0000 1101 0011 1111", description: "Call subroutine at a relative address." }],
    ['DINT',  { operands: [], syntax: "DINT", opcode: "0000 0011 0110 0000", description: "Disable interrupts." }],
    ['EINT',  { operands: [], syntax: "EINT", opcode: "0000 1101 0110 0000", description: "Enable interrupts." }],
    ['EMU',   { operands: [], syntax: "EMU", opcode: "0000 0001 0000 0000", description: "Initiate emulation." }],
    ['EXGF',  { operands: [OperandType.Register, OperandType.Flag], syntax: "EXGF Rd, F", opcode: "1101 01F1 F00R DDDD", description: "Exchange field size." }],
    ['EXGPC', { operands: [OperandType.Register], syntax: "EXGPC Rd", opcode: "0000 0001 001R DDDD", description: "Exchange Program Counter with a register." }],
    ['GETPC', { operands: [OperandType.Register], syntax: "GETPC Rd", opcode: "0000 0001 010R DDDD", description: "Get the value of the Program Counter." }],
    ['GETST', { operands: [OperandType.Register], syntax: "GETST Rd", opcode: "0000 0001 100R DDDD", description: "Get the value of the Status Register." }],
    ['NOP',   { operands: [], syntax: "NOP", opcode: "0000 0011 0000 0000", description: "No operation." }],
    ['POPST', { operands: [], syntax: "POPST", opcode: "0000 0001 1100 0000", description: "Pop the Status Register from the stack." }],
    ['PUSHST',{ operands: [], syntax: "PUSHST", opcode: "0000 0001 1110 0000", description: "Push the Status Register onto the stack." }],
    ['PUTST', { operands: [OperandType.Register], syntax: "PUTST Rs", opcode: "0000 0001 101R DDDD", description: "Copy a register's value to the Status Register." }],
    ['RETI',  { operands: [], syntax: "RETI", opcode: "0000 1001 0100 0000", description: "Return from interrupt." }],
    ['RETS',  { operands: [OperandType.Constant], syntax: "RETS [N]", opcode: "0000 1001 011N NNNN", minOperands: 0, description: "Return from subroutine." }],
    ['REV',   { operands: [OperandType.Register], syntax: "REV Rd", opcode: "0000 0000 001R DDDD", description: "Get the TMS34010 revision level." }],
    ['SETF',  { operands: [OperandType.Constant, OperandType.Constant, OperandType.Constant], syntax: "SETF FS, FE, F", opcode: "0000 0111 01(FS)(FE)(F)0 0000", description: "Set the field parameters." }],
    ['TRAP',  { operands: [OperandType.Constant], syntax: "TRAP N", opcode: "0000 1001 000N NNNN", description: "Software interrupt." }],
    ['DSJ',   { operands: [OperandType.Register, OperandType.Label], syntax: "DSJ Rd, Address", opcode: "0000 1101 1000 DDDD", description: "Decrement and skip if not zero." }],
    ['DSJEQ', { operands: [OperandType.Register, OperandType.Label], syntax: "DSJEQ Rd, Address", opcode: "0000 1101 1010 DDDD", description: "Decrement and skip if equal." }],
    ['DSJNE', { operands: [OperandType.Register, OperandType.Label], syntax: "DSJNE Rd, Address", opcode: "0000 1101 1100 DDDD", description: "Decrement and skip if not equal." }],
    ['DSJS',  { operands: [OperandType.Register, OperandType.Label], syntax: "DSJS Rd, Address", opcode: "0011 1Dxx xxx0 DDDD", description: "Decrement and skip short." }],
    ['JUMP',  { operands: [OperandType.Register], syntax: "JUMP Rs", opcode: "0000 0001 011R SSSS", description: "Jump to the address in a register." }],
    ...['JRP', 'JRLS', 'JRLT', 'JRLE', 'JREQ', 'JRNE', 'JRGT', 'JRGE', 'JRHI', 'JRCC', 'JRCS', 'JRVC', 'JRVS', 'JRPL', 'JRMI', 'JRUC'].map(j => [j, {operands: [OperandType.Label], syntax: `${j} Address`, opcode: "1100 cccc oooooooo", description: `Jump relative if condition '${j.substring(2)}' is met.`}] as [string, InstructionRule]),
    ['JR', {operands: [OperandType.Label], syntax: `JR Address`, opcode: "1100 cccc oooooooo", description: "Jump relative unconditionally."}],
    ...['JAP', 'JALS', 'JALT', 'JALE', 'JAEQ', 'JANE', 'JAGT', 'JAGE', 'JAHI', 'JACC', 'JACS', 'JAVC', 'JAVS', 'JAPL', 'JAMI', 'JAUC', 'JA'].map(j => [j, {operands: [OperandType.Label], syntax: `${j} Address`, opcode: "1100 cccc 10000000", description: `Jump absolute if condition '${j.substring(2)}' is met.`}] as [string, InstructionRule]),
    ...['RL', 'SLA', 'SLL', 'SRA', 'SRL'].map(s => [s, {operands: [OperandType.RegisterOrConstant, OperandType.Register], syntax: `${s} K/Rs, Rd`, opcode: `K: 001x xxKK KKK0 DDDD\nRs: 0110 xx0S SSSR DDDD`, hasOptionalFieldSize: true, requireSameRegisterPage: true, description: `Shift or rotate a register.` }] as [string, InstructionRule])
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
    '.if', '.else', '.endif', '.include', '.file', '.title'
]);

const DIAGNOSTIC_COLLECTION = vscode.languages.createDiagnosticCollection('tms34010');

// A cache to store symbols for each document, used by DefinitionProvider
const documentSymbolsCache = new Map<string, Map<string, SymbolInfo>>();

function resolveSymbols(operand: string, definedSymbols: Map<string, SymbolInfo>): string {
    let resolved = operand;
    // Loop to handle nested aliases, with a limit to prevent infinite loops from circular definitions
    for (let i = 0; i < 10; i++) {
        let changedInThisIteration = false;
        for (const [symbol, info] of definedSymbols.entries()) {
            if (info.value) {
                const regex = new RegExp(`\\b${symbol}\\b`, 'gi');
                if (regex.test(resolved)) {
                    resolved = resolved.replace(regex, info.value);
                    changedInThisIteration = true;
                }
            }
        }
        if (!changedInThisIteration) {
            break; 
        }
    }
    return resolved;
}


export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.languages.registerCompletionItemProvider('tms-assembly', {
            provideCompletionItems(document, position, token, context) {
                const line = document.lineAt(position);
                const linePrefix = line.text.substr(0, position.character);
                const trimmedPrefix = linePrefix.trimLeft();

                // If at the very beginning of a non-whitespace line part
                if (trimmedPrefix.length === linePrefix.length && !trimmedPrefix.includes(' ')) {
                    return Array.from(KNOWN_INSTRUCTIONS).map(instr =>
                        new vscode.CompletionItem(instr, vscode.CompletionItemKind.Keyword)
                    );
                }
                
                const parts = trimmedPrefix.split(/[\s,]+/);
                const mnemonic = parts[0].toUpperCase();
                
                if (INSTRUCTION_RULES.has(mnemonic)) {
                    const rule = INSTRUCTION_RULES.get(mnemonic)!;
                    const commaCount = (linePrefix.match(/,/g) || []).length;
                    
                    const currentOperandIndex = commaCount;
                    if (currentOperandIndex < rule.operands.length) {
                        const expectedType = rule.operands[currentOperandIndex];
                        
                        const createRegisterSuggestions = () => Array.from(TMS34010_REGISTERS).map(reg => new vscode.CompletionItem(reg, vscode.CompletionItemKind.Variable));
                        
                        // Suggest registers if the user just typed '*'
                        if(context.triggerCharacter === '*') {
                            return createRegisterSuggestions();
                        }

                        const createLabelSuggestions = () => {
                            const labels: vscode.CompletionItem[] = [];
                            for (let i = 0; i < document.lineCount; i++) {
                                const text = document.lineAt(i).text;
                                const labelMatch = text.match(/^\s*([a-zA-Z_][a-zA-Z0-9_]*)/);
                                const equateMatch = text.trim().match(/^([a-zA-Z_][a-zA-Z0-9_]+)\s+\.(equ|set)\s+/i);
                                if (labelMatch && !KNOWN_INSTRUCTIONS.has(labelMatch[1].toUpperCase()) && !KNOWN_DIRECTIVES.has(labelMatch[1].toLowerCase())) {
                                    labels.push(new vscode.CompletionItem(labelMatch[1], vscode.CompletionItemKind.Reference));
                                }
                                if (equateMatch) {
                                     labels.push(new vscode.CompletionItem(equateMatch[1], vscode.CompletionItemKind.Constant));
                                }
                            }
                            return labels;
                        };

                        switch (expectedType) {
                            case OperandType.Register:
                            case OperandType.RegisterOrConstant:
                            case OperandType.RegisterOrLabel:
                            case OperandType.Addressable:
                            case OperandType.RegisterList:
                                return createRegisterSuggestions();
                            case OperandType.Label:
                            case OperandType.Immediate:
                                return createLabelSuggestions();
                            case OperandType.Flag:
                                return [new vscode.CompletionItem('0'), new vscode.CompletionItem('1')];
                            case OperandType.FillMode:
                                return [new vscode.CompletionItem('L'), new vscode.CompletionItem('XY')];
                            case OperandType.PixbltMode:
                                return [new vscode.CompletionItem('L'), new vscode.CompletionItem('XY'), new vscode.CompletionItem('B')];
                        }
                    }
                }
                
                return undefined;
            }
        },
        ' ', ',', '*') // Trigger characters
    );
    
    context.subscriptions.push(
        vscode.languages.registerHoverProvider('tms-assembly', {
            provideHover(document, position, token) {
                const range = document.getWordRangeAtPosition(position);
                if (!range) { return null; }
                const word = document.getText(range).toUpperCase();
                
                if (INSTRUCTION_RULES.has(word)) {
                    const rule = INSTRUCTION_RULES.get(word)!;
                    const content = new vscode.MarkdownString(`**${word}**\n\n*${rule.syntax}*\n\n${rule.description}`);
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

    context.subscriptions.push(
        vscode.languages.registerDefinitionProvider('tms-assembly', {
            provideDefinition(document, position, token) {
                const wordRange = document.getWordRangeAtPosition(position);
                if (!wordRange) {
                    return undefined;
                }
                const word = document.getText(wordRange).toUpperCase();
                const symbols = documentSymbolsCache.get(document.uri.toString());

                if (symbols && symbols.has(word)) {
                    return new vscode.Location(document.uri, symbols.get(word)!.range);
                }

                return undefined;
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

async function parseSymbols(doc: vscode.TextDocument, definedSymbols: Map<string, SymbolInfo>, processedFiles: Set<string>, diagnostics: vscode.Diagnostic[]): Promise<void> {
    if (processedFiles.has(doc.uri.toString())) {
        return; // Avoid circular includes
    }
    processedFiles.add(doc.uri.toString());
    
    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const text = line.text;
        
        const symbolRange = (symbol: string) => {
            const index = text.indexOf(symbol);
            return index >= 0 ? new vscode.Range(lineIndex, index, lineIndex, index + symbol.length) : null;
        };

        const trimmedText = text.trim();
        const parts = trimmedText.split(/\s+/);
        const firstWord = parts[0];

        const equateMatch = text.trim().match(/^([a-zA-Z_][a-zA-Z0-9_]+)\s+\.(equ|set)\s+(.+)/i);
        if (equateMatch) {
            const equateName = equateMatch[1].toUpperCase();
            const directiveType = equateMatch[2].toLowerCase() as 'equ' | 'set';
            const value = equateMatch[3];
            const currentRange = symbolRange(equateMatch[1]);
            if (currentRange) {
                const existingSymbol = definedSymbols.get(equateName);
                if (existingSymbol && existingSymbol.type !== 'set') {
                    const originalRange = existingSymbol.range;
                    const diagnostic = new vscode.Diagnostic(currentRange, `Duplicate symbol definition: '${equateMatch[1]}'`, vscode.DiagnosticSeverity.Error);
                    diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(doc.uri, originalRange), 'First defined here')];
                    diagnostics.push(diagnostic);
                } else {
                    definedSymbols.set(equateName, { range: currentRange, value: value, type: directiveType });
                }
            }
        } else if (firstWord && !KNOWN_INSTRUCTIONS.has(firstWord.toUpperCase()) && !KNOWN_DIRECTIVES.has(firstWord.toLowerCase())) {
            const labelName = firstWord.replace(':', '').toUpperCase();
            const currentRange = symbolRange(firstWord.replace(':', ''));
             if (currentRange && !definedSymbols.has(labelName)) {
                definedSymbols.set(labelName, { range: currentRange, value: null, type: 'label'});
            }
        }
        
        const includeMatch = text.trim().match(/^\.include\s+"(.+)"/i);
        if(includeMatch){
            const fileName = includeMatch[1];
            const currentDir = path.dirname(doc.uri.fsPath);
            const includeUri = vscode.Uri.file(path.join(currentDir, fileName));
            try {
                const includedDoc = await vscode.workspace.openTextDocument(includeUri);
                await parseSymbols(includedDoc, definedSymbols, processedFiles, diagnostics);
            } catch {
                 const range = new vscode.Range(lineIndex, line.text.indexOf(fileName), lineIndex, line.text.indexOf(fileName) + fileName.length);
                 diagnostics.push(new vscode.Diagnostic(range, `Included file not found: ${fileName}`, vscode.DiagnosticSeverity.Error));
            }
        }
    }
}

async function updateDiagnostics(doc: vscode.TextDocument, collection: vscode.DiagnosticCollection): Promise<void> {
    if (doc.languageId !== 'tms-assembly') {
        return;
    }

    const diagnostics: vscode.Diagnostic[] = [];
    const definedSymbols = new Map<string, SymbolInfo>();
    const processedFiles = new Set<string>();

    await parseSymbols(doc, definedSymbols, processedFiles, diagnostics);
    documentSymbolsCache.set(doc.uri.toString(), definedSymbols);

    // Second pass: validate instructions
    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        let lineWithoutComment = line.text.split(';')[0];
        let text = lineWithoutComment.trim();
        
        if (text.length === 0) { continue; }
        
        const parts = text.split(/\s+/);
        let mnemonic = parts[0].toUpperCase();
        let operandStr = parts.slice(1).join(' ');

        if(definedSymbols.has(mnemonic.replace(':', ''))){
            mnemonic = (parts[1] || '').toUpperCase();
            operandStr = parts.slice(2).join(' ');
        }
        if(!mnemonic) continue;
        
        if (KNOWN_INSTRUCTIONS.has(mnemonic)) {
            const rule = INSTRUCTION_RULES.get(mnemonic)!;
            
            // Smarter operand parsing
            const operandParts = operandStr.split(',').map(s => s.trim());
            let operands: string[] = [];
            let fieldSize : string | null = null;
            
            if (rule.hasOptionalFieldSize && operandParts.length > rule.operands.length) {
                const lastPart = operandParts[operandParts.length - 1];
                if (lastPart.match(/^\d+$/)) {
                    fieldSize = lastPart;
                    operands = operandParts.slice(0, -1);
                } else {
                    operands = operandParts;
                }
            } else {
                operands = operandParts;
            }

            operands = operands.filter(s => s.length > 0);

            if(fieldSize){
                const fieldVal = parseInt(fieldSize, 10);
                if (fieldVal < 0 || fieldVal > 31) {
                    let fsStartIndex = lineWithoutComment.lastIndexOf(fieldSize);
                    if (fsStartIndex === -1) fsStartIndex = line.firstNonWhitespaceCharacterIndex;
                    const range = new vscode.Range(lineIndex, fsStartIndex, lineIndex, fsStartIndex + fieldSize.length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid Field Size. Must be between 0 and 31.`, vscode.DiagnosticSeverity.Error));
                }
            }
            
            const minOps = rule.minOperands ?? rule.operands.length;
            if (operands.length < minOps || operands.length > rule.operands.length) {
                const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, lineWithoutComment.trimRight().length);
                diagnostics.push(new vscode.Diagnostic(range, `Invalid operand count for ${mnemonic}. Expected ${minOps} to ${rule.operands.length}, but got ${operands.length}.`, vscode.DiagnosticSeverity.Error));
                continue;
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
                const originalOperandValue = operands[i];
                let operandValue = resolveSymbols(originalOperandValue, definedSymbols);
                const expectedType = rule.operands[i];
                let isValid = false;

                const checkLabel = (op: string) => isLabelFormat(op) && definedSymbols.has(op.toUpperCase());

                switch (expectedType) {
                    case OperandType.Register: isValid = isRegister(operandValue); break;
                    case OperandType.Immediate: isValid = isImmediate(operandValue) || checkLabel(operandValue); break;
                    case OperandType.Constant: isValid = isConstant(operandValue); break;
                    case OperandType.Flag: isValid = isFlag(operandValue); break;
                    case OperandType.FillMode: isValid = isFillMode(operandValue); break;
                    case OperandType.PixbltMode: isValid = isPixbltMode(operandValue); break;
                    case OperandType.RegisterList: isValid = true; break; // Handled separately
                    case OperandType.Label:
                        isValid = checkLabel(operandValue);
                        if (isLabelFormat(operandValue) && !isValid) {
                            const range = new vscode.Range(lineIndex, lineWithoutComment.indexOf(operandValue), lineIndex, lineWithoutComment.indexOf(operandValue) + operandValue.length);
                            diagnostics.push(new vscode.Diagnostic(range, `Undefined label: '${operandValue}'`, vscode.DiagnosticSeverity.Error));
                            continue; // Skip generic error
                        }
                        break;
                    case OperandType.Address: isValid = isAddress(operandValue); break;
                    case OperandType.Addressable: isValid = isRegister(operandValue) || isAddress(operandValue); break;
                    case OperandType.RegisterOrConstant: isValid = isRegister(operandValue) || isConstant(operandValue); break;
                    case OperandType.RegisterOrLabel: isValid = isRegister(operandValue) || checkLabel(operandValue); break;
                }
                
                if (!isValid) {
                    let operandStartIndex = lineWithoutComment.lastIndexOf(originalOperandValue);
                    if (operandStartIndex === -1) { operandStartIndex = line.firstNonWhitespaceCharacterIndex; }
                    const range = new vscode.Range(lineIndex, operandStartIndex, lineIndex, operandStartIndex + originalOperandValue.length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid type for operand ${i + 1} of ${mnemonic}.`, vscode.DiagnosticSeverity.Error));
                } else if (isAddress(operandValue)) {
                    const offsetMatch = operandValue.match(/\((.+)\)/);
                    if (offsetMatch) {
                        const offsetValue = offsetMatch[1];
                        if (!isImmediate(offsetValue) && !definedSymbols.has(offsetValue.toUpperCase())) {
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