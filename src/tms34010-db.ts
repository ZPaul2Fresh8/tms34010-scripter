// This file contains the "database" for the TMS34010 assembly language.

// Defines the different categories of operands our language supports.
export enum OperandType {
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

// Defines the structure for an instruction's syntax rule.
export interface InstructionRule {
    operands: OperandType[];
    syntax: string;
    opcode: string;
    description: string;
    hasOptionalFieldSize?: boolean;
    requireSameRegisterPage?: boolean;
    minOperands?: number;
}

export const A_REGISTERS_ORDERED = ['A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 'A12', 'A13', 'A14', 'SP'];
export const B_REGISTERS_ORDERED = ['B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'B13', 'B14', 'FP'];

export const A_REGISTERS = new Set(A_REGISTERS_ORDERED);
export const B_REGISTERS = new Set(B_REGISTERS_ORDERED);
const OTHER_REGISTERS = new Set(['ST', 'PC', 'IOSTAT', 'CTRL1', 'CTRL2', 'HSTADR', 'HSTDATA', 'HSTCTL', 'INTPEND', 'INTENB', 'DPYCTL', 'DPYSTRT', 'DPYADR', 'VCOUNT', 'HCOUNT', 'PFILL', 'PLINE', 'CONVSP', 'CONVDP', 'PSIZE', 'PMOVE', 'SADDR', 'SCOUNT', 'DADDR', 'DCOUNT', 'OFFSET', 'WINDOW', 'WSTART', 'WEND', 'DYDX', 'COLOR0', 'COLOR1']);
export const TMS34010_REGISTERS = new Set([...A_REGISTERS, ...B_REGISTERS, ...OTHER_REGISTERS]);

// A structured map of ALL instructions and their validation rules.
export const INSTRUCTION_RULES: Map<string, InstructionRule> = new Map([
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
    ['EXGF',  { operands: [OperandType.Register, OperandType.Constant], syntax: "EXGF Rd, F", opcode: "1101 01F1 F00R DDDD", description: "Exchange field size." }],
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
    ['SETF',  { operands: [OperandType.Constant, OperandType.Flag], syntax: "SETF FS, FE, F", hasOptionalFieldSize:true, opcode: "0000 0111 01(FS)(FE)(F)0 0000", description: "Set the field parameters." }],
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

// Set of all known assembler directives
export const KNOWN_DIRECTIVES = new Set([
    '.set', '.equ', '.word', '.long', '.string', '.asciiz', '.byte', '.field', '.sint', '.float',
    '.sect', '.bss', '.text', '.data', '.align', '.space',
    '.global', '.globl',
    '.end', '.org', 
    '.def', '.ref', '.newblock', '.cdef', 'endcdef', '.clink', '.cstruct', '.endstruct',
    '.struct', '.union', '.tag', '.eval', '.emsg', '.wmsg', '.fclist', '.fcnolist',
    '.drlist', '.drnolist', '.mlist', '.mnolist', '.sslist', '.ssnolist', '.var',
    '.label', 'option',
    '.if', '.else', '.endif', '.include', '.file', '.title'
]);