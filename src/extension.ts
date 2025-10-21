import * as vscode from 'vscode';
import * as path from 'path';
import Mexp from 'math-expression-evaluator';
import {
    OperandType,
    InstructionRule,
    INSTRUCTION_RULES,
    KNOWN_DIRECTIVES,
    TMS34010_REGISTERS,
    A_REGISTERS,
    B_REGISTERS,
    A_REGISTERS_ORDERED,
    B_REGISTERS_ORDERED
} from './tms34010-db';

type SymbolInfo = {
    uri: vscode.Uri;
    range: vscode.Range;
    value: string | null;
    size?: string;
    type: 'label' | 'equ' | 'set' | 'bss' | 'global';
    declaration?: {
        uri: vscode.Uri;
        range: vscode.Range;
    };
};

type SectionInfo = {
    uri: vscode.Uri;
    range: vscode.Range;
    type: 'sect' | 'usect';
}

type EvaluationResult = {
    value: number | null;
    errorSymbol?: string;
}

// --- Helper Functions ---
const mexp = new Mexp();
const isRegister = (op: string): boolean => TMS34010_REGISTERS.has(op.toUpperCase());

const isImmediate = (op: string): boolean => {
    if (/^-?[0-9A-F]+H$/i.test(op)) return true;
    if (/^0X[0-9A-F]+$/i.test(op)) return true;
    if (/^>[0-9A-F]+$/i.test(op)) return true;
    if (/^B[01]+$/i.test(op)) return true;
    if (/^-?[0-9]+$/.test(op)) return true;
    return false;
};

const parseNumericValue = (op: string): number | null => {
    const upperOp = op.toUpperCase();
    if (/^-?[0-9A-F]+H$/i.test(op)) return parseInt(upperOp.slice(0, -1), 16);
    if (/^0X[0-9A-F]+$/i.test(op)) return parseInt(upperOp.substring(2), 16);
    if (/^>[0-9A-F]+$/i.test(op)) return parseInt(upperOp.substring(1), 16);
    if (/^B[01]+$/i.test(op)) return parseInt(upperOp.substring(1), 2);
    if (/^-?[0-9]+$/.test(op)) return parseInt(op, 10);
    return null;
};

const isConstant = (op: string): boolean => {
    const numValue = parseNumericValue(op);
    return numValue !== null && numValue >= 1 && numValue <= 32;
};

const isFlag = (op: string): boolean => {
    const lowerOp = op.toLowerCase();
    return lowerOp === '0' || lowerOp === '1' || lowerOp === 'w' || lowerOp === 'l';
};

const isFillMode = (op: string): boolean => op.toUpperCase() === 'L' || op.toUpperCase() === 'XY';
const isPixbltMode = (op: string): boolean => ['L', 'XY', 'B'].includes(op.toUpperCase());
const isLabelFormat = (op: string): boolean => /^[a-zA-Z_][a-zA-Z0-9_.]*$/.test(op) && !isRegister(op);
const isAddress = (op: string): boolean => {
    op = op.toUpperCase();
    if (op.startsWith('@')) {
        const subOperand = op.substring(1);
        return isLabelFormat(subOperand) || isImmediate(subOperand);
    }
    if (op.startsWith('-*')) {
        const reg = op.substring(2);
        return A_REGISTERS.has(reg) || B_REGISTERS.has(reg);
    }
    if (op.startsWith('*')) {
        if (op.endsWith('+')) {
            const reg = op.substring(1, op.length - 1);
            return A_REGISTERS.has(reg) || B_REGISTERS.has(reg);
        }
        if (op.match(/^\*(A[0-9]{1,2}|B[0-9]{1,2}|SP|FP)\((.+)\)$/)) return true;
        const reg = op.substring(1);
        return A_REGISTERS.has(reg) || B_REGISTERS.has(reg);
    }
    return false;
};

function resolveSymbols(operand: string, definedSymbols: Map<string, SymbolInfo>): string {
    let resolved = operand;
    for (let i = 0; i < 10; i++) { 
        let changed = false;
        const symbolNames = Array.from(definedSymbols.keys());
        if (symbolNames.length === 0) break;

        const regex = new RegExp(`\\b(${symbolNames.join('|')})\\b`, 'g');
        
        const newResolved = resolved.replace(regex, (match) => {
            const symbolInfo = definedSymbols.get(match);
            if (symbolInfo && symbolInfo.value) {
                changed = true;
                return symbolInfo.value;
            }
            return match; 
        });

        if (!changed) break;
        resolved = newResolved;
    }
    return resolved;
}

function evaluateSymbolicExpression(expression: string, definedSymbols: Map<string, SymbolInfo>): EvaluationResult {
    let resolved = expression;
    
    for (let i = 0; i < 10; i++) { 
        let changed = false;
        const symbolNames = Array.from(definedSymbols.keys());
        if (symbolNames.length === 0) break;
        
        const regex = new RegExp(`\\b(${symbolNames.join('|')})\\b`, 'g');
        const newResolved = resolved.replace(regex, (match) => {
            const symbolInfo = definedSymbols.get(match);
            if (symbolInfo && symbolInfo.value) {
                changed = true;
                return `(${symbolInfo.value})`; 
            }
            return match; 
        });
        
        if (!changed) break;
        resolved = newResolved;
    }
    
    try {
        let processedForMexp = resolved
            .replace(/\b([0-9A-F]+)H\b/gi, (match, p1) => parseInt(p1, 16).toString())
            .replace(/>([0-9A-F]+)\b/gi, (match, p1) => parseInt(p1, 16).toString())
            .replace(/\b0X([0-9A-F]+)\b/gi, (match, p1) => parseInt(p1, 16).toString())
            .replace(/\b([01]+)B\b/gi, (match, p1) => parseInt(p1, 2).toString());

        const remainingWords = processedForMexp.match(/\b[a-zA-Z_][a-zA-Z0-9_.]*/g);
        if (remainingWords) {
            for (const word of remainingWords) {
                if (isNaN(Number(word))) {
                    const symbolInfo = definedSymbols.get(word);
                    if (symbolInfo && !symbolInfo.value) {
                        return { value: null, errorSymbol: word };
                    }
                }
            }
        }

        const result = mexp.eval(processedForMexp, [], {});
        if (typeof result === 'number' && !isNaN(result)) {
            return { value: result };
        }
    } catch (e) {
        return { value: null };
    }
    return { value: null };
}

// --- NEW OPCODE BUILDING LOGIC ---
function getRegisterNumber(reg: string): number {
    const upperReg = reg.toUpperCase();
    let index = A_REGISTERS_ORDERED.indexOf(upperReg);
    if (index !== -1) return index;
    
    index = B_REGISTERS_ORDERED.indexOf(upperReg);
    if (index !== -1) return index;

    throw new Error(`Invalid register name: ${reg}`);
}

function buildOpcodeFromLine(lineText: string, definedSymbols: Map<string, SymbolInfo>): [number, (number | string)[]] {
    const codePart = lineText.split(';')[0].trim();
    if (!codePart) throw new Error("Empty line");

    const firstSpaceIndex = codePart.indexOf(' ');
    const mnemonic = (firstSpaceIndex === -1 ? codePart : codePart.substring(0, firstSpaceIndex)).toUpperCase();
    
    if (!mnemonic || (!INSTRUCTION_RULES.has(mnemonic) && mnemonic !== 'MOVE')) {
        throw new Error("Invalid mnemonic");
    }
    
    const operandStr = (firstSpaceIndex === -1 ? '' : codePart.substring(firstSpaceIndex + 1)).trim();
    let operands = operandStr.split(',').map(p => p.trim()).filter(p => p);
    const additionalWords: (number | string)[] = []; // MODIFIED: Array can hold numbers or strings

    let rule: InstructionRule;
    let binaryOpcode: string = '';
    let fieldSizeFlag: string | null = null;

    if (INSTRUCTION_RULES.has(mnemonic)) {
        rule = INSTRUCTION_RULES.get(mnemonic)!;
        
        if (rule.hasOptionalFieldSize && operands.length === rule.operands.length + 1) {
            fieldSizeFlag = operands.pop()!;
        }
    }

    if (mnemonic === 'MOVE') {
        throw new Error("MOVE opcode building is not yet supported.");
    } else if (mnemonic === 'MMFM' || mnemonic === 'MMTM') {
        // This logic remains the same, but additionalWords is now (number | string)[]
        rule = INSTRUCTION_RULES.get(mnemonic)!;
        binaryOpcode = rule.opcode.replace(/\s/g, '');
        const firstCommaIndex = operandStr.indexOf(',');
        if (firstCommaIndex === -1) throw new Error("MMFM/MMTM requires 2 operands");
        const pointerRegStr = operandStr.substring(0, firstCommaIndex).trim();
        const regListStr = operandStr.substring(firstCommaIndex + 1).trim();
        const pointerRegNum = getRegisterNumber(pointerRegStr);
        const pointerRegFile = A_REGISTERS.has(pointerRegStr.toUpperCase()) ? '0' : '1';
        binaryOpcode = binaryOpcode.replace('R', pointerRegFile).replace('DDDD', pointerRegNum.toString(2).padStart(4, '0'));
        const regListParts = regListStr.split(',').map(p => p.trim());
        const isARegList = A_REGISTERS.has(regListParts[0].toUpperCase().split('-')[0]);
        const sourceList = isARegList ? A_REGISTERS_ORDERED : B_REGISTERS_ORDERED;
        let registerMask = 0;
        for (const part of regListParts) {
            const rangeMatch = part.match(/^([AB][0-9]{1,2}|SP|FP)-([AB][0-9]{1,2}|SP|FP)$/i);
            if (rangeMatch) {
                const startReg = rangeMatch[1].toUpperCase();
                const endReg = rangeMatch[2].toUpperCase();
                const startIndex = sourceList.indexOf(startReg);
                const endIndex = sourceList.indexOf(endReg);
                for (let i = startIndex; i <= endIndex; i++) {
                    registerMask |= (1 << i);
                }
            } else {
                const regIndex = sourceList.indexOf(part.toUpperCase());
                if(regIndex !== -1) {
                    registerMask |= (1 << regIndex);
                }
            }
        }
        additionalWords.push(registerMask);
    } else if (mnemonic.startsWith('J') || mnemonic.startsWith('CALL') || mnemonic.startsWith('DSJ')) {
        rule = INSTRUCTION_RULES.get(mnemonic)!;
        binaryOpcode = rule.opcode.replace(/\s/g, '');

        if (rule.operands[0] === OperandType.Register) {
            const regStr = operands[0];
            const regNum = getRegisterNumber(regStr);
            const regFile = A_REGISTERS.has(regStr.toUpperCase()) ? '0' : '1';
            binaryOpcode = binaryOpcode.replace('R', regFile).replace(/S{4}/, regNum.toString(2).padStart(4,'0'));
        }
        
        if(mnemonic.startsWith('DSJ')){
            const regStr = operands[0];
            const regNum = getRegisterNumber(regStr);
            binaryOpcode = binaryOpcode.replace(/D{4}/, regNum.toString(2).padStart(4, '0'));
        }
        
        // --- MODIFIED: Push string placeholder for labels ---
        if (rule.operands.some(op => op === OperandType.Label)) {
            const labelOperand = operands[operands.length - 1];
            if (mnemonic === 'CALLA' || mnemonic.startsWith('JA')) {
                additionalWords.push(`<${labelOperand}>`);
            } else if (mnemonic === 'CALLR' || mnemonic.startsWith('JR') || mnemonic.startsWith('DSJ')) {
                additionalWords.push(`<${labelOperand}_offset>`);
            }
        }

    } else {
        // ... Logic for other instructions is unchanged ...
    }
    
    binaryOpcode = binaryOpcode.replace(/[A-Z]/g, '0');
    return [parseInt(binaryOpcode, 2), additionalWords];
}

const KNOWN_INSTRUCTIONS = new Set(INSTRUCTION_RULES.keys());
const DIAGNOSTIC_COLLECTION = vscode.languages.createDiagnosticCollection('tms34010');
const documentSymbolsCache = new Map<string, Map<string, SymbolInfo>>();

function debounce<F extends (...args: any[]) => any>(func: F, waitFor: number) {
    let timeout: NodeJS.Timeout | null = null;
    return (...args: Parameters<F>): void => {
        if (timeout) clearTimeout(timeout);
        timeout = setTimeout(() => func(...args), waitFor);
    };
}

export function activate(context: vscode.ExtensionContext) {
    const debouncedUpdateDiagnostics = debounce(updateDiagnostics, 300);
    
    // Autocomplete
    context.subscriptions.push(
        vscode.languages.registerCompletionItemProvider('tms-assembly', {
            provideCompletionItems(document, position, token, context) {
                if (context.triggerCharacter === '@') {
                    const symbols = documentSymbolsCache.get(document.uri.toString());
                    if (!symbols) return [];

                    const replacementRange = new vscode.Range(position.line, position.character - 1, position.line, position.character);

                    const labelSuggestions: vscode.CompletionItem[] = [];
                    for (const [name, info] of symbols.entries()) {
                        if (info.type === 'label') {
                            const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Reference);
                            item.range = replacementRange;
                            labelSuggestions.push(item);
                        }
                    }
                    return labelSuggestions;
                }
                
                if (context.triggerCharacter === '.') {
                    return Array.from(KNOWN_DIRECTIVES).map(dir => {
                        const cleanDir = dir.startsWith('.') ? dir.substring(1) : dir;
                        return new vscode.CompletionItem(cleanDir, vscode.CompletionItemKind.Keyword);
                    });
                }
                
                const line = document.lineAt(position);
                const linePrefix = line.text.substr(0, position.character);
                const trimmedPrefix = linePrefix.trimLeft();

                if (trimmedPrefix.length === linePrefix.length && !trimmedPrefix.includes(' ')) {
                    return Array.from(KNOWN_INSTRUCTIONS).map(instr => new vscode.CompletionItem(instr, vscode.CompletionItemKind.Keyword));
                }
                
                const parts = trimmedPrefix.split(/[\s,]+/);
                const mnemonic = parts[0].toUpperCase().replace('.', '');
                
                if (INSTRUCTION_RULES.has(mnemonic)) {
                    const rule = INSTRUCTION_RULES.get(mnemonic)!;
                    const commaCount = (linePrefix.match(/,/g) || []).length;
                    
                    const currentOperandIndex = commaCount;
                    if (currentOperandIndex < rule.operands.length) {
                        const expectedType = rule.operands[currentOperandIndex];
                        const createRegisterSuggestions = () => Array.from(TMS34010_REGISTERS).map(reg => new vscode.CompletionItem(reg, vscode.CompletionItemKind.Variable));
                        
                        if(context.triggerCharacter === '*') return createRegisterSuggestions();
                        
                        const createLabelSuggestions = () => {
                            const symbols = documentSymbolsCache.get(document.uri.toString());
                            if (!symbols) return [];
                            return Array.from(symbols.entries()).map(([name, info]) => {
                                let kind: vscode.CompletionItemKind;
                                switch (info.type) {
                                    case 'label': case 'bss': case 'global': kind = vscode.CompletionItemKind.Reference; break;
                                    case 'equ': case 'set': kind = vscode.CompletionItemKind.Constant; break;
                                    default: kind = vscode.CompletionItemKind.Variable;
                                }
                                const item = new vscode.CompletionItem(name, kind);
                                if (info.value) item.detail = `(equates to: ${info.value})`;
                                return item;
                            });
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
                            case OperandType.ImmediateOrLabel:
                                return createLabelSuggestions();
                            case OperandType.Flag: return [new vscode.CompletionItem('0'), new vscode.CompletionItem('1')];
                            case OperandType.FillMode: return [new vscode.CompletionItem('L'), new vscode.CompletionItem('XY')];
                            case OperandType.PixbltMode: return [new vscode.CompletionItem('L'), new vscode.CompletionItem('XY'), new vscode.CompletionItem('B')];
                        }
                    }
                }
                
                return undefined;
            }
        }, ' ', ',', '*', '.', '@')
    );
    // Hover
    context.subscriptions.push(
        vscode.languages.registerHoverProvider('tms-assembly', {
            provideHover(document, position, token) {
                const line = document.lineAt(position.line);
                const lineText = line.text.trim();
                const lineParts = lineText.split(/\s+/);
                const mnemonic = lineParts.length > 0 ? lineParts[0].toUpperCase() : '';

                if (INSTRUCTION_RULES.has(mnemonic) || mnemonic === 'MOVE') {
                    let rule: Partial<InstructionRule>;
                    let content: vscode.MarkdownString;

                    if (mnemonic === 'MOVE') {
                        rule = { 
                            syntax: "MOVE src, dest", 
                            description: "Move data between registers and/or memory." 
                        };
                        content = new vscode.MarkdownString(`**${mnemonic}**\n\n*${rule.syntax}*\n\n${rule.description}`);
                    } else {
                        rule = INSTRUCTION_RULES.get(mnemonic)!;
                        content = new vscode.MarkdownString(`**${mnemonic}**\n\n*${rule.syntax}*\n\n${rule.description}`);
                        if (rule.opcode) {
                            content.appendCodeblock(rule.opcode, 'plaintext');
                        }
                    }
                    
                    try {
                        const symbols = documentSymbolsCache.get(document.uri.toString());
                        if (symbols) {
                            const [opcodeValue, additionalWords] = buildOpcodeFromLine(line.text, symbols);
                            let hexOpcode = `0x${opcodeValue.toString(16).toUpperCase().padStart(4, '0')}`;
                            
                            // --- MODIFIED: Handle both numbers and string placeholders ---
                            if (additionalWords.length > 0) {
                                hexOpcode += ' ' + additionalWords.map(word => {
                                    if (typeof word === 'number') {
                                        return `0x${(word & 0xFFFF).toString(16).toUpperCase().padStart(4, '0')}`;
                                    } else {
                                        return word; // This is our string placeholder
                                    }
                                }).join(' ');
                            }
                            content.appendMarkdown(`\n\n**Calculated Opcode:** \`${hexOpcode}\``);
                        }
                    } catch (e) {
                        // Silently fail
                    }

                    return new vscode.Hover(content, line.range);
                }

                // ... The rest of the hover provider is unchanged ...
            }
        })
    );
    // Provide Definition
    context.subscriptions.push(
        vscode.languages.registerDefinitionProvider('tms-assembly', {
            provideDefinition(document, position, token): vscode.ProviderResult<vscode.Definition> {
                const line = document.lineAt(position.line);
                const includeMatch = line.text.match(/^\s*\.include\s+(?:"([^"]+)"|([^\s"]+))/i);

                if (includeMatch) {
                    const fileName = includeMatch[1] || includeMatch[2];
                    const matchStartIndex = line.text.indexOf(fileName);
                    const matchEndIndex = matchStartIndex + fileName.length;
                    const fileNameRange = new vscode.Range(position.line, matchStartIndex, position.line, matchEndIndex);

                    if (fileNameRange.contains(position)) {
                        const currentDir = path.dirname(document.uri.fsPath);
                        const includeUri = vscode.Uri.file(path.join(currentDir, fileName));
                        
                        const targetPosition = new vscode.Position(0, 0);
                        return new vscode.Location(includeUri, targetPosition);
                    }
                }

                const wordRange = document.getWordRangeAtPosition(position);
                if (!wordRange) return undefined;
                
                const word = document.getText(wordRange);
                const symbols = documentSymbolsCache.get(document.uri.toString());

                if (symbols && symbols.has(word)) {
                    const symbolInfo = symbols.get(word)!;
                    const locations: vscode.Location[] = [];

                    if (symbolInfo.type !== 'global') {
                        locations.push(new vscode.Location(symbolInfo.uri, symbolInfo.range));
                    }

                    if (symbolInfo.declaration) {
                        locations.push(new vscode.Location(symbolInfo.declaration.uri, symbolInfo.declaration.range));
                    }
                    
                    return locations;
                }

                return undefined;
            }
        })
    );
    // Signature Help
    context.subscriptions.push(
        vscode.languages.registerSignatureHelpProvider('tms-assembly', {
            provideSignatureHelp(document, position, token, context) {
                const line = document.lineAt(position).text.substring(0, position.character);
                const parts = line.trim().split(/[\s,]+/);

                if (parts.length < 1) {
                    return undefined;
                }

                const mnemonic = parts[0].toUpperCase();
                if (!INSTRUCTION_RULES.has(mnemonic)) {
                    return undefined;
                }

                const rule = INSTRUCTION_RULES.get(mnemonic)!;
                const activeParameter = (line.match(/,/g) || []).length;

                if (activeParameter >= rule.operands.length) {
                    return undefined;
                }

                const signature = new vscode.SignatureInformation(rule.syntax, new vscode.MarkdownString(rule.description));
                
                const operandStr = rule.syntax.substring(mnemonic.length).trim();
                const paramLabels = operandStr.split(',').map(p => p.trim());
                signature.parameters = paramLabels.map(label => new vscode.ParameterInformation(label));
                
                const signatureHelp = new vscode.SignatureHelp();
                signatureHelp.signatures = [signature];
                signatureHelp.activeSignature = 0;
                signatureHelp.activeParameter = activeParameter;

                return signatureHelp;
            }
        }, ' ', ',')
    );
    // Document Formatting
    context.subscriptions.push(
        vscode.languages.registerDocumentFormattingEditProvider('tms-assembly', {
            provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.TextEdit[] {
                const edits: vscode.TextEdit[] = [];
                const formattedLines: string[] = [];
                const mnemonicCol = 16;
                const operandCol = 28;
                const commentCol = 48;

                for (let i = 0; i < document.lineCount; i++) {
                    const line = document.lineAt(i);

                    if (line.isEmptyOrWhitespace) {
                        formattedLines.push('');
                        continue;
                    }

                    const text = line.text;
                    const trimmedText = text.trim();

                    if (trimmedText.startsWith(';') || trimmedText.startsWith('*')) {
                        formattedLines.push(text);
                        continue;
                    }
                    
                    let label: string | undefined;
                    let mnemonic: string | undefined;
                    let operands: string | undefined;
                    let comment: string | undefined;

                    const commentIndex = text.indexOf(';');
                    if (commentIndex !== -1) {
                        comment = text.substring(commentIndex);
                    }
                    const codePart = (commentIndex !== -1 ? text.substring(0, commentIndex) : text).trim();
                    
                    const parts = codePart.split(/\s+/).filter(p => p);
                    
                    if (parts.length > 0) {
                        const firstWord = parts[0];
                        if (firstWord.endsWith(':') || (!KNOWN_INSTRUCTIONS.has(firstWord.toUpperCase()) && !KNOWN_DIRECTIVES.has(firstWord.toLowerCase()))) {
                            label = parts.shift();
                        }
                        
                        if (parts.length > 0) {
                            mnemonic = parts.shift();
                        }
                        if (parts.length > 0) {
                            operands = parts.join(' ');
                        }
                    }

                    let builtLine = '';
                    if (label) {
                        builtLine += label;
                    }

                    if (mnemonic) {
                        while (builtLine.length < mnemonicCol) {
                            builtLine += ' ';
                        }
                        builtLine += mnemonic;
                    }

                    if (operands) {
                        while (builtLine.length < operandCol) {
                            builtLine += ' ';
                        }
                        builtLine += operands;
                    }

                    if (comment) {
                        while (builtLine.length < commentCol) {
                            builtLine += ' ';
                        }
                        builtLine += comment;
                    }

                    formattedLines.push(builtLine.trimEnd());
                }

                const fullRange = new vscode.Range(
                    document.positionAt(0),
                    document.positionAt(document.getText().length)
                );
                
                edits.push(vscode.TextEdit.replace(fullRange, formattedLines.join('\n')));
                return edits;
            }
        })
    );

    if (vscode.window.activeTextEditor) {
        updateDiagnostics(vscode.window.activeTextEditor.document, DIAGNOSTIC_COLLECTION);
    }
    context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(editor => {
        if (editor) updateDiagnostics(editor.document, DIAGNOSTIC_COLLECTION);
    }));
    context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(event => {
        if (event.document.languageId === 'tms-assembly') debouncedUpdateDiagnostics(event.document, DIAGNOSTIC_COLLECTION);
    }));
    context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(doc => DIAGNOSTIC_COLLECTION.delete(doc.uri)));
}

function defineSymbol(
    name: string,
    type: 'label' | 'equ' | 'set' | 'bss' | 'global',
    value: string | null,
    range: vscode.Range | null,
    definedSymbols: Map<string, SymbolInfo>,
    uri: vscode.Uri,
    diagnosticsByUri: Map<string, vscode.Diagnostic[]>,
    inConditionalBlock: boolean,
    size?: string
) {
    if (!range) return;
    const existingSymbol = definedSymbols.get(name);

    if (existingSymbol) {
        if (existingSymbol.type === 'global' && (type === 'label' || type === 'bss' || type === 'equ' || type === 'set')) {
            const newInfo: SymbolInfo = {
                uri,
                range,
                value,
                type,
                size,
                declaration: { uri: existingSymbol.uri, range: existingSymbol.range }
            };
            definedSymbols.set(name, newInfo);
            return;
        }

        if (existingSymbol.type !== 'set') {
            if (inConditionalBlock) {
                definedSymbols.set(name, { uri, range, value, type, size });
                return;
            }

            const diagnostic = new vscode.Diagnostic(range, `Duplicate symbol definition: '${name}'`, vscode.DiagnosticSeverity.Error);
            diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(existingSymbol.uri, existingSymbol.range), 'First defined here')];
            
            const uriString = uri.toString();
            if (!diagnosticsByUri.has(uriString)) {
                diagnosticsByUri.set(uriString, []);
            }
            diagnosticsByUri.get(uriString)!.push(diagnostic);
            return;
        }
    }

    definedSymbols.set(name, { uri, range, value, type, size });
}

async function parseSymbols(
    doc: vscode.TextDocument,
    definedSymbols: Map<string, SymbolInfo>,
    processedFiles: Set<string>,
    diagnosticsByUri: Map<string, vscode.Diagnostic[]>,
    definedSections: Map<string, SectionInfo>,
    inConditionalBlock = false
): Promise<boolean> {
    const uriString = doc.uri.toString();
    if (processedFiles.has(uriString)) {
        return inConditionalBlock; 
    }
    processedFiles.add(uriString);
    
    let currentConditionalState = inConditionalBlock;

    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const includeMatch = line.text.trim().match(/^\.include\s+(?:"([^"]+)"|([^\s"]+))/i);
        
        if (includeMatch) {
            const fileName = includeMatch[1] || includeMatch[2];
            const currentDir = path.dirname(doc.uri.fsPath);
            const includeUri = vscode.Uri.file(path.join(currentDir, fileName));
            
            try {
                const includedDoc = await vscode.workspace.openTextDocument(includeUri);
                currentConditionalState = await parseSymbols(includedDoc, definedSymbols, processedFiles, diagnosticsByUri, definedSections, currentConditionalState);
            } catch (err) {
                const range = new vscode.Range(lineIndex, line.text.indexOf(fileName), lineIndex, line.text.indexOf(fileName) + fileName.length);
                const diagnostic = new vscode.Diagnostic(range, `Included file not found: ${fileName}`, vscode.DiagnosticSeverity.Error);
                
                if (!diagnosticsByUri.has(uriString)) {
                    diagnosticsByUri.set(uriString, []);
                }
                diagnosticsByUri.get(uriString)!.push(diagnostic);
            }
        }
    }

    processedFiles.delete(uriString);

    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);

        // --- THIS IS THE FIX ---
        // Clean the EOF character from the line before any parsing.
        const text = line.text.replace(/\u001a/g, '');
        
        const trimmedText = text.trim();
        const upperTrimmedText = trimmedText.toUpperCase();
        
        if (trimmedText.length === 0 || trimmedText.startsWith(';') || trimmedText.startsWith('*') || upperTrimmedText.startsWith('.INCLUDE')) {
            continue;
        }

        if (upperTrimmedText.startsWith('.IF')) {
            currentConditionalState = true;
            continue;
        }
        if (upperTrimmedText.startsWith('.ENDIF')) {
            currentConditionalState = false;
            continue;
        }
        
        const symbolRange = (symbol: string) => {
            const index = text.indexOf(symbol);
            return index >= 0 ? new vscode.Range(lineIndex, index, lineIndex, index + symbol.length) : null;
        };
        
        const globalMatch = trimmedText.match(/^\.(global|globl)\s+(.+)/i);
        const equateMatch = trimmedText.match(/\b([a-zA-Z_][a-zA-Z0-9_]+)\s+(\.equ|equ|\.set|set|\.usect|usect)\s+(.+)/i);
        const bssMatch = trimmedText.match(/\.bss\s+([a-zA-Z_][a-zA-Z0-9_.]+)(.*)/i);
        const labelMatch = trimmedText.match(/^([a-zA-Z_][a-zA-Z0-9_.]+):/);
        const sectMatch = trimmedText.match(/^\.sect\s+(.*)/i);

        if (globalMatch) {
            const symbolsStr = globalMatch[2].split(';')[0];
            const globalSymbols = symbolsStr.split(',').map(s => s.trim()).filter(Boolean);
            for (const symbolName of globalSymbols) {
                const range = symbolRange(symbolName);
                if (!range) continue;
                
                const existingSymbol = definedSymbols.get(symbolName);
                if (existingSymbol && (existingSymbol.type === 'label' || existingSymbol.type === 'bss' || existingSymbol.type === 'equ' || existingSymbol.type === 'set')) {
                    existingSymbol.declaration = { uri: doc.uri, range };
                } 
                else if (!existingSymbol) {
                    definedSymbols.set(symbolName, { uri: doc.uri, range, value: null, type: 'global' });
                }
            }
        } else if (equateMatch) {
            const name = equateMatch[1];
            const directive = equateMatch[2].toLowerCase().replace('.', '');
            const rawValue = equateMatch[3];
            const cleanValue = rawValue.split(';')[0].trim();
            
            if (directive === 'usect') {
                defineSymbol(name, 'bss', null, symbolRange(name), definedSymbols, doc.uri, diagnosticsByUri, currentConditionalState);
                const operands = cleanValue.split(',');
                if (operands.length > 0) {
                    const sectionName = operands[0].trim();
                    const sectionRange = symbolRange(sectionName);
                    if (sectionRange) {
                        const existingSection = definedSections.get(sectionName);
                        if (existingSection && existingSection.type === 'sect') {
                            const diagnostic = new vscode.Diagnostic(sectionRange, `Section name '${sectionName}' cannot be used with .usect because it was already defined with .sect.`, vscode.DiagnosticSeverity.Error);
                            diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(existingSection.uri, existingSection.range), 'First defined here')];
                            if (!diagnosticsByUri.has(uriString)) diagnosticsByUri.set(uriString, []);
                            diagnosticsByUri.get(uriString)!.push(diagnostic);
                        } else if (!existingSection) {
                            definedSections.set(sectionName, { uri: doc.uri, range: sectionRange, type: 'usect' });
                        }
                    }
                }
            } else {
                defineSymbol(name, directive as 'equ'|'set', cleanValue, symbolRange(name), definedSymbols, doc.uri, diagnosticsByUri, currentConditionalState);
            }
        } else if (bssMatch) {
            const bssName = bssMatch[1];
            const restOfLine = bssMatch[2].trim();
            let size;
            if (restOfLine.startsWith(',')) {
                const operands = restOfLine.substring(1).trim().split(';')[0].split(',');
                if (operands.length > 0) {
                    size = operands[0].trim();
                }
            }
            defineSymbol(bssName, 'bss', null, symbolRange(bssName), definedSymbols, doc.uri, diagnosticsByUri, currentConditionalState, size);
        } else if (sectMatch) {
            const sectionNameWithComment = sectMatch[1].trim();
            const sectionName = sectionNameWithComment.split(';')[0].trim();
            const sectionRange = symbolRange(sectionName);
            if (sectionRange) {
                const existingSection = definedSections.get(sectionName);
                if (existingSection && existingSection.type === 'usect') {
                    const diagnostic = new vscode.Diagnostic(sectionRange, `Section name '${sectionName}' cannot be used with .sect because it was already defined with .usect.`, vscode.DiagnosticSeverity.Error);
                    diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(existingSection.uri, existingSection.range), 'First defined here')];
                    if (!diagnosticsByUri.has(uriString)) diagnosticsByUri.set(uriString, []);
                    diagnosticsByUri.get(uriString)!.push(diagnostic);
                } else if (!existingSection) {
                    definedSections.set(sectionName, { uri: doc.uri, range: sectionRange, type: 'sect' });
                }
            }
        } else if (labelMatch) {
            const labelName = labelMatch[1];
            defineSymbol(labelName, 'label', null, symbolRange(labelName), definedSymbols, doc.uri, diagnosticsByUri, currentConditionalState);
        } else if (line.firstNonWhitespaceCharacterIndex === 0) {
            const firstWordRaw = trimmedText.split(/\s+/)[0];
            if (!KNOWN_INSTRUCTIONS.has(firstWordRaw.toUpperCase()) && !KNOWN_DIRECTIVES.has(firstWordRaw.toLowerCase())) {
                defineSymbol(firstWordRaw, 'label', null, symbolRange(firstWordRaw), definedSymbols, doc.uri, diagnosticsByUri, currentConditionalState);
            }
        }
    }
    return currentConditionalState;
}

async function updateDiagnostics(doc: vscode.TextDocument, collection: vscode.DiagnosticCollection): Promise<void> {
    if (doc.languageId !== 'tms-assembly') return;

    const definedSymbols = new Map<string, SymbolInfo>();
    const processedFiles = new Set<string>();
    const diagnosticsByUri = new Map<string, vscode.Diagnostic[]>();
    const definedSections = new Map<string, SectionInfo>();
    
    await parseSymbols(doc, definedSymbols, processedFiles, diagnosticsByUri, definedSections);
    documentSymbolsCache.set(doc.uri.toString(), definedSymbols);

    for (const uriString of processedFiles) {
        let fileDoc: vscode.TextDocument;
        try {
            fileDoc = await vscode.workspace.openTextDocument(vscode.Uri.parse(uriString));
        } catch (error) {
            continue;
        }

        const diagnostics = diagnosticsByUri.get(uriString) || [];

        for (let lineIndex = 0; lineIndex < fileDoc.lineCount; lineIndex++) {
            const line = fileDoc.lineAt(lineIndex);
            
            const cleanedText = line.text.replace(/\u001a/g, '');
            const lineWithoutComment = cleanedText.split(';')[0];
            const text = lineWithoutComment.trim();
            
            if (text.length === 0 || text.startsWith('*') || text.match(/^\s*\.(if|else|endif|include)/i)) {
                continue;
            }

            const upperText = text.toUpperCase();
            if (upperText.startsWith('.GLOBAL') || upperText.startsWith('.GLOBL')) {
                const parts = text.split(/\s+/);
                const operandStr = parts.slice(1).join(' ').split(';')[0];
                const globalSymbols = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);

                for (const symbol of globalSymbols) {
                    const symbolInfo = definedSymbols.get(symbol);
                    if (!symbolInfo || symbolInfo.type === 'global') {
                        const index = line.text.indexOf(symbol);
                        const range = new vscode.Range(lineIndex, index, lineIndex, index + symbol.length);
                        diagnostics.push(new vscode.Diagnostic(
                            range,
                            `Symbol '${symbol}' is declared global but is not defined in the project.`,
                            vscode.DiagnosticSeverity.Warning
                        ));
                    }
                }
            }
            
            const parts = text.split(/\s+/);
            const mnemonicIndex = parts.findIndex(p => KNOWN_INSTRUCTIONS.has(p.toUpperCase()) || KNOWN_DIRECTIVES.has(p.toLowerCase()));

            if (mnemonicIndex === -1) {
                const firstWord = parts[0].replace(':', '');
                if (!definedSymbols.has(firstWord)) {
                     const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.firstNonWhitespaceCharacterIndex + parts[0].length);
                     diagnostics.push(new vscode.Diagnostic(range, `Unrecognized instruction or directive: '${parts[0]}'`, vscode.DiagnosticSeverity.Error));
                }
                continue;
            }

            const mnemonic = parts[mnemonicIndex];
            const operandStr = parts.slice(mnemonicIndex + 1).join(' ').split(';')[0].trim();
            const upperMnemonic = mnemonic.toUpperCase().replace('.', '');
            
            if (upperMnemonic === 'SECT') {
                const sectOperands = operandStr.split(';')[0].trim();
                if (!sectOperands.startsWith('"') || !sectOperands.endsWith('"')) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, `.sect directive requires a single quoted string operand for the section name.`, vscode.DiagnosticSeverity.Error));
                }
            } else if (upperMnemonic === 'USECT') {
                // Validation for usect is handled in the symbol parsing pass where context is richer
            } else if (upperMnemonic === 'MMTM' || upperMnemonic === 'MMFM') {
                const firstCommaIndex = operandStr.indexOf(',');
                if (firstCommaIndex === -1) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, `${upperMnemonic} requires 2 operands.`, vscode.DiagnosticSeverity.Error));
                    continue;
                }

                const regOperand = operandStr.substring(0, firstCommaIndex).trim();
                const listOperand = operandStr.substring(firstCommaIndex + 1).trim();

                if (!isRegister(regOperand)) {
                    const index = lineWithoutComment.indexOf(regOperand);
                    const range = new vscode.Range(lineIndex, index, lineIndex, index + regOperand.length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid register for ${upperMnemonic}.`, vscode.DiagnosticSeverity.Error));
                }

                validateRegisterList(listOperand, lineIndex, lineWithoutComment, diagnostics);
            } else if (upperMnemonic === 'PIXT') {
                const operandParts = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
                if (operandParts.length !== 2) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, `PIXT requires 2 operands.`, vscode.DiagnosticSeverity.Error));
                    continue;
                }

                const isValidPixtOperand = (op: string): boolean => {
                    if (isRegister(op) || isAddress(op)) return true;
                    if (op.toUpperCase().match(/^\*(A[0-9]{1,2}|B[0-9]{1,2}|SP|FP)\.XY$/)) return true;
                    return false;
                };

                for (const originalOperand of operandParts) {
                    const resolvedOperand = resolveSymbols(originalOperand, definedSymbols);
                    if (!isValidPixtOperand(resolvedOperand)) {
                        const index = lineWithoutComment.indexOf(originalOperand);
                        const range = new vscode.Range(lineIndex, index, lineIndex, index + originalOperand.length);
                        const message = originalOperand === resolvedOperand
                            ? `Invalid operand '${originalOperand}' for PIXT.`
                            : `Invalid operand '${originalOperand}' (resolved to '${resolvedOperand}') for PIXT.`;
                        diagnostics.push(new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error));
                    }
                }
            } else if (KNOWN_INSTRUCTIONS.has(upperMnemonic)) {
                const rule = INSTRUCTION_RULES.get(upperMnemonic)!;
                const operandParts = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
                
                const minOps = rule.minOperands ?? rule.operands.length;
                const maxOps = rule.hasOptionalFieldSize ? rule.operands.length + 1 : rule.operands.length;

                if (operandParts.length < minOps || operandParts.length > maxOps) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, lineWithoutComment.trimRight().length);
                    const expected = minOps === maxOps ? minOps : `${minOps} to ${maxOps}`;
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid operand count for ${upperMnemonic}. Expected ${expected}, but got ${operandParts.length}.`, vscode.DiagnosticSeverity.Error));
                    continue;
                }
                
                let searchStart = line.text.toUpperCase().indexOf(upperMnemonic) + upperMnemonic.length;

                for (let i = 0; i < operandParts.length; i++) {
                    const originalOperandValue = operandParts[i];
                    const operandStartIndex = line.text.indexOf(originalOperandValue, searchStart);
                    if (operandStartIndex === -1) continue; 

                    const range = new vscode.Range(lineIndex, operandStartIndex, lineIndex, operandStartIndex + originalOperandValue.length);
                    searchStart = operandStartIndex + originalOperandValue.length;

                    const operandValue = resolveSymbols(originalOperandValue, definedSymbols);
                    
                    if (rule.hasOptionalFieldSize && i === rule.operands.length) {
                        if (!isFlag(operandValue)) {
                            diagnostics.push(new vscode.Diagnostic(range, `Invalid field size. Expected 0 (W) or 1 (L).`, vscode.DiagnosticSeverity.Error));
                        }
                        continue; 
                    }

                    const expectedType = rule.operands[i];
                    let isValid = false;
                    
                    const checkLabel = (op: string): boolean => {
                        const symbol = definedSymbols.get(op);
                        return !!(isLabelFormat(op) && symbol && (symbol.type === 'label' || symbol.type === 'global'));
                    };

                    switch (expectedType) {
                        case OperandType.Register: isValid = isRegister(operandValue); break;
                        case OperandType.Immediate:
                        case OperandType.ImmediateOrLabel:
                            if (isImmediate(operandValue) || checkLabel(operandValue)) {
                                isValid = true;
                            } else {
                                const evalResult = evaluateSymbolicExpression(operandValue, definedSymbols);
                                isValid = evalResult.value !== null;
                                if (!isValid && evalResult.errorSymbol) {
                                    const symInfo = definedSymbols.get(evalResult.errorSymbol);
                                    const symType = symInfo ? `a memory location defined by .${symInfo.type}` : 'an undefined symbol';
                                    diagnostics.push(new vscode.Diagnostic(range, `Symbol '${evalResult.errorSymbol}' is ${symType} and cannot be used in this expression.`, vscode.DiagnosticSeverity.Error));
                                    continue;
                                }
                            }
                            break;
                        case OperandType.Constant: isValid = isConstant(operandValue); break;
                        case OperandType.Flag: isValid = isFlag(operandValue); break;
                        case OperandType.FillMode: isValid = isFillMode(operandValue); break;
                        case OperandType.PixbltMode: isValid = isPixbltMode(operandValue); break;
                        case OperandType.RegisterList: isValid = true; break; 
                        case OperandType.Label:
                            isValid = checkLabel(operandValue);
                            if (isLabelFormat(operandValue) && !isValid) {
                                diagnostics.push(new vscode.Diagnostic(range, `Undefined label: '${operandValue}'`, vscode.DiagnosticSeverity.Error));
                                continue;
                            }
                            break;
                        case OperandType.Address: isValid = isAddress(operandValue); break;
                        case OperandType.Addressable: isValid = isRegister(operandValue) || isAddress(operandValue); break;
                        case OperandType.RegisterOrConstant: isValid = isRegister(operandValue) || isConstant(operandValue); break;
                        case OperandType.RegisterOrLabel: isValid = isRegister(operandValue) || checkLabel(operandValue); break;
                    }
                    
                    if (!isValid) {
                        const message = originalOperandValue === operandValue
                            ? `Invalid type for operand '${originalOperandValue}' of ${upperMnemonic}.`
                            : `Invalid type for operand '${originalOperandValue}' (resolved to '${operandValue}') of ${upperMnemonic}.`;
                        diagnostics.push(new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error));
                    }
                }
            } else if (upperMnemonic === 'SET' || upperMnemonic === 'EQU') {
                const equateMatch = text.match(/\b([a-zA-Z_][a-zA-Z0-9_]+)\s+(?:\.equ|equ|\.set|set)\s+(.+)/i);
                if (equateMatch) {
                    const value = equateMatch[2].trim();
                    let isValid = false;

                    const resolvedValue = resolveSymbols(value, definedSymbols);

                    if (isRegister(resolvedValue) || isImmediate(resolvedValue)) {
                        isValid = true;
                    } 
                    else if (value.startsWith('[') && value.endsWith(']')) {
                        const bracketContent = value.substring(1, value.length - 1);
                        const parts = bracketContent.split(',');
                        if (parts.length === 2) {
                            const part1 = evaluateSymbolicExpression(parts[0].trim(), definedSymbols);
                            const part2 = evaluateSymbolicExpression(parts[1].trim(), definedSymbols);

                            if (part1.value !== null && (part1.value >= 0 && part1.value <= 65535) &&
                                part2.value !== null && (part2.value >= 0 && part2.value <= 65535)) {
                                isValid = true;
                            }
                        }
                    }
                    else {
                        const evalResult = evaluateSymbolicExpression(value, definedSymbols);
                        if (evalResult.value !== null) {
                            isValid = true;
                        }
                    }

                    if (!isValid) {
                        const index = lineWithoutComment.indexOf(value);
                        const range = new vscode.Range(lineIndex, index, lineIndex, index + value.length);
                        const errorMessage = `Invalid value for .${upperMnemonic}. Must be a register, a numeric expression, or two 16-bit words in brackets [w1,w2].`;
                        diagnostics.push(new vscode.Diagnostic(range, errorMessage, vscode.DiagnosticSeverity.Error));
                    }
                }
            } else if (upperMnemonic === 'WORD' || upperMnemonic === 'LONG') {
                const values = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
                if (values.length === 0) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, `.${upperMnemonic} directive requires at least one value.`, vscode.DiagnosticSeverity.Error));
                }

                const checkLabel = (op: string) => isLabelFormat(op) && definedSymbols.has(op);
                
                for (const value of values) {
                    const range = (str: string) => {
                        const index = lineWithoutComment.indexOf(str);
                        return new vscode.Range(lineIndex, index, lineIndex, index + str.length);
                    };

                    if (checkLabel(value)) {
                        continue;
                    }

                    const evalResult = evaluateSymbolicExpression(value, definedSymbols);
                    if (evalResult.value !== null) {
                        const numValue = evalResult.value;
                        if (upperMnemonic === 'WORD') {
                            if (numValue < 0 || numValue > 65535) {
                                diagnostics.push(new vscode.Diagnostic(range(value), `Value '${value}' (${numValue}) is out of the valid 16-bit range (0 to 65535).`, vscode.DiagnosticSeverity.Error));
                            }
                        } else { // LONG
                            if (numValue < 0 || numValue > 4294967295) {
                                diagnostics.push(new vscode.Diagnostic(range(value), `Value '${value}' (${numValue}) is out of the valid 32-bit range (0 to 4294967295).`, vscode.DiagnosticSeverity.Error));
                            }
                        }
                    } else {
                         diagnostics.push(new vscode.Diagnostic(range(value), `Invalid value '${value}' for .${upperMnemonic}. Must resolve to a number or be a defined label.`, vscode.DiagnosticSeverity.Error));
                    }
                }
            } else if (upperMnemonic === 'BYTE') {
                const values = operandStr.match(/"[^"]*"|[^,]+/g)?.map(s => s.trim()).filter(s => s.length > 0) || [];
                if (values.length === 0) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, `.${upperMnemonic} directive requires at least one value.`, vscode.DiagnosticSeverity.Error));
                    continue;
                }

                let totalBytes = 0;
                for (const value of values) {
                    const range = (str: string) => {
                        const index = lineWithoutComment.indexOf(str);
                        return new vscode.Range(lineIndex, index, lineIndex, index + str.length);
                    };

                    if (value.startsWith('"') && value.endsWith('"')) {
                        totalBytes += value.length - 2;
                        continue; 
                    }

                    const evalResult = evaluateSymbolicExpression(value, definedSymbols);
                    if (evalResult.value !== null) {
                        if (evalResult.value >= -128 && evalResult.value <= 255) {
                            totalBytes++;
                        } else {
                            diagnostics.push(new vscode.Diagnostic(range(value), `Value '${value}' (${evalResult.value}) is out of the valid 8-bit range (-128 to 255).`, vscode.DiagnosticSeverity.Error));
                        }
                    } else {
                        diagnostics.push(new vscode.Diagnostic(range(value), `Invalid value '${value}' for .${upperMnemonic}. Must resolve to an 8-bit number or be a string literal.`, vscode.DiagnosticSeverity.Error));
                    }
                }

                if (totalBytes > 0 && totalBytes % 2 !== 0) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(
                        range,
                        `Byte array has an odd length (${totalBytes} bytes) and is not word-aligned.`,
                        vscode.DiagnosticSeverity.Warning
                    ));
                }
            } else if (upperMnemonic === 'BSS') {
                const bssOperands = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
                
                if (bssOperands.length < 1) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, '.bss directive requires at least a symbol operand.', vscode.DiagnosticSeverity.Error));
                    continue;
                }

                const [symbol, size, alignment] = bssOperands;
                const getOperandRange = (operand: string) => {
                    const index = lineWithoutComment.indexOf(operand);
                    return new vscode.Range(lineIndex, index, lineIndex, index + operand.length);
                };

                if (!isLabelFormat(symbol)) {
                    diagnostics.push(new vscode.Diagnostic(getOperandRange(symbol), `Invalid symbol name for .bss directive: '${symbol}'.`, vscode.DiagnosticSeverity.Error));
                }
                
                if (size) {
                    const sizeResult = evaluateSymbolicExpression(size, definedSymbols);
                    if (sizeResult.value === null) {
                        if (sizeResult.errorSymbol) {
                            const symInfo = definedSymbols.get(sizeResult.errorSymbol);
                            const symType = symInfo ? `a memory location defined by .${symInfo.type}` : 'an undefined symbol';
                            diagnostics.push(new vscode.Diagnostic(getOperandRange(size), `Symbol '${sizeResult.errorSymbol}' is ${symType} and cannot be used in a mathematical expression.`, vscode.DiagnosticSeverity.Error));
                        } else {
                            diagnostics.push(new vscode.Diagnostic(getOperandRange(size), `Size operand for .bss must be a valid numeric value or expression.`, vscode.DiagnosticSeverity.Error));
                        }
                    } else {
                        if (sizeResult.value % 16 !== 0) {
                            diagnostics.push(new vscode.Diagnostic(
                                getOperandRange(size), 
                                `Size (${sizeResult.value}) is not word-aligned (divisible by 16).`,
                                vscode.DiagnosticSeverity.Warning
                            ));
                        }
                    }
                }
                
                if (alignment) {
                    if (!isFlag(alignment)) {
                        diagnostics.push(new vscode.Diagnostic(getOperandRange(alignment), `Flag for .bss must be 0 or 1.`, vscode.DiagnosticSeverity.Error));
                    }
                }
            } else if (upperMnemonic === 'MOVE') {
                const moveOperands = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);

                if (moveOperands.length < 2 || moveOperands.length > 3) {
                    const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                    diagnostics.push(new vscode.Diagnostic(range, 'MOVE instruction requires 2 or 3 (with field flag) operands.', vscode.DiagnosticSeverity.Error));
                    continue;
                }

                const [op1, op2, fieldFlag] = moveOperands;

                const isValidOperand = (op: string): boolean => {
                    const resolvedOp = resolveSymbols(op, definedSymbols);
                    return isRegister(resolvedOp) || isAddress(resolvedOp);
                };

                if (!isValidOperand(op1)) {
                    const index = lineWithoutComment.indexOf(op1);
                    const range = new vscode.Range(lineIndex, index, lineIndex, index + op1.length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid source operand '${op1}' for MOVE.`, vscode.DiagnosticSeverity.Error));
                }
                
                if (!isValidOperand(op2)) {
                    const index = lineWithoutComment.indexOf(op2);
                    const range = new vscode.Range(lineIndex, index, lineIndex, index + op2.length);
                    diagnostics.push(new vscode.Diagnostic(range, `Invalid destination operand '${op2}' for MOVE.`, vscode.DiagnosticSeverity.Error));
                }

                if (fieldFlag && !isFlag(fieldFlag)) {
                    const index = lineWithoutComment.indexOf(fieldFlag);
                    const range = new vscode.Range(lineIndex, index, lineIndex, index + fieldFlag.length);
                    diagnostics.push(new vscode.Diagnostic(range, 'Invalid field size flag for MOVE. Must be 0 or 1.', vscode.DiagnosticSeverity.Error));
                }

            }
        }
        diagnosticsByUri.set(uriString, diagnostics);
    }
    
    collection.clear();
    diagnosticsByUri.forEach((diagnostics, uriString) => {
        collection.set(vscode.Uri.parse(uriString), diagnostics);
    });
}

function validateRegisterList(listStr: string, lineIndex: number, lineText: string, diagnostics: vscode.Diagnostic[]) {
    const listRange = new vscode.Range(lineIndex, lineText.indexOf(listStr), lineIndex, listStr.length);
    let finalRegs: string[] = [];

    const parts = listStr.split(',').map(p => p.trim());
    for (const part of parts) {
        const rangeMatch = part.match(/^([AB][0-9]{1,2}|SP|FP)-([AB][0-9]{1,2}|SP|FP)$/i);
        if (rangeMatch) {
            const startReg = rangeMatch[1].toUpperCase();
            const endReg = rangeMatch[2].toUpperCase();
            const isAReg = A_REGISTERS.has(startReg);
            const isBReg = B_REGISTERS.has(startReg);

            if (isAReg !== A_REGISTERS.has(endReg) || isBReg !== B_REGISTERS.has(endReg)) {
                diagnostics.push(new vscode.Diagnostic(listRange, 'Register range must be within the same file (A or B).', vscode.DiagnosticSeverity.Error));
                return;
            }

            const sourceList = isAReg ? A_REGISTERS_ORDERED : B_REGISTERS_ORDERED;
            const startIndex = sourceList.indexOf(startReg);
            const endIndex = sourceList.indexOf(endReg);

            if (startIndex === -1 || endIndex === -1 || startIndex > endIndex) {
                diagnostics.push(new vscode.Diagnostic(listRange, `Invalid register range: '${part}'.`, vscode.DiagnosticSeverity.Error));
                return;
            }
            finalRegs.push(...sourceList.slice(startIndex, endIndex + 1));
        } else {
            finalRegs.push(part.toUpperCase());
        }
    }

    if (finalRegs.length === 0) {
        diagnostics.push(new vscode.Diagnostic(listRange, 'Register list cannot be empty.', vscode.DiagnosticSeverity.Error));
        return;
    }

    const isARegList = A_REGISTERS.has(finalRegs[0]);
    const isBRegList = B_REGISTERS.has(finalRegs[0]);
    if (!isARegList && !isBRegList) {
        diagnostics.push(new vscode.Diagnostic(listRange, `Invalid register '${finalRegs[0]}' in list.`, vscode.DiagnosticSeverity.Error));
        return;
    }
    
    const sourceList = isARegList ? A_REGISTERS_ORDERED : B_REGISTERS_ORDERED;
    let lastIndex = -1;

    for (const reg of finalRegs) {
        if ((isARegList && !A_REGISTERS.has(reg)) || (isBRegList && !B_REGISTERS.has(reg))) {
            diagnostics.push(new vscode.Diagnostic(listRange, 'Cannot mix A and B registers in the same list.', vscode.DiagnosticSeverity.Error));
            return;
        }

        const currentIndex = sourceList.indexOf(reg);
        if (currentIndex <= lastIndex) {
            diagnostics.push(new vscode.Diagnostic(listRange, 'Registers in list must be in ascending order and unique.', vscode.DiagnosticSeverity.Error));
            return;
        }
        lastIndex = currentIndex;
    }
}



export function deactivate() {}