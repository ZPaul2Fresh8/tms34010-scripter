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
    B_REGISTERS
} from './tms34010-db';

type SymbolInfo = {
    uri: vscode.Uri; 
    range: vscode.Range;
    value: string | null;
    type: 'label' | 'equ' | 'set' | 'bss';
};

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

const isFlag = (op: string): boolean => op === '0' || op === '1';
const isFillMode = (op: string): boolean => op.toUpperCase() === 'L' || op.toUpperCase() === 'XY';
const isPixbltMode = (op: string): boolean => ['L', 'XY', 'B'].includes(op.toUpperCase());
const isLabelFormat = (op: string): boolean => /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(op) && !isRegister(op);
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

        // If the entire operand is a symbol, replace it without parentheses
        if (symbolNames.includes(resolved)) {
            const symbolInfo = definedSymbols.get(resolved);
            if (symbolInfo && symbolInfo.value) {
                // Recursively resolve in case the value is another alias
                return resolveSymbols(symbolInfo.value, definedSymbols);
            }
        }
        
        // Otherwise, replace symbols as part of a larger expression
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
    return resolved;
}

function evaluateSymbolicExpression(expression: string, definedSymbols: Map<string, SymbolInfo>): EvaluationResult {
    let resolved = resolveSymbols(expression, definedSymbols);
    
    try {
        resolved = resolved.replace(/([0-9A-F]+)H/gi, "0x$1");
        resolved = resolved.replace(/>([0-9A-F]+)/gi, "0x$1");
        resolved = resolved.replace(/([01]+)B/gi, "0b$1");
        
        const remainingWords = resolved.match(/(?<!0x|0b)\b[a-zA-Z_][a-zA-Z0-9_]*/g);
        if (remainingWords) {
            for (const word of remainingWords) {
                const symbolInfo = definedSymbols.get(word);
                if (symbolInfo && !symbolInfo.value) {
                    return { value: null, errorSymbol: word };
                }
            }
        }

        const result = mexp.eval(resolved, [], {});
        if (typeof result === 'number' && !isNaN(result)) {
            return { value: result };
        }
    } catch (e) {
        return { value: null };
    }
    return { value: null };
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
    
    context.subscriptions.push(
        vscode.languages.registerCompletionItemProvider('tms-assembly', {
            provideCompletionItems(document, position, token, context) {
                const line = document.lineAt(position);
                const linePrefix = line.text.substr(0, position.character);
                const trimmedPrefix = linePrefix.trimLeft();

                if (trimmedPrefix.length === linePrefix.length && !trimmedPrefix.includes(' ')) {
                    return Array.from(KNOWN_INSTRUCTIONS).map(instr => new vscode.CompletionItem(instr, vscode.CompletionItemKind.Keyword));
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
                        
                        if(context.triggerCharacter === '*') return createRegisterSuggestions();
                        
                        const createLabelSuggestions = () => {
                            const symbols = documentSymbolsCache.get(document.uri.toString());
                            if (!symbols) return [];
                            return Array.from(symbols.entries()).map(([name, info]) => {
                                let kind: vscode.CompletionItemKind;
                                switch (info.type) {
                                    case 'label': case 'bss': kind = vscode.CompletionItemKind.Reference; break;
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
                                return createLabelSuggestions();
                            case OperandType.Flag: return [new vscode.CompletionItem('0'), new vscode.CompletionItem('1')];
                            case OperandType.FillMode: return [new vscode.CompletionItem('L'), new vscode.CompletionItem('XY')];
                            case OperandType.PixbltMode: return [new vscode.CompletionItem('L'), new vscode.CompletionItem('XY'), new vscode.CompletionItem('B')];
                        }
                    }
                }
                
                return undefined;
            }
        }, ' ', ',', '*')
    );
    
    context.subscriptions.push(
        vscode.languages.registerHoverProvider('tms-assembly', {
            provideHover(document, position, token) {
                const range = document.getWordRangeAtPosition(position);
                if (!range) return null;
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
                if (!wordRange) return undefined;
                
                const word = document.getText(wordRange);
                const symbols = documentSymbolsCache.get(document.uri.toString());

                if (symbols && symbols.has(word)) {
                    const symbolInfo = symbols.get(word)!;
                    return new vscode.Location(symbolInfo.uri, symbolInfo.range);
                }

                return undefined;
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

function defineSymbol(name: string, type: 'label' | 'equ' | 'set' | 'bss', value: string | null, range: vscode.Range | null, definedSymbols: Map<string, SymbolInfo>, uri: vscode.Uri, diagnostics: vscode.Diagnostic[]) {
    if (!range) return;
    const existingSymbol = definedSymbols.get(name);
    if (existingSymbol && existingSymbol.type !== 'set') {
        const diagnostic = new vscode.Diagnostic(range, `Duplicate symbol definition: '${name}'`, vscode.DiagnosticSeverity.Error);
        diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(existingSymbol.uri, existingSymbol.range), 'First defined here')];
        diagnostics.push(diagnostic);
    } else {
        definedSymbols.set(name, { uri, range, value, type });
    }
}

async function parseSymbols(doc: vscode.TextDocument, definedSymbols: Map<string, SymbolInfo>, processedFiles: Set<string>, diagnostics: vscode.Diagnostic[]): Promise<void> {
    if (processedFiles.has(doc.uri.toString())) {
        return; 
    }
    processedFiles.add(doc.uri.toString());
    
    const includePromises: Promise<void>[] = [];
    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const includeMatch = line.text.trim().match(/^\.include\s+"([^"]+)"/i);
        if (includeMatch) {
            const fileName = includeMatch[1];
            const currentDir = path.dirname(doc.uri.fsPath);
            const includeUri = vscode.Uri.file(path.join(currentDir, fileName));
            
            const promise = new Promise<void>((resolve) => {
                vscode.workspace.openTextDocument(includeUri).then(
                    (includedDoc) => {
                        parseSymbols(includedDoc, definedSymbols, processedFiles, diagnostics).then(resolve);
                    },
                    (err) => {
                        const range = new vscode.Range(lineIndex, line.text.indexOf(fileName), lineIndex, line.text.indexOf(fileName) + fileName.length);
                        diagnostics.push(new vscode.Diagnostic(range, `Included file not found: ${fileName}`, vscode.DiagnosticSeverity.Error));
                        resolve(); 
                    }
                );
            });
            includePromises.push(promise);
        }
    }
    await Promise.all(includePromises);

    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const text = line.text;
        const trimmedText = text.trim();
        
        if (trimmedText.length === 0 || trimmedText.startsWith(';') || trimmedText.match(/^\.include/i)) {
            continue;
        }

        const symbolRange = (symbol: string) => {
            const index = text.indexOf(symbol);
            return index >= 0 ? new vscode.Range(lineIndex, index, lineIndex, index + symbol.length) : null;
        };
        
        const equateMatch = trimmedText.match(/\b([a-zA-Z_][a-zA-Z0-9_]+)\s+\.(equ|set)\s+(.+)/i);
        const bssMatch = trimmedText.match(/\.bss\s+([a-zA-Z_][a-zA-Z0-9_]+)/i);
        const labelMatch = trimmedText.match(/^([a-zA-Z_][a-zA-Z0-9_]+):/);
        
        if (equateMatch) {
            const equateName = equateMatch[1];
            defineSymbol(equateName, equateMatch[2].toLowerCase() as 'equ'|'set', equateMatch[3].trim(), symbolRange(equateName), definedSymbols, doc.uri, diagnostics);
        } else if (bssMatch) {
            const bssName = bssMatch[1];
            defineSymbol(bssName, 'bss', null, symbolRange(bssName), definedSymbols, doc.uri, diagnostics);
        } else if (labelMatch) {
            const labelName = labelMatch[1];
            defineSymbol(labelName, 'label', null, symbolRange(labelName), definedSymbols, doc.uri, diagnostics);
        } else if (line.firstNonWhitespaceCharacterIndex === 0) {
            const firstWordRaw = trimmedText.split(/\s+/)[0];
            if (!KNOWN_INSTRUCTIONS.has(firstWordRaw.toUpperCase()) && !KNOWN_DIRECTIVES.has(firstWordRaw.toLowerCase())) {
                defineSymbol(firstWordRaw, 'label', null, symbolRange(firstWordRaw), definedSymbols, doc.uri, diagnostics);
            }
        }
    }
}

async function updateDiagnostics(doc: vscode.TextDocument, collection: vscode.DiagnosticCollection): Promise<void> {
    if (doc.languageId !== 'tms-assembly') return;

    const diagnostics: vscode.Diagnostic[] = [];
    const definedSymbols = new Map<string, SymbolInfo>();
    const processedFiles = new Set<string>();

    await parseSymbols(doc, definedSymbols, processedFiles, diagnostics);
    documentSymbolsCache.set(doc.uri.toString(), definedSymbols);

    for (let lineIndex = 0; lineIndex < doc.lineCount; lineIndex++) {
        const line = doc.lineAt(lineIndex);
        const lineWithoutComment = line.text.split(';')[0];
        const text = lineWithoutComment.trim();
        
        if (text.length === 0) continue;
        
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
        const operandStr = parts.slice(mnemonicIndex + 1).join(' ');
        const upperMnemonic = mnemonic.toUpperCase();
        
        if (KNOWN_INSTRUCTIONS.has(upperMnemonic)) {
            const rule = INSTRUCTION_RULES.get(upperMnemonic)!;
            const operandParts = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
            
            const minOps = rule.minOperands ?? rule.operands.length;
            if (operandParts.length < minOps || operandParts.length > rule.operands.length) {
                const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, lineWithoutComment.trimRight().length);
                diagnostics.push(new vscode.Diagnostic(range, `Invalid operand count for ${upperMnemonic}. Expected ${minOps} to ${rule.operands.length}, but got ${operandParts.length}.`, vscode.DiagnosticSeverity.Error));
                continue;
            }
            
            let currentSearchIndex = line.text.toUpperCase().indexOf(upperMnemonic) + upperMnemonic.length;
            
            for (let i = 0; i < operandParts.length; i++) {
                const originalOperandValue = operandParts[i];
                let operandStartIndex = line.text.indexOf(originalOperandValue, currentSearchIndex);
                if (operandStartIndex === -1) {
                     operandStartIndex = line.text.toUpperCase().indexOf(originalOperandValue.toUpperCase(), currentSearchIndex);
                }

                if (operandStartIndex === -1) continue;
                
                const accurateRange = new vscode.Range(lineIndex, operandStartIndex, lineIndex, operandStartIndex + originalOperandValue.length);
                currentSearchIndex = operandStartIndex + originalOperandValue.length;
                
                const operandValue = resolveSymbols(originalOperandValue, definedSymbols);
                const expectedType = rule.operands[i];
                let isValid = false;
                
                const checkLabel = (op: string) => isLabelFormat(op) && definedSymbols.has(op);
                
                switch (expectedType) {
                    case OperandType.Register: isValid = isRegister(operandValue); break;
                    case OperandType.Immediate:
                        const evalResult = evaluateSymbolicExpression(operandValue, definedSymbols);
                        isValid = evalResult.value !== null;
                        if (!isValid && evalResult.errorSymbol) {
                            const symInfo = definedSymbols.get(evalResult.errorSymbol);
                            const symType = symInfo ? `a memory location defined by .${symInfo.type}` : 'an undefined symbol';
                            diagnostics.push(new vscode.Diagnostic(accurateRange, `Symbol '${evalResult.errorSymbol}' is ${symType} and cannot be used in this expression.`, vscode.DiagnosticSeverity.Error));
                            continue;
                        }
                        break;
                    case OperandType.Constant: isValid = isConstant(operandValue); break;
                    case OperandType.Flag: isValid = isFlag(operandValue); break;
                    case OperandType.FillMode: isValid = isFillMode(operandValue); break;
                    case OperandType.PixbltMode: isValid = isPixbltMode(operandValue); break;
                    case OperandType.RegisterList: isValid = true; break;
                    case OperandType.Label:
                        isValid = isLabelFormat(operandValue) && definedSymbols.has(operandValue);
                        if (isLabelFormat(operandValue) && !isValid) {
                            diagnostics.push(new vscode.Diagnostic(accurateRange, `Undefined label: '${operandValue}'`, vscode.DiagnosticSeverity.Error));
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
                    diagnostics.push(new vscode.Diagnostic(accurateRange, message, vscode.DiagnosticSeverity.Error));
                }
            }
        } else if (upperMnemonic === '.BSS') {
            const bssOperands = operandStr.split(',').map(s => s.trim()).filter(s => s.length > 0);
            
            if (bssOperands.length < 2 || bssOperands.length > 3) {
                const range = new vscode.Range(lineIndex, line.firstNonWhitespaceCharacterIndex, lineIndex, line.text.length);
                diagnostics.push(new vscode.Diagnostic(range, '.bss directive requires 2 or 3 operands: symbol, size, [alignment].', vscode.DiagnosticSeverity.Error));
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
            
            if (alignment) {
                const alignmentResult = evaluateSymbolicExpression(alignment, definedSymbols);
                 if (alignmentResult.value === null) {
                    if (alignmentResult.errorSymbol) {
                        const symInfo = definedSymbols.get(alignmentResult.errorSymbol);
                        const symType = symInfo ? `a memory location defined by .${symInfo.type}` : 'an undefined symbol';
                        diagnostics.push(new vscode.Diagnostic(getOperandRange(alignment), `Symbol '${alignmentResult.errorSymbol}' is ${symType} and cannot be used in a mathematical expression.`, vscode.DiagnosticSeverity.Error));
                    } else {
                        diagnostics.push(new vscode.Diagnostic(getOperandRange(alignment), `Alignment operand for .bss must be a valid numeric value or expression.`, vscode.DiagnosticSeverity.Error));
                    }
                }
            }
        }
    }
    
    collection.set(doc.uri, diagnostics);
}

export function deactivate() {}