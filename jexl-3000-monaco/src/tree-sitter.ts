/**
 * Tree-sitter Integration for jexl-3000-monaco.
 *
 * This module is Monaco-free. It wraps the web-tree-sitter library and exposes
 * parsing, tokenization, and node-query utilities used by the tokenizer and hover
 * providers.
 *
 * Key difference from the demo: WASM paths are configurable via `initializeTreeSitter()`
 * rather than being hardcoded as absolute /public paths.
 */

import type { TreeSitterNode, TreeSitterTree } from "./types.js";
import { Parser, Language, Query } from "web-tree-sitter";
import type { Node as WTSNode, Tree as WTSTree } from "web-tree-sitter";

// ── Module-level state ────────────────────────────────────────────────────────

let parser: Parser | null = null;
let language: Language | null = null;
let currentTree: WTSTree | null = null;

// ── Types ─────────────────────────────────────────────────────────────────────

export interface TreeSitterInitOptions {
  /** URL to the web-tree-sitter runtime WASM (web-tree-sitter.wasm). */
  treeSitterWasmUrl: string;
  /** URL to the jexl-3000 language grammar WASM (tree-sitter-jexl3000.wasm). */
  languageWasmUrl: string;
}

export interface TokenInfo {
  startRow: number;
  startColumn: number;
  endRow: number;
  endColumn: number;
  scope: string;
  text: string;
}

export interface NodeInfo {
  type: string;
  text: string;
  startPosition: { row: number; column: number };
  endPosition: { row: number; column: number };
  startIndex: number;
  endIndex: number;
  isNamed: boolean;
  isMissing: boolean | undefined;
  parent: { type: string } | null;
  childCount: number;
}

// ── Initialization ────────────────────────────────────────────────────────────

/**
 * Initialize Tree-sitter parser and language.
 *
 * @param options.treeSitterWasmUrl - URL to serve `web-tree-sitter.wasm`
 * @param options.languageWasmUrl   - URL to serve `tree-sitter-jexl3000.wasm`
 */
export async function initializeTreeSitter(
  options: TreeSitterInitOptions,
): Promise<void> {
  try {
    // Point the Emscripten loader at the consumer-served WASM
    await Parser.init({
      locateFile: () => options.treeSitterWasmUrl,
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any);
    parser = new Parser();

    language = await Language.load(options.languageWasmUrl);
    parser.setLanguage(language);

    console.log("Tree-sitter initialized successfully");
  } catch (error) {
    console.error("Failed to initialize Tree-sitter:", error);
    throw error;
  }
}

// ── Parsing ───────────────────────────────────────────────────────────────────

/**
 * Parse code and update the module-level syntax tree.
 * Pass `edit` for incremental re-parsing.
 */
export function parseCode(
  code: string,
  edit: object | null = null,
): TreeSitterTree {
  if (!parser) {
    throw new Error("Tree-sitter not initialized");
  }

  try {
    if (edit && currentTree) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      currentTree.edit(edit as any);
      currentTree = parser.parse(code, currentTree);
    } else {
      currentTree = parser.parse(code);
    }
    if (!currentTree) throw new Error("Parser returned null");
    return currentTree as unknown as TreeSitterTree;
  } catch (error) {
    console.error("Error parsing code:", error);
    throw error;
  }
}

/**
 * Parse code into a fresh tree WITHOUT updating the module-level `currentTree`.
 * Use this for throwaway parses (e.g. inside the completion provider).
 */
export function parseCodeFresh(code: string): TreeSitterTree | null {
  if (!parser) {
    return null;
  }
  try {
    const t = parser.parse(code);
    return t as unknown as TreeSitterTree;
  } catch (error) {
    console.error("Error parsing code (fresh):", error);
    return null;
  }
}

// ── Tree accessors ────────────────────────────────────────────────────────────

export function getCurrentTree(): TreeSitterTree | null {
  return currentTree as unknown as TreeSitterTree;
}

export function getRootNode(): TreeSitterNode | null {
  return currentTree ? (currentTree.rootNode as unknown as TreeSitterNode) : null;
}

// ── Querying ──────────────────────────────────────────────────────────────────

export function queryTree(queryString: string): unknown[] {
  if (!language || !currentTree) return [];
  try {
    const query = new Query(language, queryString);
    const matches = query.matches(currentTree.rootNode);
    return matches;
  } catch (error) {
    console.error("Error querying tree:", error);
    return [];
  }
}

/**
 * Get syntax tokens for highlighting using a tree-sitter highlights query.
 *
 * @param code            - The source code to tokenize
 * @param highlightsQuery - The highlights.scm query string
 */
export async function getTokensForHighlighting(
  code: string,
  highlightsQuery: string,
): Promise<TokenInfo[]> {
  try {
    const tree = parseCode(code);

    if (!language) return [];

    const query = new Query(language, highlightsQuery);
    // captures() returns results in document order, required for Monaco's line tokenizer.
    const rawTree = tree as unknown as WTSTree;
    const captures = query.captures(rawTree.rootNode);

    const tokens: TokenInfo[] = [];

    const SKIP_SCOPES = new Set([
      "program",
      "statement",
      "expression_statement",
    ]);

    // First-match-wins: higher-priority patterns come first in the query,
    // so we track already-seen byte ranges.
    const seen = new Set<string>();

    captures.forEach((capture) => {
      const captureName = capture.name || "text";
      if (SKIP_SCOPES.has(captureName)) return;

      const node: WTSNode = capture.node;
      const key = `${node.startIndex}:${node.endIndex}`;
      if (seen.has(key)) return;
      seen.add(key);

      tokens.push({
        startRow: node.startPosition.row,
        startColumn: node.startPosition.column,
        endRow: node.endPosition.row,
        endColumn: node.endPosition.column,
        scope: captureName,
        text: node.text,
      });
    });

    return tokens;
  } catch (error) {
    console.error("Error getting tokens:", error);
    return [];
  }
}

/**
 * Synchronous version of {@link getTokensForHighlighting}.
 *
 * Uses a fresh, throw-away parse so the module-level `currentTree` is NOT
 * mutated.  Returns an empty array (instead of throwing) when the parser is
 * not yet initialized.
 */
export function getTokensForHighlightingSync(
  code: string,
  highlightsQuery: string,
): TokenInfo[] {
  if (!language || !parser) return [];
  try {
    const rawTree = parser.parse(code);
    if (!rawTree) return [];

    const query = new Query(language, highlightsQuery);
    const captures = query.captures(rawTree.rootNode);

    const SKIP_SCOPES = new Set([
      "program",
      "statement",
      "expression_statement",
    ]);

    const seen = new Set<string>();
    const tokens: TokenInfo[] = [];

    captures.forEach((capture) => {
      const captureName = capture.name || "text";
      if (SKIP_SCOPES.has(captureName)) return;

      const node: WTSNode = capture.node;
      const key = `${node.startIndex}:${node.endIndex}`;
      if (seen.has(key)) return;
      seen.add(key);

      tokens.push({
        startRow: node.startPosition.row,
        startColumn: node.startPosition.column,
        endRow: node.endPosition.row,
        endColumn: node.endPosition.column,
        scope: captureName,
        text: node.text,
      });
    });

    return tokens;
  } catch (error) {
    console.error("Error getting tokens (sync):", error);
    return [];
  }
}

// ── Node utilities ────────────────────────────────────────────────────────────

export function getNodeAtPosition(
  row: number,
  column: number,
): TreeSitterNode | null {
  if (!currentTree) return null;
  try {
    const node = currentTree.rootNode.namedDescendantForPosition({ row, column });
    return node as unknown as TreeSitterNode;
  } catch (error) {
    console.error("Error getting node at position:", error);
    return null;
  }
}

export function getNodesByType(type: string): TreeSitterNode[] {
  if (!currentTree) return [];
  const nodes: TreeSitterNode[] = [];

  function walk(node: WTSNode): void {
    if (node.type === type) nodes.push(node as unknown as TreeSitterNode);
    for (let i = 0; i < node.childCount; i++) {
      const child = node.child(i);
      if (child) walk(child);
    }
  }

  walk(currentTree.rootNode);
  return nodes;
}

export function getNodeInfo(node: TreeSitterNode | null): NodeInfo | null {
  if (!node) return null;
  return {
    type: node.type,
    text: node.text,
    startPosition: node.startPosition,
    endPosition: node.endPosition,
    startIndex: node.startIndex,
    endIndex: node.endIndex,
    isNamed: node.isNamed,
    isMissing: node.isMissing,
    parent: node.parent ? { type: node.parent.type } : null,
    childCount: node.childCount,
  };
}

// ── Query loading ─────────────────────────────────────────────────────────────

/**
 * Load a highlights.scm query from a URL.
 * Pass a full URL (e.g. `${assetBaseUrl}/highlights.scm`).
 */
export async function loadHighlightsQuery(url: string): Promise<string> {
  try {
    const response = await fetch(url);
    if (response.ok) return await response.text();
    console.warn(`loadHighlightsQuery: fetch returned ${response.status} for ${url}`);
    return "";
  } catch (error) {
    console.error("Error loading highlights query:", error);
    return "";
  }
}

// ── Lifecycle ─────────────────────────────────────────────────────────────────

/** Reset all parser state (useful for hot-reload / cleanup). */
export function reset(): void {
  currentTree = null;
  parser = null;
  language = null;
}

export function getLanguage(): unknown {
  return language;
}

export function getParser(): Parser | null {
  return parser;
}
