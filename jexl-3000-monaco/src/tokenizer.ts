/**
 * jexl-3000-monaco Tokenizer
 *
 * Registers the `jexl3000` language, defines a syntax-coloring theme, and
 * wires up a Tree-sitter–backed token provider for a Monaco editor.
 *
 * All functions accept explicit Monaco module references and editor instances
 * rather than relying on a global state singleton — making this safe to use
 * with multiple editor instances on the same page.
 */

import type * as Monaco from "monaco-editor";
import { getTokensForHighlighting, getTokensForHighlightingSync, parseCode } from "./tree-sitter.js";
import type { TokenInfo } from "./tree-sitter.js";

// ── Transformer category map (for syntax colouring) ───────────────────────────
// Maps transform names to their primary input type category.
// Used by mapScope() to assign a more-specific CSS class.
const TRANSFORM_CATEGORY: Record<string, "string" | "number" | "array" | "object"> = {
  // string transforms
  upper: "string", lower: "string", trim: "string", trimStart: "string", trimEnd: "string",
  capitalize: "string", camelCase: "string", kebabCase: "string", snakeCase: "string",
  startCase: "string", replace: "string", replaceAll: "string", split: "string",
  padStart: "string", padEnd: "string", repeat: "string", slice: "string",
  startsWith: "string", endsWith: "string", includes: "string", substring: "string",
  truncate: "string", slug: "string", format: "string",
  // number transforms
  round: "number", floor: "number", ceil: "number", abs: "number",
  toFixed: "number", toInteger: "number", toFloat: "number", clamp: "number",
  max: "number", min: "number", pow: "number", sqrt: "number", log: "number",
  sum: "number", sumBy: "number", mean: "number", median: "number", count: "number",
  // array transforms
  sort: "array", reverse: "array", unique: "array", flatten: "array",
  flattenDeep: "array", flattenDepth: "array", chunk: "array", zip: "array",
  unzip: "array", difference: "array", union: "array", without: "array",
  compact: "array", sampleSize: "array", first: "array", last: "array",
  take: "array", drop: "array", groupBy: "array", countBy: "array",
  // object transforms
  keys: "object", values: "object", entries: "object", fromEntries: "object",
  merge: "object", mergeDeep: "object", pick: "object", omit: "object",
  keyBy: "object", invert: "object", pickBy: "object", omitBy: "object",
  set: "object",
};

// ── Token cache type ──────────────────────────────────────────────────────────

type LineTokens = Array<{ startIndex: number; scope: string }> | null;

// ── TokenizerState ────────────────────────────────────────────────────────────

class TokenizerState implements Monaco.languages.IState {
  constructor(public readonly line: number = 0) {}
  clone(): TokenizerState {
    return new TokenizerState(this.line);
  }
  equals(other: Monaco.languages.IState): boolean {
    return other instanceof TokenizerState && this.line === other.line;
  }
}

// ── Theme data ────────────────────────────────────────────────────────────────

export const JEXL3000_THEME_ID = "jexl3000-theme";

export const JEXL3000_THEME_DATA: Monaco.editor.IStandaloneThemeData = {
  base: "vs",
  inherit: true,
  rules: [
    { token: "comment", foreground: "b0b0b0", fontStyle: "italic" },
    { token: "string", foreground: "27ae60" },
    { token: "number", foreground: "e67e22" },
    { token: "constant", foreground: "e74c3c", fontStyle: "bold" },
    { token: "constant.builtin", foreground: "e74c3c", fontStyle: "bold" },
    { token: "keyword", foreground: "c0392b", fontStyle: "bold" },
    { token: "keyword.special", foreground: "c0392b", fontStyle: "bold" },
    { token: "variable", foreground: "2980b9" },
    { token: "variable.builtin", foreground: "8e44ad", fontStyle: "italic" },
    { token: "property", foreground: "2980b9" },
    { token: "operator", foreground: "e67e22" },
    { token: "punctuation", foreground: "999999" },
    { token: "punctuation.delimiter", foreground: "999999" },
    { token: "punctuation.bracket", foreground: "999999" },
    { token: "function", foreground: "8e44ad" },
    { token: "function.custom", foreground: "8e44ad" },
    { token: "transform", foreground: "6a1e8a" },
    { token: "transform.string", foreground: "1565c0", fontStyle: "bold" },
    { token: "transform.number", foreground: "1565c0", fontStyle: "bold" },
    { token: "transform.collection", foreground: "00695c", fontStyle: "bold" },
    { token: "transform.generic", foreground: "1565c0", fontStyle: "bold" },
    { token: "mrf", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.map", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.filter", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.reduce", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.sortBy", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.apply", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.find", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.findIndex", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.any", foreground: "e67e22", fontStyle: "bold" },
    { token: "mrf.all", foreground: "e67e22", fontStyle: "bold" },
  ],
  colors: {
    "editor.foreground": "#000000",
    "editor.background": "#ffffff",
  },
};

// ── Language + theme registration ─────────────────────────────────────────────

let languageRegistered = false;
let themeRegistered = false;

/**
 * Register the `jexl3000` language with Monaco (idempotent).
 */
export function registerJexl3000Language(monaco: typeof Monaco): void {
  if (languageRegistered) return;
  monaco.languages.register({ id: "jexl3000" });
  languageRegistered = true;
}

/**
 * Define the jexl3000-theme (idempotent). Call after registering the language.
 * You may optionally apply it with `monaco.editor.setTheme(JEXL3000_THEME_ID)`.
 */
export function defineJexl3000Theme(
  monaco: typeof Monaco,
  overrides?: Partial<Monaco.editor.IStandaloneThemeData>,
): void {
  if (themeRegistered && !overrides) return;
  const data = overrides
    ? {
        ...JEXL3000_THEME_DATA,
        ...overrides,
        rules: [...JEXL3000_THEME_DATA.rules, ...(overrides.rules ?? [])],
        colors: { ...JEXL3000_THEME_DATA.colors, ...overrides.colors },
      }
    : JEXL3000_THEME_DATA;
  monaco.editor.defineTheme(JEXL3000_THEME_ID, data);
  themeRegistered = true;
}

// ── Token provider registration ───────────────────────────────────────────────

/**
 * Register a Monaco tokens provider that returns scope strings derived from
 * the `scopeTokensCache`.  Most callers use `attachTreeSitterTokenizer()`
 * instead, which manages the cache automatically.
 *
 * @returns A cache setter function — call it with a fresh cache to update the
 *          provider, or with `null` to clear highlighting.
 */
export function registerTokensProvider(
  monaco: typeof Monaco,
): (cache: LineTokens[] | null) => void {
  let scopeTokensCache: LineTokens[] | null = null;

  monaco.languages.setTokensProvider("jexl3000", {
    getInitialState: () => new TokenizerState(0),
    tokenize: (_line: string, stateObj: Monaco.languages.IState) => {
      const row = stateObj instanceof TokenizerState ? stateObj.line : 0;
      const endState = new TokenizerState(row + 1);
      const lineScopes = scopeTokensCache && scopeTokensCache[row];
      if (!lineScopes || !lineScopes.length) {
        return { tokens: [{ startIndex: 0, scopes: "" }], endState };
      }
      return {
        tokens: lineScopes.map(({ startIndex, scope }) => ({
          startIndex,
          scopes: scope,
        })),
        endState,
      };
    },
  });

  return (cache: LineTokens[] | null) => {
    scopeTokensCache = cache;
  };
}

// ── Scope mapping ─────────────────────────────────────────────────────────────

/**
 * Map a raw tree-sitter capture scope to the finer-grained scope used for
 * theme colouring (e.g. `function.custom` → `transform.string`).
 */
export function mapScope(scope: string, tokenText: string): string {
  try {
    const name = tokenText.trim();
    if (!name) return scope;

    if (scope === "function.builtin") {
      return `mrf.${name}`;
    }

    if (scope === "function.custom") {
      const category = Object.hasOwn(TRANSFORM_CATEGORY, name) ? TRANSFORM_CATEGORY[name] : undefined;
      if (category === "string") return "transform.string";
      if (category === "number") return "transform.number";
      if (category === "array" || category === "object") return "transform.collection";
      return "transform.generic";
    }
  } catch {
    // fallthrough
  }
  return scope;
}

// ── HTML code highlighting ───────────────────────────────────────────────────

/** Minimal HTML entity escaping for inserting text into HTML. */
function htmlEscape(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/** Convert a row+column position to a flat char index within `code`. */
function rowColToIndex(lines: string[], row: number, col: number): number {
  let idx = 0;
  for (let r = 0; r < row && r < lines.length; r++) {
    idx += (lines[r]?.length ?? 0) + 1; // +1 for '\n'
  }
  return idx + col;
}

/**
 * Convert a tree-sitter scope string to a CSS class name.
 * e.g. `"variable.builtin"` → `"jexl-hl-variable-builtin"`
 */
export function scopeToCssClass(scope: string): string {
  return "jexl-hl-" + scope.replace(/\./g, "-");
}

/**
 * Tokenize `code` and return an HTML string where each tree-sitter token
 * is wrapped in `<span class="jexl-hl-{scope}">`.  Falls back to plain
 * HTML-escaped text when the parser is not yet initialized or tokenization
 * fails.
 *
 * @param code            - The jexl-3000 source code to highlight
 * @param highlightsQuery - The highlights.scm query string (as text)
 */
export function highlightCodeHtml(
  code: string,
  highlightsQuery: string,
): string {
  if (!code) return "";

  let tokens: TokenInfo[];
  try {
    tokens = getTokensForHighlightingSync(code, highlightsQuery);
  } catch {
    return htmlEscape(code);
  }

  if (!tokens.length) return htmlEscape(code);

  const lines = code.split("\n");

  // Convert row/col positions to flat char offsets, then map-and-sort.
  const spans = tokens
    .map((t) => ({
      start: rowColToIndex(lines, t.startRow, t.startColumn),
      end: rowColToIndex(lines, t.endRow, t.endColumn),
      scope: mapScope(t.scope || "text", t.text || ""),
    }))
    .filter((s) => s.start < s.end)
    .sort((a, b) => a.start - b.start || b.end - a.end);

  const parts: string[] = [];
  let cursor = 0;

  for (const span of spans) {
    if (span.start < cursor) continue; // overlapping span — skip
    if (span.start > cursor) {
      parts.push(htmlEscape(code.slice(cursor, span.start)));
    }
    const cls = scopeToCssClass(span.scope);
    parts.push(
      `<span class="${cls}">${htmlEscape(code.slice(span.start, span.end))}</span>`,
    );
    cursor = span.end;
  }

  if (cursor < code.length) {
    parts.push(htmlEscape(code.slice(cursor)));
  }

  return parts.join("");
}

// ── attachTreeSitterTokenizer ─────────────────────────────────────────────────

export interface AttachTokenizerOptions {
  /** The expression editor to attach to. */
  editor: Monaco.editor.IStandaloneCodeEditor;
  /** The highlights.scm query string (already loaded). */
  highlightsQuery: string;
  /** Optional: called when tokenization errors occur. */
  onError?: (err: unknown) => void;
}

/**
 * Build and maintain the per-line scope cache for `editor`, then register
 * the Monaco tokens provider.  Rebuilds whenever the editor content changes.
 *
 * @returns A disposable that stops listening when disposed.
 */
export async function attachTreeSitterTokenizer(
  monaco: typeof Monaco,
  options: AttachTokenizerOptions,
): Promise<Monaco.IDisposable> {
  const { editor, highlightsQuery, onError } = options;
  const setCache = registerTokensProvider(monaco);

  async function rebuildCache(): Promise<void> {
    try {
      const code = editor.getValue();
      if (!code) {
        setCache(null);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (editor.getModel() as any)?.tokenization?.resetTokenization();
        return;
      }

      const tokens = await getTokensForHighlighting(code, highlightsQuery);

      const lines = code.split("\n");
      const perLine: Array<Array<{ startIndex: number; scope: string }>> =
        lines.map(() => []);

      tokens.forEach((t) => {
        const startRow = t.startRow ?? 0;
        const scope = mapScope(t.scope || "text", t.text || "");
        perLine[startRow]?.push({ startIndex: t.startColumn ?? 0, scope });
      });

      const newCache: LineTokens[] = lines.map((_, lineIdx) => {
        const lineTokens = perLine[lineIdx] ?? [];
        lineTokens.sort((a, b) => a.startIndex - b.startIndex);
        return lineTokens.length ? lineTokens : null;
      });

      setCache(newCache);
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (editor.getModel() as any)?.tokenization?.resetTokenization();
    } catch (e) {
      onError?.(e);
      setCache(null);
    }
  }

  // Seed the cache for existing content
  await rebuildCache();

  // Keep cache updated on edits
  const disposable = editor.onDidChangeModelContent(() => {
    void rebuildCache();
  });

  return disposable;
}

/**
 * Convenience: rebuilds the token cache by calling parseCode + getTokensForHighlighting
 * and then forces Monaco to re-tokenize.  Useful for external triggers (e.g. initial load).
 */
export async function refreshTokenization(
  monaco: typeof Monaco,
  editor: Monaco.editor.IStandaloneCodeEditor,
  highlightsQuery: string,
): Promise<void> {
  const code = editor.getValue();
  if (!code) return;
  parseCode(code);
  // The tokens provider registered via registerTokensProvider reads its own cache;
  // just force re-tokenization so Monaco calls tokenize() again.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (editor.getModel() as any)?.tokenization?.resetTokenization();
  void monaco; // suppress unused warning
}
