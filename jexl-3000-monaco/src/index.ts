/**
 * jexl-3000-monaco
 *
 * Add full jexl-3000 editor support to an existing Monaco editor:
 * syntax highlighting, hover documentation, intelligent completions,
 * and optional expression evaluation.
 *
 * Quick start:
 * ```ts
 * import { setupJexl3000 } from "jexl-3000-monaco";
 *
 * const instance = await setupJexl3000(monaco, expressionEditor, {
 *   assetBaseUrl: "/assets/jexl3000",
 * });
 *
 * // Optional: evaluate expressions
 * const result = await instance.evaluate(expressionEditor.getValue(), context);
 * ```
 */

import type * as Monaco from "monaco-editor";
import type { Evaluator } from "./types.js";
import {
  JEXL3000_THEME_ID,
  attachTreeSitterTokenizer,
  defineJexl3000Theme,
  registerJexl3000Language,
} from "./tokenizer.js";
import {
  initializeTreeSitter,
  loadHighlightsQuery,
  reset as resetTreeSitter,
} from "./tree-sitter.js";

export type { JsonSchema, JsonSchemaType, TreeSitterNode, TreeSitterTree, Evaluator } from "./types.js";
export type { TokenInfo, NodeInfo, TreeSitterInitOptions } from "./tree-sitter.js";
export {
  JEXL3000_THEME_ID,
  JEXL3000_THEME_DATA,
  registerJexl3000Language,
  defineJexl3000Theme,
  registerTokensProvider,
  mapScope,
  attachTreeSitterTokenizer,
  highlightCodeHtml,
  scopeToCssClass,
} from "./tokenizer.js";
export {
  initializeTreeSitter,
  loadHighlightsQuery,
  parseCode,
  parseCodeFresh,
  getCurrentTree,
  getRootNode,
  queryTree,
  getTokensForHighlighting,
  getTokensForHighlightingSync,
  getNodeAtPosition,
  getNodesByType,
  getNodeInfo,
  reset as resetTreeSitter,
  getLanguage,
  getParser,
} from "./tree-sitter.js";
// ── Public API ────────────────────────────────────────────────────────────────

export interface Jexl3000SetupOptions {
  /**
   * Base URL where the WASM and query assets are served, without trailing slash.
   *
   * The following files must be accessible under this URL:
   *   - `${assetBaseUrl}/web-tree-sitter.wasm`
   *   - `${assetBaseUrl}/tree-sitter-jexl3000.wasm`
   *   - `${assetBaseUrl}/highlights.scm`
   *
   * @example "/assets/jexl3000"
   */
  assetBaseUrl: string;

  /**
   * An already-initialized jexl evaluator instance.
   * When provided, `instance.evaluate()` will delegate to it.
   * Use `initJexlEvaluator()` exported from this package to create one easily.
   */
  evaluator?: Evaluator | null;

  /**
   * Whether to apply the built-in jexl3000 theme to the editor.
   * @default true
   */
  applyTheme?: boolean;

  /** Optional theme overrides merged on top of the built-in theme. */
  themeOverrides?: Partial<Monaco.editor.IStandaloneThemeData>;

  /** Enable debug logging. @default false */
  debug?: boolean;
}

export interface Jexl3000Instance {
  /** Release all Monaco provider registrations and tree-sitter state. */
  dispose(): void;
  /**
   * Evaluate a jexl-3000 expression against a context object.
   * Requires an evaluator in options.
   */
  evaluate(expression: string, context?: unknown): Promise<unknown>;
  /** The Monaco theme ID registered by this instance. */
  themeId: string;
}

/**
 * Set up full jexl-3000 editor support on an existing Monaco editor.
 *
 * @param monaco  - The Monaco module (`import * as monaco from "monaco-editor"`)
 * @param editor  - An existing `IStandaloneCodeEditor` to attach to
 * @param options - Configuration (see Jexl3000SetupOptions)
 * @returns       A promise that resolves to a Jexl3000Instance once all WASM is loaded
 */
export async function setupJexl3000(
  monaco: typeof Monaco,
  editor: Monaco.editor.IStandaloneCodeEditor,
  options: Jexl3000SetupOptions,
): Promise<Jexl3000Instance> {
  const {
    assetBaseUrl,
    evaluator = null,
    applyTheme = true,
    themeOverrides,
    debug = false,
  } = options;

  // Normalise base URL: no trailing slash
  const base = assetBaseUrl.replace(/\/$/, "");

  // ── 1. Register language + theme ──────────────────────────────────────────

  registerJexl3000Language(monaco);
  defineJexl3000Theme(monaco, themeOverrides);

  if (applyTheme) {
    monaco.editor.setTheme(JEXL3000_THEME_ID);
  }

  // Set the editor model language to jexl3000 (in case editor was created with
  // a different language or no language)
  const model = editor.getModel();
  if (model && model.getLanguageId() !== "jexl3000") {
    monaco.editor.setModelLanguage(model, "jexl3000");
  }

  // ── 2. Tree-sitter init ───────────────────────────────────────────────────

  await initializeTreeSitter({
    treeSitterWasmUrl: `${base}/web-tree-sitter.wasm`,
    languageWasmUrl: `${base}/tree-sitter-jexl3000.wasm`,
  });

  // ── 3. Load highlights query ──────────────────────────────────────────────

  const highlightsQuery = await loadHighlightsQuery(`${base}/highlights.scm`);

  // ── 4. Attach tokenizer ───────────────────────────────────────────────────

  const tokenizerDisposable = await attachTreeSitterTokenizer(monaco, {
    editor,
    highlightsQuery,
    onError: (err) => {
      if (debug) console.warn("[jexl-3000-monaco] Tokenization error:", err);
    },
  });

  // ── Instance ──────────────────────────────────────────────────────────────

  const evalFn = evaluator
    ? (expression: string, ctx?: unknown) =>
        Promise.resolve(evaluator.evaluate(expression, ctx ?? {}))
    : null;

  const instance: Jexl3000Instance = {
    themeId: JEXL3000_THEME_ID,

    dispose() {
      tokenizerDisposable.dispose();
      resetTreeSitter();
    },

    async evaluate(expression: string, ctx?: unknown): Promise<unknown> {
      if (!evalFn) {
        throw new Error(
          "[jexl-3000-monaco] No evaluator provided. Pass an evaluator instance in options.",
        );
      }
      return evalFn(expression, ctx);
    },
  };

  return instance;
}

// ── Evaluator helper ──────────────────────────────────────────────────────────

/**
 * Initialize the jexl-wasm evaluator and return an Evaluator instance.
 *
 * Import `init` and `Evaluator` from the `jexl-wasm` package in your own
 * application (it must be resolvable in your build), then pass them here.
 * The WASM binary at `wasmUrl` is fetched at runtime.
 *
 * @example
 * ```ts
 * import { init, Evaluator } from "jexl-wasm"; // your build resolves this
 * import { initJexlEvaluator, setupJexl3000 } from "jexl-3000-monaco";
 *
 * const evaluator = await initJexlEvaluator(
 *   "/assets/jexl3000/jexl_wasm_bg.wasm",
 *   { init, Evaluator },
 * );
 * const instance = await setupJexl3000(monaco, editor, {
 *   assetBaseUrl: "/assets/jexl3000",
 *   evaluator,
 * });
 * ```
 */
export async function initJexlEvaluator(
  wasmUrl: string,
  wasmPkg: {
    init: (url: string) => Promise<unknown>;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    Evaluator: new () => any;
  },
): Promise<Evaluator> {
  await wasmPkg.init(wasmUrl);
  return new wasmPkg.Evaluator() as Evaluator;
}
