// Monaco editor setup: language registration, theme, tokenizer, and editor instances.
// All jexl-specific language features are provided by `jexl-3000-monaco`.
//
// The module mutates the shared `state` object to store editor instances.

import * as monaco from "monaco-editor";
import {
  attachTreeSitterTokenizer,
  defineJexl3000Theme,
  JEXL3000_THEME_ID,
  initializeTreeSitter,
  registerJexl3000Language,
  registerTokensProvider,
} from "jexl-3000-monaco";
import { state } from "./state.js";
import { openDocPanelToEntry } from "./doc-panel.js";
import { LanguageWorkerClient } from "./worker-client.js";
import type { MappedToken } from "./worker-client.js";

// Base URL for jexl3000 WASM / .scm assets served from the public folder.
const ASSET_BASE = "/assets/jexl3000";

// ── Monaco web worker setup ───────────────────────────────────────────────────
// Must be configured before any monaco.editor.create() call.
// Vite resolves `new URL('...', import.meta.url)` at build time so workers are
// properly hashed and served as separate chunks.
self.MonacoEnvironment = {
  getWorker(_: unknown, label: string): Worker {
    if (label === "json") {
      return new Worker(
        new URL(
          "monaco-editor/esm/vs/language/json/json.worker.js",
          import.meta.url,
        ),
        { type: "module" },
      );
    }
    return new Worker(
      new URL(
        "monaco-editor/esm/vs/editor/editor.worker.js",
        import.meta.url,
      ),
      { type: "module" },
    );
  },
};

// ── attachTreeSitterListener ──────────────────────────────────────────────────

/**
 * Initialize tree-sitter on the main thread (for hover / completion) and
 * create a shared language worker for tokenization + doc-panel highlighting.
 *
 * Exported so other modules (or tests) can call it independently of the
 * full Monaco initialization flow.
 */
export async function attachTreeSitterListener(): Promise<void> {
  if (!state.expressionEditor) return;

  const tsWasmUrl = `${ASSET_BASE}/web-tree-sitter.wasm`;
  const langWasmUrl = `${ASSET_BASE}/tree-sitter-jexl3000.wasm`;
  const highlightsUrl = `${ASSET_BASE}/highlights.scm`;

  // 0. Pre-fetch all assets once so neither the main thread nor the worker
  //    triggers a duplicate HTTP request.  WASM bytes are turned into Blob
  //    URLs that both contexts can consume in-memory.
  let tsBlobUrl: string;
  let langBlobUrl: string;
  let highlightsQueryText: string;
  try {
    const [tsBytes, langBytes, hlText] = await Promise.all([
      fetch(tsWasmUrl).then((r) => r.arrayBuffer()),
      fetch(langWasmUrl).then((r) => r.arrayBuffer()),
      fetch(highlightsUrl).then((r) => r.text()),
    ]);
    tsBlobUrl = URL.createObjectURL(
      new Blob([tsBytes], { type: "application/wasm" }),
    );
    langBlobUrl = URL.createObjectURL(
      new Blob([langBytes], { type: "application/wasm" }),
    );
    highlightsQueryText = hlText;
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Failed to pre-fetch tree-sitter assets:", e);
    return;
  }

  // 1. Initialize tree-sitter on the main thread (hover + completion need it).
  try {
    await initializeTreeSitter({
      treeSitterWasmUrl: tsBlobUrl,
      languageWasmUrl: langBlobUrl,
    });
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Tree-sitter initialization failed:", e);
    return;
  }

  // 2. Store the highlights query (used by fallback tokenizer + doc panel).
  state.highlightsQuery = highlightsQueryText;

  // 3. Create the shared language worker and use it for tokenization.
  //    The worker receives blob URLs (same bytes, no re-fetch) and the
  //    highlights query text so it doesn't need to fetch that either.
  try {
    const client = new LanguageWorkerClient(
      new Worker(new URL("./language-worker.ts", import.meta.url), { type: "module" }),
    );
    await client.init({
      treeSitterWasmUrl: tsBlobUrl,
      languageWasmUrl: langBlobUrl,
      highlightsQueryText,
    });
    state.workerClient = client;

    attachWorkerTokenizer(client, state.expressionEditor);
    if (state.loggingEnabled)
      console.log("Language worker initialized — tokenization offloaded");
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Language worker init failed, falling back to main-thread tokenizer:", e);

    // Fallback: main-thread tokenization (same as the original behaviour).
    if (state.highlightsQuery) {
      await attachTreeSitterTokenizer(monaco, {
        editor: state.expressionEditor,
        highlightsQuery: state.highlightsQuery,
        onError: (err) => {
          if (state.loggingEnabled)
            console.warn("Tree-sitter tokenization error:", err);
        },
      });
    }
  }
}

// ── Worker-backed tokenizer ───────────────────────────────────────────────────

type LineTokens = Array<{ startIndex: number; scope: string }> | null;

/**
 * Register a Monaco token provider whose cache is populated by the language
 * worker.  On every content change the code is sent to the worker for
 * parsing + tokenization; the response updates the per-line cache and
 * triggers a Monaco re-tokenization.
 */
function attachWorkerTokenizer(
  client: LanguageWorkerClient,
  editor: monaco.editor.IStandaloneCodeEditor,
): void {
  const setCache = registerTokensProvider(monaco);

  function buildCacheFromTokens(code: string, tokens: MappedToken[]): LineTokens[] {
    const lines = code.split("\n");
    const perLine: Array<Array<{ startIndex: number; scope: string }>> = lines.map(() => []);

    for (const t of tokens) {
      perLine[t.startRow]?.push({ startIndex: t.startColumn, scope: t.scope });
    }

    return lines.map((_, idx) => {
      const lineTokens = perLine[idx] ?? [];
      lineTokens.sort((a, b) => a.startIndex - b.startIndex);
      return lineTokens.length ? lineTokens : null;
    });
  }

  async function rebuildTokens(): Promise<void> {
    try {
      const code = editor.getValue();
      if (!code) {
        setCache(null);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (editor.getModel() as any)?.tokenization?.resetTokenization();
        return;
      }

      const { tokens } = await client.parse(code);
      setCache(buildCacheFromTokens(code, tokens));
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (editor.getModel() as any)?.tokenization?.resetTokenization();
    } catch (e) {
      if (state.loggingEnabled)
        console.warn("Worker tokenization error:", e);
      setCache(null);
    }
  }

  // Seed the cache for existing content.
  void rebuildTokens();

  // Rebuild on every edit.
  editor.onDidChangeModelContent(() => void rebuildTokens());
}

// ── initializeMonaco ──────────────────────────────────────────────────────────

export async function initializeMonaco(): Promise<{
  contextEditor: monaco.editor.IStandaloneCodeEditor;
  expressionEditor: monaco.editor.IStandaloneCodeEditor;
  resultEditor: monaco.editor.IStandaloneCodeEditor;
  schemaEditor: monaco.editor.IStandaloneCodeEditor;
}> {
  // Register the JEXL language and theme
  registerJexl3000Language(monaco);
  defineJexl3000Theme(monaco);

  // Dispose previous editors (if re-initializing)
  const editorsToDispose = [
    "contextEditor",
    "expressionEditor",
    "resultEditor",
    "schemaEditor",
  ] as const;

  editorsToDispose.forEach((k) => {
    try {
      const ed = state[k];
      if (ed) {
        ed.dispose();
      }
    } catch {
      // ignore
    }
    state[k] = null;
  });

  // Dispose hover provider if previously registered
  if (state.hoverProviderDisposable) {
    try {
      state.hoverProviderDisposable.dispose();
    } catch {
      // ignore
    }
    state.hoverProviderDisposable = null;
  }

  // ── Create editors ────────────────────────────────────────────────────────
  // If a persisted playground state exists, prefer using those values so
  // the UI doesn't briefly show the bundled defaults and then replace
  // them when `loadState()` runs. Read synchronously from localStorage.
  let persistedRaw: string | null = null;
  try {
    persistedRaw = localStorage.getItem("jexl-playground-state");
  } catch {
    persistedRaw = null;
  }

  let initialContextText = JSON.stringify(state.DEFAULT_CONTEXT, null, 2);
  let initialExpressionText = state.DEFAULT_EXPRESSION;
  let initialSchemaText = JSON.stringify(state.DEFAULT_SCHEMA, null, 2);

  if (persistedRaw) {
    try {
      const parsed = JSON.parse(persistedRaw) as Record<string, string>;
      if (parsed.context) initialContextText = parsed.context;
      if (parsed.expression) initialExpressionText = parsed.expression;
      if (parsed.schema) initialSchemaText = parsed.schema;
    } catch {
      // ignore parse errors and fall back to defaults
    }
  }

  state.contextEditor = monaco.editor.create(
    document.getElementById("context-editor")!,
    {
      value: initialContextText,
      language: "json",
      theme: "vs",
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      automaticLayout: true,
      tabSize: 2,
      fontSize: 13,
    },
  );

  state.expressionEditor = monaco.editor.create(
    document.getElementById("expression-editor")!,
    {
      value: initialExpressionText,
      language: "jexl3000",
      theme: JEXL3000_THEME_ID,
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      automaticLayout: true,
      fontSize: 13,
      hover: { enabled: true },
      quickSuggestions: {
        other: true,
        comments: false,
        strings: false,
      },
      fixedOverflowWidgets: true,
    },
  );

  // Keep runtime state in sync with the initial editor contents so
  // other modules (and the completion provider when it's created) see
  // the correct starting values.
  try {
    try {
      state.currentContext = JSON.parse(initialContextText);
    } catch {
      state.currentContext = JSON.parse(JSON.stringify(state.DEFAULT_CONTEXT));
    }
    state.currentExpression = initialExpressionText;
    try {
      state.currentSchema = JSON.parse(initialSchemaText);
    } catch {
      state.currentSchema = JSON.parse(JSON.stringify(state.DEFAULT_SCHEMA));
    }
  } catch {
    // ignore any errors while syncing initial state
  }

  state.resultEditor = monaco.editor.create(
    document.getElementById("result-editor")!,
    {
      value: "",
      language: "json",
      theme: "vs",
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      automaticLayout: true,
      readOnly: true,
      fontSize: 13,
    },
  );

  state.schemaEditor = monaco.editor.create(
    document.getElementById("schema-editor")!,
    {
      value: initialSchemaText,
      language: "json",
      theme: "vs",
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      automaticLayout: true,
      fontSize: 13,
    },
  );

  // ── Apply theme ───────────────────────────────────────────────────────────

  monaco.editor.setTheme(JEXL3000_THEME_ID);

  // ── Register hover provider (WASM LanguageService) ─────────────────────────

  monaco.editor.registerCommand(
    "jexl3000.openDoc",
    (_accessor: unknown, entryId: string) => {
      openDocPanelToEntry(entryId);
    },
  );

  try {
    state.hoverProviderDisposable = monaco.languages.registerHoverProvider("jexl3000", {
      provideHover(model, position): monaco.languages.Hover | null {
        if (!state.languageService) return null;
        const expr = model.getValue();
        const byteOffset = model.getOffsetAt(position);
        const info = state.languageService.hover(expr, byteOffset) as
          | { content: string; range?: [number, number]; doc_id?: string }
          | null;
        if (!info || !info.content) return null;

        let range: monaco.IRange | undefined;
        if (info.range) {
          const start = model.getPositionAt(info.range[0]);
          const end = model.getPositionAt(info.range[1]);
          range = {
            startLineNumber: start.lineNumber,
            startColumn: start.column,
            endLineNumber: end.lineNumber,
            endColumn: end.column,
          };
        }

        let content = info.content;
        if (info.doc_id) {
          const isOurSite =
            window.location.hostname === "jexl-3000.barbotte.net" ||
            window.location.hostname === "localhost" ||
            window.location.hostname === "127.0.0.1";
          if (isOurSite) {
            const args = encodeURIComponent(JSON.stringify([info.doc_id]));
            content += `\n\n[📖 Documentation](command:jexl3000.openDoc?${args})`;
          } else {
            content += `\n\n[📖 Documentation](https://jexl-3000.barbotte.net/?doc=${encodeURIComponent(info.doc_id)})`;
          }
        }

        return {
          contents: [{ value: content, isTrusted: { enabledCommands: ["jexl3000.openDoc"] } }],
          range,
        };
      },
    });
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Failed to register hover provider:", e);
  }

  // ── Completion provider (WASM LanguageService) ────────────────────────────

  const KIND_MAP: Record<string, monaco.languages.CompletionItemKind> = {
    function: monaco.languages.CompletionItemKind.Function,
    property: monaco.languages.CompletionItemKind.Field,
    variable: monaco.languages.CompletionItemKind.Variable,
    keyword: monaco.languages.CompletionItemKind.Keyword,
  };

  try {
    monaco.languages.registerCompletionItemProvider("jexl3000", {
      triggerCharacters: [".", "|", " ", "("],
      provideCompletionItems(model, position): monaco.languages.CompletionList {
        if (!state.languageService) return { suggestions: [] };

        const expr = model.getValue();
        const byteOffset = model.getOffsetAt(position);
        const items = state.languageService.completions(expr, byteOffset) as Array<{
          label: string;
          kind: string;
          detail?: string;
          insert_text?: string;
          is_snippet?: boolean;
          sort_order: number;
        }> | null;

        if (!items || !Array.isArray(items)) return { suggestions: [] };

        const word = model.getWordUntilPosition(position);
        const range: monaco.IRange = {
          startLineNumber: position.lineNumber,
          startColumn: word.startColumn,
          endLineNumber: position.lineNumber,
          endColumn: word.endColumn,
        };

        return {
          suggestions: items.map((item) => ({
            label: item.label,
            kind: KIND_MAP[item.kind] ?? monaco.languages.CompletionItemKind.Text,
            detail: item.detail,
            insertText: item.insert_text ?? item.label,
            insertTextRules: item.is_snippet
              ? monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
              : undefined,
            range,
            sortText: String(item.sort_order).padStart(5, "0"),
          })),
        };
      },
    });
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Failed to register completion provider:", e);
  }

  // ── Signature Help provider (WASM LanguageService) ─────────────────────────

  try {
    monaco.languages.registerSignatureHelpProvider("jexl3000", {
      signatureHelpTriggerCharacters: ["(", ","],
      signatureHelpRetriggerCharacters: [","],
      provideSignatureHelp(model, position): monaco.languages.SignatureHelpResult | null {
        if (!state.languageService) return null;
        const expr = model.getValue();
        const byteOffset = model.getOffsetAt(position);
        const info = (state.languageService as unknown as {
          signatureHelp(expr: string, offset: number): {
            label: string;
            documentation?: string;
            parameters: Array<{ label: string; documentation?: string }>;
            active_parameter: number;
          } | null;
        }).signatureHelp(expr, byteOffset);
        if (!info) return null;

        return {
          value: {
            signatures: [
              {
                label: info.label,
                documentation: info.documentation ?? undefined,
                parameters: info.parameters.map((p) => ({
                  label: p.label,
                  documentation: p.documentation ?? undefined,
                })),
              },
            ],
            activeSignature: 0,
            activeParameter: info.active_parameter,
          },
          dispose() {},
        };
      },
    });
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Failed to register signature help provider:", e);
  }

  // ── Code Action provider (WASM LanguageService) ────────────────────────────

  try {
    monaco.languages.registerCodeActionProvider("jexl3000", {
      provideCodeActions(
        model,
        _range,
        context,
      ): monaco.languages.CodeActionList {
        const actions: monaco.languages.CodeAction[] = [];
        for (const marker of context.markers) {
          // Find the matching diagnostic's code_actions by message
          if (!state.languageService) continue;
          // The markers carry the message; match it against our diagnostics.
          const expr = model.getValue();
          const diags = (state.languageService as unknown as {
            validate(expr: string): Array<{
              message: string;
              code_actions?: Array<{
                title: string;
                replacement: string;
                start: number;
                end: number;
              }>;
            }>;
          }).validate(expr);
          for (const d of diags ?? []) {
            if (d.message !== marker.message) continue;
            for (const ca of d.code_actions ?? []) {
              const startPos = model.getPositionAt(ca.start);
              const endPos = model.getPositionAt(ca.end);
              actions.push({
                title: ca.title,
                kind: "quickfix",
                edit: {
                  edits: [
                    {
                      resource: model.uri,
                      textEdit: {
                        range: {
                          startLineNumber: startPos.lineNumber,
                          startColumn: startPos.column,
                          endLineNumber: endPos.lineNumber,
                          endColumn: endPos.column,
                        },
                        text: ca.replacement,
                      },
                      versionId: model.getVersionId(),
                    },
                  ],
                },
                isPreferred: true,
              });
            }
          }
        }
        return { actions, dispose() {} };
      },
    });
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Failed to register code action provider:", e);
  }

  // ── Document Formatting provider (WASM LanguageService) ────────────────────

  try {
    monaco.languages.registerDocumentFormattingEditProvider("jexl3000", {
      provideDocumentFormattingEdits(
        model,
      ): monaco.languages.TextEdit[] {
        if (!state.languageService) return [];
        const expr = model.getValue();
        const result = (state.languageService as unknown as {
          format(expr: string): string | { error: string };
        }).format(expr);
        if (typeof result !== "string") return [];
        const fullRange = model.getFullModelRange();
        return [
          {
            range: fullRange,
            text: result,
          },
        ];
      },
    });
  } catch (e) {
    if (state.loggingEnabled)
      console.warn("Failed to register formatting provider:", e);
  }

  // ── Tree-sitter tokenizer ─────────────────────────────────────────────────

  if (state.highlightMode === "treesitter") {
    await attachTreeSitterListener().catch((e) => {
      if (state.loggingEnabled)
        console.warn("Tree-sitter listener attach failed:", e);
    });
  }

  // ── Layout on window resize ───────────────────────────────────────────────

  window.addEventListener("resize", () => {
    state.contextEditor?.layout();
    state.expressionEditor?.layout();
    state.resultEditor?.layout();
    state.schemaEditor?.layout();
  });

  if (state.loggingEnabled) {
    console.log("Monaco editors initialized (monaco-setup)");
  }

  return {
    contextEditor: state.contextEditor,
    expressionEditor: state.expressionEditor,
    resultEditor: state.resultEditor,
    schemaEditor: state.schemaEditor,
  };
}

export default initializeMonaco;
