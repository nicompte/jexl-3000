// Main application glue for the JEXL-3000 Monaco playground.
// Responsibilities:
// - Initialize editors and WASM evaluator
// - Wire completion provider into Monaco
// - Evaluate expressions and show results
// - Persist/load state to localStorage
// - Scan expressions for schema/completion warnings and display markers
//
// This file is intentionally focused on orchestration and keeps editor/tokenizer
// details in `monaco-setup.ts` and shared runtime/defaults in `state.ts`.

import { EvaluatorWorkerClient } from "./evaluator-worker-client.js";
import * as monaco from "monaco-editor";

import wasmInit, { LanguageService } from "jexl-wasm";
import { highlightCodeHtml } from "jexl-3000-monaco";
import { initializeMonaco } from "./monaco-setup.js";
import { applyDefaultsToEditors, state } from "./state.js";
import { initResizablePanels } from "./resizable-panels.js";
import { initDocPanel, setCodeHighlighter } from "./doc-panel.js";

// ── Configuration ─────────────────────────────────────────────────────────────

const LOCALSTORAGE_KEY = "jexl-playground-state";
const SAVE_INTERVAL_MS = 5000;
const SAVE_DELAY_AFTER_INIT_MS = 2000;

// ── Types ─────────────────────────────────────────────────────────────────────

interface PersistedState {
  context: string;
  expression: string;
  schema: string;
  timestamp: string;
}

interface ErrorLocation {
  startOffset: number;
  endOffset: number;
}

interface WasmDiagnostic {
  message: string;
  severity: "error" | "warning" | "info";
  start: number;
  end: number;
}

type WasmError =
  | string
  | { error: string; caused_by?: string[] }
  | null
  | undefined;

// ── Initialization ─────────────────────────────────────────────────────────────

/**
 * Initialize the entire playground application.
 * - initializes Monaco editors
 * - initializes WASM evaluator (optional/falls back gracefully)
 * - initializes completion provider and registers it with Monaco
 * - wires up event listeners and loads persisted/default state
 */
export async function initializeApp(): Promise<void> {
  try {
    if (state.loggingEnabled) {
      console.log("Initializing JEXL-3000 Playground (app.ts)...");
    }

    // Prepare evaluator worker initialization (prefetch wasm bytes and
    // create a worker to run the evaluator off the main thread).

    // Initialize Monaco editors and theme/tokenizer. This populates
    // `state.contextEditor`, `state.expressionEditor`, `state.resultEditor`,
    // and `state.schemaEditor`.
    await initializeMonaco();

    // Expose the expression editor for the inline format button script.
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (window as any).__jexl3000_expressionEditor = state.expressionEditor;

    // Initialize evaluator worker: fetch WASM bytes, create blob URL,
    // start worker and call its init RPC. If anything fails we continue
    // without an evaluator (UI will show a warning).
    let wasmBlobUrl: string | null = null;
    try {
      const wasmFetchStart = performance.now();
      try {
        const wasmResp = await fetch("/assets/jexl3000/jexl_wasm_bg.wasm");
        if (wasmResp.ok) {
          const wasmBytes = await wasmResp.arrayBuffer();
          wasmBlobUrl = URL.createObjectURL(new Blob([wasmBytes], { type: "application/wasm" }));
        }
      } catch (e) {
        if (state.loggingEnabled) console.warn("WASM prefetch failed:", e);
      }

      const wasmFetchEnd = performance.now();
      if (state.loggingEnabled)
        console.log("WASM fetch/prep took", `${(wasmFetchEnd - wasmFetchStart).toFixed(2)}ms`);

      const worker = new Worker(new URL("./evaluator-worker.ts", import.meta.url), { type: "module" });
      const client = new EvaluatorWorkerClient(worker);
      try {
        await client.init({ wasmUrl: wasmBlobUrl ?? "/assets/jexl3000/jexl_wasm_bg.wasm" });
        // Expose a small async shim that matches the previous evaluator API
        // but returns a Promise. Call sites will await this.
        state.evaluator = {
          evaluate: (expr: string, ctx?: unknown) => client.evaluate(expr, ctx),
        } as unknown as { evaluate: (s: string, c?: unknown) => Promise<unknown> };
        if (state.loggingEnabled) console.log("Evaluator worker initialized");
      } catch (e) {
        state.evaluator = null;
        if (state.loggingEnabled)
          console.warn("Evaluator worker failed to init; continuing without it", e);
        client.terminate();
      }
    } catch (err) {
      state.evaluator = null;
      if (state.loggingEnabled)
        console.warn("WASM evaluator not available; continuing without it", err);
    }

    // Initialize the WASM LanguageService on the main thread for
    // completions, hover, and validation (Monaco providers require main-thread).
    try {
      await wasmInit(wasmBlobUrl ?? "/assets/jexl3000/jexl_wasm_bg.wasm");
      state.languageService = new LanguageService();
      if (state.currentSchema) state.languageService.setSchema(state.currentSchema);
      state.languageService.setContext(state.currentContext);
      if (state.loggingEnabled) console.log("WASM LanguageService initialized on main thread");
    } catch (e) {
      state.languageService = null;
      if (state.loggingEnabled)
        console.warn("WASM LanguageService not available; continuing without it", e);
    }

    // Wire syntax highlighting into doc-panel example code blocks.
    // state.highlightsQuery is set inside initializeMonaco() once tree-sitter
    // loads successfully.  If tree-sitter failed to initialize this is null,
    // and the doc panel falls back to plain-text escaping.
    if (state.highlightsQuery) {
      setCodeHighlighter((code) =>
        highlightCodeHtml(code, state.highlightsQuery!),
      );
    }

    // If the shared language worker was initialized (inside initializeMonaco),
    // pass it to the doc panel so it uses the same worker for highlighting
    // instead of creating its own.
    if (state.workerClient) {
      try {
        const { setWorkerClient } = await import("./doc-panel.js");
        setWorkerClient(state.workerClient);
      } catch (e) {
        // ignore — shared worker is optional for the doc panel
      }
    }

    // Set up resizable panel splitters and restore saved layout
    initResizablePanels();

    // Initialize the documentation panel (bound to DOM elements in index.html)
    initDocPanel();

    // Apply default values into editors (this should update editor contents)
    // Defer non-critical work so the page becomes interactive while
    // heavy worker startup and parsing complete (socket thread work).
    const scheduleIdle = (fn: () => void) => {
      if (typeof (window as any).requestIdleCallback === "function") {
        (window as any).requestIdleCallback(() => {
          try {
            fn();
          } catch (e) {
            console.warn("deferred task failed", e);
          }
        }, { timeout: 1000 });
      } else {
        setTimeout(() => {
          try {
            fn();
          } catch (e) {
            console.warn("deferred task failed", e);
          }
        }, 0);
      }
    };

    if (state.loggingEnabled)
      console.log("Deferring non-critical startup work via requestIdleCallback");

    scheduleIdle(() => {
      // Reveal resizers now that heavy startup activity is complete so the
      // UI doesn't show awkward handles while workers parse/initialise.
      try {
        document.body.classList.remove("resizers-hidden");
      } catch {}

      // Only apply the bundled defaults if there's no persisted state in
      // localStorage. This prevents briefly showing the default expression
      // and then replacing it when the saved state is loaded.
      let applyDefaults = true;
      try {
        applyDefaults = !localStorage.getItem(LOCALSTORAGE_KEY);
      } catch {
        applyDefaults = true;
      }

      if (applyDefaults) {
        const applyDefaultsStart = performance.now();
        applyDefaultsToEditors();
        const applyDefaultsEnd = performance.now();
        if (state.loggingEnabled)
          console.log(
            "applyDefaultsToEditors (deferred) took",
            `${(applyDefaultsEnd - applyDefaultsStart).toFixed(2)}ms`,
          );
      } else {
        if (state.loggingEnabled)
          console.log("Skipping default editor values because persisted state exists");
      }
    });

    // Wire event listeners between editors and evaluation/completion logic.
    // This is cheap so run synchronously.
    if (state.loggingEnabled) console.log("Registering event listeners...");
    const listenersStart = performance.now();
    setupEventListeners();
    const listenersEnd = performance.now();
    if (state.loggingEnabled)
      console.log(
        "setupEventListeners took",
        `${(listenersEnd - listenersStart).toFixed(2)}ms`,
      );

    // Defer loading persisted state, initial evaluation, and scanning so the
    // browser can finish worker startup without blocking the main thread.
    scheduleIdle(() => {
      if (state.loggingEnabled) console.log("Loading persisted state (deferred)...");
      const loadStateStart = performance.now();
      try {
        loadState();
      } catch (e) {
        console.warn("loadState (deferred) failed", e);
      }
      const loadStateEnd = performance.now();
      if (state.loggingEnabled)
        console.log("loadState (deferred) took", `${(loadStateEnd - loadStateStart).toFixed(2)}ms`);

      if (state.loggingEnabled) console.log("Evaluating initial expression (deferred)...");
      const evalStart = performance.now();
      void evaluateExpression().then(() => {
        const evalEnd = performance.now();
        if (state.loggingEnabled)
          console.log("initial evaluateExpression (deferred) took", `${(evalEnd - evalStart).toFixed(2)}ms`);
      });

      if (state.loggingEnabled) console.log("Scanning expression warnings (deferred)...");
      const scanStart = performance.now();
      try {
        scanExpressionWarnings();
      } catch (e) {
        console.warn("scanExpressionWarnings (deferred) failed", e);
      }
      const scanEnd = performance.now();
      if (state.loggingEnabled)
        console.log("scanExpressionWarnings (deferred) took", `${(scanEnd - scanStart).toFixed(2)}ms`);
    });

    // Kick off periodic save (delayed so initialization completes first)
    setTimeout(() => {
      setInterval(saveState, SAVE_INTERVAL_MS);
    }, SAVE_DELAY_AFTER_INIT_MS);

    if (state.loggingEnabled) {
      console.log("Playground initialization complete");
    }
  } catch (error) {
    console.error("Failed to initialize playground:", error);
    // Surface a visible error to the user if possible
    try {
      alert("Failed to initialize playground. See console for details.");
    } catch {
      // ignore
    }
  }
}

// ── Event listeners ────────────────────────────────────────────────────────────

/**
 * Set up editor event listeners that keep shared state in sync and trigger
 * evaluations / completion scans as needed.
 */
export function setupEventListeners(): void {
  // Expression editor changes
  if (state.expressionEditor) {
    state.expressionEditor.onDidChangeModelContent(() => {
      try {
        state.currentExpression = state.expressionEditor!.getValue();
        // Evaluate as the user types
        void evaluateExpression();

        scanExpressionWarnings();
      } catch (e) {
        if (state.loggingEnabled)
          console.error("Error handling expression change:", e);
      }
    });
  }

  // Context editor changes
  if (state.contextEditor) {
    state.contextEditor.onDidChangeModelContent(() => {
      void (async () => {
        try {
          const contextText = state.contextEditor!.getValue();
          state.currentContext = JSON.parse(contextText) as Record<
            string,
            unknown
          >;
          state.languageService?.setContext(state.currentContext);
          await evaluateExpression();
          scanExpressionWarnings();
        } catch (err) {
          if (err instanceof SyntaxError) {
            if (state.loggingEnabled) {
              console.warn(
                "Invalid JSON context (waiting for valid JSON):",
                err.message,
              );
            }
          } else {
            console.error("Error parsing context:", err);
          }
        }
      })();
    });
  }

  // Schema editor changes
  if (state.schemaEditor) {
    state.schemaEditor.onDidChangeModelContent(() => {
      try {
        const schemaText = state.schemaEditor!.getValue();
        state.currentSchema = JSON.parse(
          schemaText,
        ) as typeof state.currentSchema;
        if (state.currentSchema) state.languageService?.setSchema(state.currentSchema);
        if (state.loggingEnabled)
          console.log("Schema updated", state.currentSchema);

        scanExpressionWarnings();
      } catch (err) {
        if (err instanceof SyntaxError) {
          if (state.loggingEnabled) {
            console.warn(
              "Invalid JSON schema (waiting for valid JSON):",
              err.message,
            );
          }
        } else {
          console.error("Error parsing schema:", err);
        }
      }
    });
  }

  // Window resize: ensure editors re-layout properly
  window.addEventListener("resize", () => {
    state.contextEditor?.layout();
    state.expressionEditor?.layout();
    state.resultEditor?.layout();
    state.schemaEditor?.layout();
  });
}

// ── Evaluation ─────────────────────────────────────────────────────────────────

/**
 * Evaluate the current expression using the WASM evaluator when available.
 * Updates `resultEditor` with JSON containing the result and duration, or
 * an error message.
 */
export async function evaluateExpression(): Promise<void> {
  const startTime = performance.now();

  try {
    const expression = state.expressionEditor
      ? state.expressionEditor.getValue()
      : "";

    if (!expression || !expression.trim()) {
      state.resultEditor?.setValue("");
      handleEvaluationError(null);
      return;
    }

    let result: unknown = null;
    let error: WasmError = null;

    if (state.evaluator) {
      try {
        // `state.evaluator.evaluate` is an async RPC-backed call when using
        // the worker shim, so await it here. If a synchronous evaluator is
        // later re-introduced this still works for Promise results.
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        result = await (state.evaluator as any).evaluate(expression, state.currentContext);
        if (state.loggingEnabled) console.log("WASM result:", result);
      } catch (wasmError) {
        if (state.loggingEnabled) {
          console.warn("WASM evaluation failed, skipping result:", wasmError);
        }
        // Normalize thrown/rejected errors into a serializable shape so
        // the UI can show a helpful message instead of "[object Object]".
        if (wasmError instanceof Error) {
          const w = (wasmError as any).worker;
          error = Object.assign({ message: wasmError.message }, w ?? {});
          if (wasmError.stack) (error as any).stack = wasmError.stack;
        } else if (typeof wasmError === "string") {
          error = wasmError;
        } else {
          try {
            error = wasmError as WasmError;
          } catch {
            error = String(wasmError) as WasmError;
          }
        }
      }
    } else {
      if (state.loggingEnabled) {
        console.warn("No WASM evaluator available; evaluation skipped");
      }
      error = "WASM evaluator not available";
    }

    const endTime = performance.now();
    const duration = (endTime - startTime).toFixed(2);

    if (error) {
      console.warn("Evaluation error:", error);
      // Ensure the displayed JSON has a friendly message field when possible.
      let displayError: unknown;
      if (typeof error === "string") {
        displayError = { message: error };
      } else if (error && typeof error === "object") {
        displayError = error;
      } else {
        displayError = { message: String(error) };
      }

      // If the worker serialized a payload, prefer that structured payload
      // (it contains `error`/`caused_by`) instead of a JSON-encoded `message`.
      try {
        if (
          displayError &&
          typeof displayError === "object" &&
          (displayError as any).payload &&
          typeof (displayError as any).payload === "object"
        ) {
          displayError = (displayError as any).payload;
        } else if (
          displayError &&
          typeof displayError === "object" &&
          typeof (displayError as any).message === "string"
        ) {
          const msg = (displayError as any).message as string;
          try {
            const parsed = JSON.parse(msg);
            if (parsed && typeof parsed === "object") displayError = parsed;
          } catch {
            // leave as-is
          }
        }
      } catch {
        // ignore any parsing errors
      }
      state.resultEditor?.setValue(
        JSON.stringify({ error: displayError, duration: `${duration}ms` }, null, 2),
      );
      if (typeof displayError === "object" && displayError !== null) {
        handleEvaluationError(displayError as WasmError);
      } else {
        handleEvaluationError(null);
      }
    } else {
      state.resultEditor?.setValue(
        JSON.stringify({ result, duration: `${duration}ms` }, null, 2),
      );
      handleEvaluationError(null);
    }

    try {
      const el = document.getElementById("update-time");
      if (el) el.textContent = `${duration}ms`;
    } catch {
      // ignore
    }

    if (state.loggingEnabled) {
      if (error) {
        console.warn("Evaluation failed", { error, duration: `${duration}ms` });
      } else {
        console.log("Evaluation succeeded", {
          result,
          duration: `${duration}ms`,
        });
      }
    }
  } catch (err) {
    console.error("Unexpected error during evaluation:", err);
    const msg = err instanceof Error ? err.message : String(err);
    state.resultEditor?.setValue(JSON.stringify({ error: msg }, null, 2));
  }
}

// ── Persistence ───────────────────────────────────────────────────────────────

/**
 * Save current editor state to localStorage
 */
export function saveState(): void {
  try {
    if (!state.contextEditor || !state.expressionEditor) return;
    const s: PersistedState = {
      context: state.contextEditor.getValue(),
      expression: state.expressionEditor.getValue(),
      schema: state.schemaEditor ? state.schemaEditor.getValue() : "",
      timestamp: new Date().toISOString(),
    };
    localStorage.setItem(LOCALSTORAGE_KEY, JSON.stringify(s));
    if (state.loggingEnabled) console.log("State saved to localStorage");
  } catch (err) {
    console.error("Failed to save state:", err);
  }
}

/**
 * Load state from localStorage and apply to editors (if present)
 */
export function loadState(): void {
  try {
    if (!state.contextEditor || !state.expressionEditor) return;
    const stored = localStorage.getItem(LOCALSTORAGE_KEY);
    if (!stored) return;

    const parsed = JSON.parse(stored) as Partial<PersistedState>;
    if (parsed.context) state.contextEditor.setValue(parsed.context);
    if (parsed.expression) state.expressionEditor.setValue(parsed.expression);
    if (parsed.schema && state.schemaEditor) {
      state.schemaEditor.setValue(parsed.schema);
      try {
        state.currentSchema = JSON.parse(
          parsed.schema,
        ) as typeof state.currentSchema;
        if (state.currentSchema) state.languageService?.setSchema(state.currentSchema);
      } catch (e) {
        if (state.loggingEnabled)
          console.warn("Saved schema is invalid JSON:", e);
      }
    }

    if (state.loggingEnabled) console.log("State loaded from localStorage");
  } catch (err) {
    console.error("Failed to load state:", err);
  }
}

// ── Warning scanning ──────────────────────────────────────────────────────────

/**
 * Ask the WASM LanguageService to validate the current expression and
 * display any diagnostics as Monaco markers and in the warning panel.
 */
export function scanExpressionWarnings(): void {
  if (!state.languageService || !state.expressionEditor) return;
  const model = state.expressionEditor.getModel();
  if (!model) return;

  const expression = state.expressionEditor.getValue();
  if (!expression || !expression.trim()) {
    monaco.editor.setModelMarkers(model, "jexl-warnings", []);
    updateWarningPanel([]);
    return;
  }

  let diagnostics: WasmDiagnostic[];
  try {
    diagnostics = state.languageService.validate(expression) as WasmDiagnostic[] ?? [];
  } catch (e) {
    if (state.loggingEnabled) console.warn("Error during WASM validate:", e);
    return;
  }

  const SEVERITY_MAP: Record<string, monaco.MarkerSeverity> = {
    error: monaco.MarkerSeverity.Error,
    warning: monaco.MarkerSeverity.Warning,
    info: monaco.MarkerSeverity.Info,
  };

  const markers: monaco.editor.IMarkerData[] = diagnostics.map((d) => {
    const start = model.getPositionAt(d.start);
    const end = model.getPositionAt(d.end);
    return {
      startLineNumber: start.lineNumber,
      startColumn: start.column,
      endLineNumber: end.lineNumber,
      endColumn: end.column,
      message: d.message,
      severity: SEVERITY_MAP[d.severity] ?? monaco.MarkerSeverity.Warning,
      source: "JEXL schema",
    };
  });

  monaco.editor.setModelMarkers(model, "jexl-warnings", markers);
  updateWarningPanel(diagnostics);
}

// ── Error / warning display ───────────────────────────────────────────────────

/**
 * Parse a "(start, end)" byte-offset pair from a WASM EvaluationError message.
 */
export function parseErrorLocation(message: string): ErrorLocation | null {
  if (!message || typeof message !== "string") return null;
  const match = message.match(/\((\d+),\s*(\d+)\)/);
  if (!match) return null;
  return {
    startOffset: parseInt(match[1]!, 10),
    endOffset: parseInt(match[2]!, 10),
  };
}

/**
 * Given a raw WASM error, create a Monaco Error marker at the reported
 * position and show the message in the warning panel.
 * Pass null/undefined to clear any previous evaluation error.
 */
export function handleEvaluationError(error: WasmError): void {
  if (!state.expressionEditor) return;
  const model = state.expressionEditor.getModel();
  if (!model) return;

  monaco.editor.setModelMarkers(model, "jexl-eval-error", []);

  const warningPanel = document.getElementById("warning-panel");

  function clearEvalErrorPanel(): void {
    if (!warningPanel) return;
    const existing = warningPanel.querySelector(".eval-error-item");
    if (existing) existing.remove();
    if (warningPanel.children.length === 0) {
      warningPanel.classList.add("hidden");
    }
  }

  if (!error) {
    clearEvalErrorPanel();
    return;
  }

  let message: string | null = null;
  if (typeof error === "string") {
    message = error;
  } else if (error && typeof error === "object") {
    const causedBy = (error as { caused_by?: string[] }).caused_by;
    if (Array.isArray(causedBy) && causedBy.length > 0) {
      message = causedBy[0] ?? null;
    } else if (typeof (error as { error?: string }).error === "string") {
      message = (error as { error: string }).error;
    } else if (typeof (error as { message?: string }).message === "string") {
      message = (error as unknown as { message: string }).message;
    } else {
      message = String(error);
    }
  } else {
    message = String(error);
  }

  if (!message) {
    clearEvalErrorPanel();
    return;
  }

  const loc = parseErrorLocation(message);

  let marker: monaco.editor.IMarkerData;
  if (loc) {
    const startPos = model.getPositionAt(loc.startOffset);
    let endPos = model.getPositionAt(loc.endOffset);
    if (
      endPos.lineNumber === startPos.lineNumber &&
      endPos.column <= startPos.column
    ) {
      const lineContent = model.getLineContent(startPos.lineNumber) || "";
      let endCol = startPos.column;
      while (
        endCol <= lineContent.length &&
        !/\s/.test(lineContent[endCol - 1]!)
      ) {
        endCol++;
      }
      if (endCol === startPos.column) endCol = startPos.column + 1;
      endPos = { lineNumber: startPos.lineNumber, column: endCol } as monaco.Position;
    }

    marker = {
      startLineNumber: startPos.lineNumber,
      startColumn: startPos.column,
      endLineNumber: endPos.lineNumber,
      endColumn: endPos.column,
      message,
      severity: monaco.MarkerSeverity.Error,
      source: "JEXL eval",
    };
  } else {
    let fallbackLine = 1;
    const lineCount = model.getLineCount();
    for (let i = 1; i <= lineCount; i++) {
      if (model.getLineContent(i).trim()) {
        fallbackLine = i;
        break;
      }
    }
    const lineLen = model.getLineContent(fallbackLine).length || 1;
    marker = {
      startLineNumber: fallbackLine,
      startColumn: 1,
      endLineNumber: fallbackLine,
      endColumn: lineLen + 1,
      message,
      severity: monaco.MarkerSeverity.Error,
      source: "JEXL eval",
    };
  }

  monaco.editor.setModelMarkers(model, "jexl-eval-error", [marker]);

  if (warningPanel) {
    clearEvalErrorPanel();
    const item = document.createElement("div");
    item.className = "warning-item warning-error eval-error-item";
    item.innerHTML = `<span>⛔ ${message}</span>`;
    warningPanel.appendChild(item);
  }
}

/**
 * Display diagnostics in the warning panel DOM element.
 */
export function updateWarningPanel(diagnostics: WasmDiagnostic[] = []): void {
  const warningPanel = document.getElementById("warning-panel");
  if (!warningPanel) return;

  const existingEvalError = warningPanel.querySelector(".eval-error-item");

  if (!diagnostics || diagnostics.length === 0) {
    Array.from(warningPanel.children)
      .filter((el) => !el.classList.contains("eval-error-item"))
      .forEach((el) => el.remove());

    if (!existingEvalError) {
      warningPanel.classList.add("hidden");
    }
    return;
  }

  Array.from(warningPanel.children)
    .filter((el) => !el.classList.contains("eval-error-item"))
    .forEach((el) => el.remove());

  const ICON_MAP: Record<string, string> = {
    error: "⛔",
    warning: "⚠️",
    info: "ℹ️",
  };

  const fragment = document.createDocumentFragment();
  diagnostics.forEach((d) => {
    const div = document.createElement("div");
    div.className = `warning-item warning-${d.severity}`;
    div.innerHTML = `<span>${ICON_MAP[d.severity] ?? "⚠️"} ${d.message}</span>`;
    fragment.appendChild(div);
  });

  if (existingEvalError) {
    warningPanel.insertBefore(fragment, existingEvalError);
  } else {
    warningPanel.appendChild(fragment);
  }
}

// ── Public API ────────────────────────────────────────────────────────────────

const api = {
  initializeApp,
  evaluateExpression,
  saveState,
  loadState,
  scanExpressionWarnings,
  updateWarningPanel,
  handleEvaluationError,
  parseErrorLocation,
  state,
};

// Attach to window for easy debugging in dev builds
declare global {
  interface Window {
    jexlPlayground?: typeof api;
  }
}

try {
  window.jexlPlayground = { ...(window.jexlPlayground ?? {}), ...api };
  if (state.loggingEnabled)
    console.log("jexlPlayground API attached to window");
} catch {
  // noop in restrictive environments
}

export default api;
