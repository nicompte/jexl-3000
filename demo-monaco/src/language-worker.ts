/**
 * Multi-purpose tree-sitter language worker.
 *
 * Handles syntax highlighting (HTML for doc panel + per-line tokens for Monaco
 * tokenization) and tree-sitter node queries off the main thread.
 *
 * Protocol: id-based RPC over postMessage.
 *   Request:  { type: "req", id: string, method: string, params: object }
 *   Response: { type: "res", id: string, ok: boolean, result?: any, error?: { message, code? } }
 *
 * Methods:
 *   init            — load tree-sitter WASM + language WASM + highlights query
 *   highlightHtml   — return HTML-highlighted code string (for doc panel)
 *   parse           — parse code, update module tree, return mapped tokens
 *   getNodeAtPosition — return serialized NodeInfo at (line, column)
 *   disposeModel    — free per-model cached state
 */

import {
  initializeTreeSitter,
  loadHighlightsQuery,
  getTokensForHighlighting,
  getNodeAtPosition as tsGetNodeAtPosition,
  getNodeInfo as tsGetNodeInfo,
  highlightCodeHtml,
  mapScope,
} from "jexl-3000-monaco";

// ── Types ─────────────────────────────────────────────────────────────────────

interface RpcRequest {
  type: "req";
  id: string;
  method: string;
  params: Record<string, unknown>;
}

interface SerializedError {
  message: string;
  name?: string;
  stack?: string;
  payload?: any;
  code?: string;
}

interface RpcResponse {
  type: "res";
  id: string;
  ok: boolean;
  result?: unknown;
  error?: SerializedError;
}

// ── State ─────────────────────────────────────────────────────────────────────

let initialized = false;
let highlightsQuery: string | null = null;

// ── Helpers ───────────────────────────────────────────────────────────────────

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function respond(id: string, result: unknown): void {
  const msg: RpcResponse = { type: "res", id, ok: true, result };
  (self as unknown as { postMessage(msg: unknown): void }).postMessage(msg);
}

function serializeError(e: unknown): SerializedError {
  if (e instanceof Error) {
    return { message: e.message, name: e.name, stack: e.stack };
  }
  if (typeof e === "string") {
    return { message: e };
  }
  try {
    return { message: JSON.stringify(e), payload: e };
  } catch {
    return { message: String(e) };
  }
}

function respondError(id: string, e: unknown, code?: string): void {
  const err = serializeError(e);
  if (code) err.code = code;
  const msg: RpcResponse = {
    type: "res",
    id,
    ok: false,
    error: err,
  };
  (self as unknown as { postMessage(msg: unknown): void }).postMessage(msg);
}

// ── Handlers ──────────────────────────────────────────────────────────────────

async function handleInit(params: Record<string, unknown>): Promise<void> {
  const { treeSitterWasmUrl, languageWasmUrl, highlightsUrl, highlightsQueryText } = params as {
    treeSitterWasmUrl: string;
    languageWasmUrl: string;
    highlightsUrl?: string;
    highlightsQueryText?: string;
  };

  await initializeTreeSitter({ treeSitterWasmUrl, languageWasmUrl });

  // Prefer pre-fetched text (avoids a duplicate network request).
  if (highlightsQueryText) {
    highlightsQuery = highlightsQueryText;
  } else if (highlightsUrl) {
    highlightsQuery = await loadHighlightsQuery(highlightsUrl);
  }

  initialized = true;
}

function handleHighlightHtml(params: Record<string, unknown>): { html: string } {
  const { code } = params as { code: string };
  if (!highlightsQuery || !initialized) return { html: escapeHtml(code) };
  try {
    return { html: highlightCodeHtml(code, highlightsQuery) };
  } catch {
    return { html: escapeHtml(code) };
  }
}

async function handleParse(params: Record<string, unknown>): Promise<unknown> {
  const { code } = params as { code: string };

  if (!highlightsQuery || !initialized) {
    return { tokens: [] };
  }

  // getTokensForHighlighting calls parseCode(code) internally, which
  // updates the module-level currentTree (used by getNodeAtPosition).
  // It then runs the highlights query and returns raw tokens.
  const rawTokens = await getTokensForHighlighting(code, highlightsQuery);

  const tokens = rawTokens.map((t) => ({
    startRow: t.startRow,
    startColumn: t.startColumn,
    endRow: t.endRow,
    endColumn: t.endColumn,
    scope: mapScope(t.scope || "text", t.text || ""),
    text: t.text,
  }));

  return { tokens };
}

function handleGetNodeAtPosition(params: Record<string, unknown>): unknown {
  const { line, column } = params as { line: number; column: number };
  const node = tsGetNodeAtPosition(line, column);
  if (!node) return null;
  return tsGetNodeInfo(node);
}

// ── Message dispatcher ────────────────────────────────────────────────────────

self.addEventListener("message", (ev: MessageEvent) => {
  const data = ev.data;
  if (!data || data.type !== "req") return;

  const { id, method, params } = data as RpcRequest;

  void (async () => {
    try {
      switch (method) {
        case "init":
          await handleInit(params);
          respond(id, { ok: true });
          break;
        case "highlightHtml":
          respond(id, handleHighlightHtml(params));
          break;
        case "parse":
          respond(id, await handleParse(params));
          break;
        case "getNodeAtPosition":
          respond(id, handleGetNodeAtPosition(params));
          break;
        case "disposeModel":
          respond(id, { ok: true });
          break;
        default:
          respondError(id, `Unknown method: ${method}`, "UNKNOWN_METHOD");
      }
    } catch (e) {
      respondError(id, e);
    }
  })();
});
