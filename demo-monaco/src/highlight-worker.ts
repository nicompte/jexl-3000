/* Highlight worker — attempts to initialize Tree-sitter inside the worker
 * and use the same `highlightCodeHtml` implementation from
 * `jexl-3000-monaco`. If initialization hasn't completed or fails, the
 * worker will fall back to a minimal escape-only response.
 */

import { initializeTreeSitter, loadHighlightsQuery } from "jexl-3000-monaco";
import { highlightCodeHtml } from "jexl-3000-monaco";

let highlightsQuery: string | null = null;

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

async function initTreeSitterIfNeeded(options: { treeSitterWasmUrl: string; languageWasmUrl: string; highlightsUrl?: string } | null) {
  if (!options) return;
  try {
    await initializeTreeSitter({
      treeSitterWasmUrl: options.treeSitterWasmUrl,
      languageWasmUrl: options.languageWasmUrl,
    });
    if (options.highlightsUrl) {
      try {
        highlightsQuery = await loadHighlightsQuery(options.highlightsUrl);
      } catch {
        highlightsQuery = null;
      }
    }
  } catch (e) {
    // keep highlightsQuery null — worker will fallback to plain escaping
    highlightsQuery = null;
  }
}

self.addEventListener("message", (ev: MessageEvent) => {
  const data = ev.data as any;
  // Initialization message
  if (data && data.type === "init") {
    void initTreeSitterIfNeeded({
      treeSitterWasmUrl: data.treeSitterWasmUrl,
      languageWasmUrl: data.languageWasmUrl,
      highlightsUrl: data.highlightsUrl,
    });
    return;
  }

  // Highlight request
  if (!data || typeof data.code !== "string" || !data.id) return;
  (async () => {
    try {
      if (highlightsQuery) {
        const html = highlightCodeHtml(data.code, highlightsQuery);
        (self as any).postMessage({ id: data.id, html });
      } else {
        // Worker not initialized with tree-sitter — fallback to escaping
        (self as any).postMessage({ id: data.id, html: escapeHtml(data.code) });
      }
    } catch (e) {
      (self as any).postMessage({ id: data.id, html: escapeHtml(data.code) });
    }
  })();
});
