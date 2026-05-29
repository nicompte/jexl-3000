/**
 * Main-thread RPC client for the tree-sitter language worker.
 *
 * Provides Promise-based methods for all worker operations.  A single
 * instance should be shared across monaco-setup (tokenization) and
 * doc-panel (HTML highlighting).
 */

import type { NodeInfo } from "jexl-3000-monaco";

// ── Types ─────────────────────────────────────────────────────────────────────

interface RpcResponse {
  type: "res";
  id: string;
  ok: boolean;
  result?: unknown;
  error?: any;
}

export interface WorkerInitOptions {
  treeSitterWasmUrl: string;
  languageWasmUrl: string;
  highlightsUrl?: string;
  /** Pre-fetched highlights.scm text (avoids a duplicate fetch in the worker). */
  highlightsQueryText?: string;
}

export interface MappedToken {
  startRow: number;
  startColumn: number;
  endRow: number;
  endColumn: number;
  scope: string;
  text: string;
}

export interface ParseResult {
  tokens: MappedToken[];
}

// ── Client ────────────────────────────────────────────────────────────────────

export class LanguageWorkerClient {
  private worker: Worker;
  private nextId = 0;
  private pending = new Map<
    string,
    { resolve: (value: unknown) => void; reject: (reason: Error) => void }
  >();

  constructor(worker: Worker) {
    this.worker = worker;
    this.worker.addEventListener("message", this.onMessage.bind(this));
  }

  // ── Internal RPC plumbing ─────────────────────────────────────────────────

  private onMessage(ev: MessageEvent): void {
    const data = ev.data as RpcResponse;
    if (!data || data.type !== "res") return;

    const entry = this.pending.get(data.id);
    if (!entry) return;
    this.pending.delete(data.id);

    if (data.ok) {
      entry.resolve(data.result);
    } else {
      const err = new Error(data.error?.message ?? "Worker error");
      try {
        (err as any).worker = data.error;
      } catch {}
      entry.reject(err);
    }
  }

  private call(method: string, params: Record<string, unknown>): Promise<unknown> {
    return new Promise((resolve, reject) => {
      const id = `r-${++this.nextId}`;
      this.pending.set(id, { resolve, reject });
      this.worker.postMessage({ type: "req", id, method, params });
    });
  }

  // ── Public API ────────────────────────────────────────────────────────────

  /** Initialize tree-sitter WASM inside the worker. Must be called once. */
  async init(options: WorkerInitOptions): Promise<void> {
    await this.call("init", options as unknown as Record<string, unknown>);
  }

  /** Return syntax-highlighted HTML for a code snippet (doc panel use). */
  async highlightHtml(code: string): Promise<string> {
    const res = (await this.call("highlightHtml", { code })) as { html: string };
    return res.html;
  }

  /**
   * Parse code and return mapped tokens for Monaco tokenization.
   * Also updates the worker's internal tree (for getNodeAtPosition).
   */
  async parse(code: string): Promise<ParseResult> {
    return (await this.call("parse", { code })) as ParseResult;
  }

  /** Return serialized NodeInfo at (line, column), or null. */
  async getNodeAtPosition(line: number, column: number): Promise<NodeInfo | null> {
    return (await this.call("getNodeAtPosition", { line, column })) as NodeInfo | null;
  }

  /** Free any per-model state cached inside the worker. */
  async disposeModel(modelId: string): Promise<void> {
    await this.call("disposeModel", { modelId });
  }

  /** Terminate the worker. Rejects all in-flight requests. */
  terminate(): void {
    this.worker.terminate();
    for (const [, entry] of this.pending) {
      entry.reject(new Error("Worker terminated"));
    }
    this.pending.clear();
  }
}
