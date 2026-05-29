/**
 * Main-thread RPC client for the evaluator worker.
 */

type RpcResponse = { type: "res"; id: string; ok: boolean; result?: unknown; error?: any };

export class EvaluatorWorkerClient {
  private worker: Worker;
  private nextId = 0;
  private pending = new Map<string, { resolve: (v: unknown) => void; reject: (r: Error) => void }>();

  constructor(worker: Worker) {
    this.worker = worker;
    this.worker.addEventListener("message", this.onMessage.bind(this));
  }

  private onMessage(ev: MessageEvent): void {
    const data = ev.data as RpcResponse;
    if (!data || data.type !== "res") return;
    const entry = this.pending.get(data.id);
    if (!entry) return;
    this.pending.delete(data.id);
    if (data.ok) entry.resolve(data.result);
    else {
      const err = new Error(data.error?.message ?? "Worker error");
      try {
        (err as any).worker = data.error;
      } catch {}
      entry.reject(err);
    }
  }

  private call(method: string, params: Record<string, unknown> | undefined): Promise<unknown> {
    return new Promise((resolve, reject) => {
      const id = `r-${++this.nextId}`;
      this.pending.set(id, { resolve, reject });
      this.worker.postMessage({ type: "req", id, method, params });
    });
  }

  async init(options: { wasmUrl: string }): Promise<void> {
    await this.call("init", options as unknown as Record<string, unknown>);
  }

  async evaluate(expression: string, context?: unknown): Promise<unknown> {
    return await this.call("evaluate", { expression, context });
  }

  terminate(): void {
    this.worker.terminate();
    for (const [, entry] of this.pending) entry.reject(new Error("Worker terminated"));
    this.pending.clear();
  }
}

export default EvaluatorWorkerClient;
