// Worker that runs the jexl-wasm evaluator off the main thread.
import init, { Evaluator } from "jexl-wasm";

type RpcReq = { type: "req"; id: string; method: string; params?: any };
type SerializedError = { message: string; name?: string; stack?: string; payload?: any };
type RpcRes = { type: "res"; id: string; ok: boolean; result?: any; error?: SerializedError };

let evaluator: any = null;
let initialized = false;

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


self.addEventListener("message", (ev: MessageEvent) => {
  const data = ev.data as RpcReq;
  if (!data || data.type !== "req") return;

  const { id, method, params } = data;

  (async () => {
    try {
      switch (method) {
        case "init": {
          const wasmUrl = params?.wasmUrl;
          if (!wasmUrl) throw new Error("missing wasmUrl");
          await init(wasmUrl);
          evaluator = new Evaluator();
          initialized = true;
          const res: RpcRes = { type: "res", id, ok: true, result: { ok: true } };
          (self as any).postMessage(res);
          break;
        }
        case "evaluate": {
          if (!initialized || !evaluator) throw new Error("Evaluator not initialized");
          const expr = params?.expression;
          const ctx = params?.context ?? null;
          try {
            const out = evaluator.evaluate(expr, ctx);
            const res: RpcRes = { type: "res", id, ok: true, result: out };
            (self as any).postMessage(res);
          } catch (e: any) {
            const res: RpcRes = { type: "res", id, ok: false, error: serializeError(e) };
            (self as any).postMessage(res);
          }
          break;
        }
        case "dispose": {
          evaluator = null;
          initialized = false;
          const res: RpcRes = { type: "res", id, ok: true, result: { disposed: true } };
          (self as any).postMessage(res);
          break;
        }
        default: {
          const res: RpcRes = { type: "res", id, ok: false, error: { message: "unknown method" } };
          (self as any).postMessage(res);
          break;
        }
      }
    } catch (err: any) {
      const res: RpcRes = { type: "res", id, ok: false, error: serializeError(err) };
      (self as any).postMessage(res);
    }
  })();
});

export {};
