import { defineConfig } from "vite";
import wasmPack from "vite-plugin-wasm-pack";
import { resolve } from "path";

export default defineConfig({
  plugins: [wasmPack("../jexl-wasm")],
  build: {
    target: ["chrome95", "firefox95"],
    lib: {
      entry: resolve(__dirname, "src/index.ts"),
      name: "Jexl3000Monaco",
      fileName: "index",
      formats: ["es"],
    },
    rollupOptions: {
      external: ["monaco-editor"],
      output: {
        globals: {
          "monaco-editor": "monaco",
        },
      },
    },
    // Don't inline the wasm — it will be served from assets
    assetsInlineLimit: 0,
  },
  assetsInclude: ["**/*.wasm"],
  resolve: {
    extensions: [".mjs", ".js", ".ts", ".jsx", ".tsx", ".json", ".wasm"],
  },
  server: {
    fs: {
      allow: ["../jexl-wasm/pkg", "../tree-sitter-jexl3000", "./node_modules", "."],
    },
  },
});
