import { defineConfig } from "vite";
import path from "path";

export default defineConfig({
  plugins: [
    {
      name: 'defer-css',
      transformIndexHtml(html) {
        return html.replace(
          /<link rel="stylesheet"(.*?)href="(.*?\.css)"(.*?)>/g,
          `<link rel="preload"$1href="$2"$3 as="style" onload="this.onload=null;this.rel='stylesheet'">
           <noscript><link rel="stylesheet" href="$2"></noscript>`
        )
      }
    }
  ],
  build: {
    target: ["chrome95", "firefox95"],
    rollupOptions: {
      output: {
        // rolldown-vite uses advancedChunks instead of the deprecated manualChunks
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        advancedChunks: {
          groups: [{ name: "monaco", test: /monaco-editor/ }],
        },
      } as any,
    },
  },
  server: {
    port: 5174,
    fs: {
      allow: [
        "../jexl-wasm/pkg",
        "../tree-sitter-jexl3000",
        "./node_modules",
        ".",
      ],
    },
    middlewareMode: false,
  },
  preview: {
    port: 5174
  },
  assetsInclude: ["**/*.wasm"],
  worker: {
    format: "es",
  },
  resolve: {
    alias: {
      "jexl-wasm": path.resolve(__dirname, "../jexl-wasm/pkg"),
    },
    dedupe: ["monaco-editor"],
    extensions: [".mjs", ".js", ".ts", ".jsx", ".tsx", ".json", ".wasm"],
  },
});
