#!/usr/bin/env node
// Copy jexl-3000-monaco assets into public/assets/jexl3000/
// so they are available at /assets/jexl3000/ in the browser.

import { copyFileSync, mkdirSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, "..");
const dest = join(root, "public", "assets", "jexl3000");

// jexl-3000-monaco assets (tree-sitter grammar, highlights, etc.)
const monacoSrc = join(root, "node_modules", "jexl-3000-monaco", "assets");
const monacoAssets = [
  "web-tree-sitter.wasm",
  "tree-sitter-jexl3000.wasm",
  "highlights.scm",
];

// jexl-wasm pkg: sourced directly so it always matches the JS glue
// served by vite-plugin-wasm-pack from ../jexl-wasm/pkg.
const wasmPkgSrc = join(root, "..", "jexl-wasm", "pkg");
const wasmAssets = ["jexl_wasm_bg.wasm"];

mkdirSync(dest, { recursive: true });

let copied = 0;
let total = 0;

if (!existsSync(monacoSrc)) {
  console.warn(
    `[copy-assets] jexl-3000-monaco assets not found at ${monacoSrc}; skipping monaco assets.`,
  );
} else {
  for (const file of monacoAssets) {
    total++;
    const from = join(monacoSrc, file);
    const to = join(dest, file);
    if (existsSync(from)) {
      copyFileSync(from, to);
      console.log(`  copied ${file} to ${to}`);
      copied++;
    } else {
      console.warn(`  missing ${file} in jexl-3000-monaco assets`);
    }
  }
}

for (const file of wasmAssets) {
  total++;
  const from = join(wasmPkgSrc, file);
  const to = join(dest, file);
  if (existsSync(from)) {
    copyFileSync(from, to);
    console.log(`  copied ${file} from jexl-wasm/pkg to ${to}`);
    copied++;
  } else {
    console.warn(`  missing ${file} in jexl-wasm/pkg (run wasm-pack build ../jexl-wasm first)`);
  }
}

console.log(`[copy-assets] ${copied}/${total} asset(s) ready in public/assets/jexl3000/`);
