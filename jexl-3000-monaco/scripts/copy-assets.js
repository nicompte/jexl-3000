#!/usr/bin/env node
/**
 * Copy WASM and query assets into the package's assets/ directory so
 * consumers can serve them from their own public folder.
 *
 * Run manually: node scripts/copy-assets.js
 * Run automatically: postinstall (best-effort, errors ignored)
 */
import { copyFileSync, mkdirSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, "..");
const assetsDir = join(root, "assets");

mkdirSync(assetsDir, { recursive: true });

const sources = [
  // tree-sitter runtime WASM — copied from web-tree-sitter npm package
  [
    join(root, "node_modules", "web-tree-sitter", "web-tree-sitter.wasm"),
    join(assetsDir, "web-tree-sitter.wasm"),
  ],
  // jexl grammar WASM — built from tree-sitter-jexl3000
  [
    join(root, "..", "tree-sitter-jexl3000", "tree-sitter-jexl3000.wasm"),
    join(assetsDir, "tree-sitter-jexl3000.wasm"),
  ],
  // highlights query for syntax highlighting
  [
    join(root, "..", "tree-sitter-jexl3000", "queries", "highlights.scm"),
    join(assetsDir, "highlights.scm"),
  ],
  // jexl evaluator WASM (built by wasm-pack)
  [
    join(root, "..", "jexl-wasm", "pkg", "jexl_wasm_bg.wasm"),
    join(assetsDir, "jexl_wasm_bg.wasm"),
  ],
];

let copied = 0;
for (const [src, dest] of sources) {
  if (existsSync(src)) {
    copyFileSync(src, dest);
    console.log(`  copied: ${src.replace(root, ".")} → assets/${dest.split("/").pop()}`);
    copied++;
  } else {
    console.warn(`  skipped (not found): ${src.replace(root, ".")}`);
  }
}
console.log(`copy-assets: ${copied}/${sources.length} files copied`);
