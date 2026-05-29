// Small bootstrap for the JEXL-3000 Monaco playground.
//
// This file is intentionally minimal: it imports the application
// orchestration module and starts initialization when the DOM is ready.
//
// The heavy lifting (Monaco setup, tree-sitter integration, completion
// provider, evaluation loop, and state management) has been moved to
// separate modules so this entrypoint remains easy to reason about.

import { initializeApp } from "./app.js";

// Defer initialization until the DOM is ready so editors can be created
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", () => void initializeApp());
} else {
  // DOM already ready
  void initializeApp();
}

// Also export initializeApp for programmatic usage (tests, debug consoles)
export default initializeApp;
