// Documentation panel for jexl-3000.
//
// Renders a single continuous scrollable list of all doc entries, grouped by
// section. The left nav highlights the section in view and scrolls to it on
// click. Search spans all categories globally.

import {
  DOC_CATEGORIES,
  DOC_ENTRIES,
  type DocCategory,
  type DocEntry,
} from "./doc-data.js";
import type { LanguageWorkerClient } from "./worker-client.js";

// ── Persistence ───────────────────────────────────────────────────────────────

const STORAGE_KEY_BASE_URL = "jexl-doc-base-url";

export const DEFAULT_DOC_BASE_URL = "http://localhost:5174";

export function getDocBaseUrl(): string {
  return localStorage.getItem(STORAGE_KEY_BASE_URL) ?? DEFAULT_DOC_BASE_URL;
}

function setDocBaseUrl(url: string): void {
  localStorage.setItem(STORAGE_KEY_BASE_URL, url);
}

// ── Doc panel sizing (persisted) ──────────────────────────────────────────

const STORAGE_KEY_WIDTH = "jexl-doc-panel-width";
const DEFAULT_PANEL_WIDTH = 380; // px
const MIN_PANEL_WIDTH = 220;
const MAX_PANEL_WIDTH = 1200;

let panelWidth = DEFAULT_PANEL_WIDTH;

function loadPanelWidth(): number {
  try {
    const raw = localStorage.getItem(STORAGE_KEY_WIDTH);
    if (raw) {
      const n = parseInt(raw, 10);
      if (!Number.isNaN(n)) return Math.min(MAX_PANEL_WIDTH, Math.max(MIN_PANEL_WIDTH, n));
    }
  } catch {
    /* ignore */
  }
  return DEFAULT_PANEL_WIDTH;
}

function savePanelWidth(w: number): void {
  try {
    localStorage.setItem(STORAGE_KEY_WIDTH, String(w));
  } catch {
    /* ignore */
  }
}

function applyPanelWidth(): void {
  const panel = document.getElementById("doc-panel");
  const resizer = document.getElementById("doc-panel-resizer");
  if (panel) panel.style.width = `${panelWidth}px`;
  if (resizer) resizer.style.right = `${panelWidth}px`;
}

function createDragOverlay(cursor: string): HTMLElement {
  const overlay = document.createElement("div");
  overlay.style.cssText = `position: fixed; inset: 0; z-index: 9998; cursor: ${cursor};`;
  document.body.appendChild(overlay);
  return overlay;
}

// ── State ─────────────────────────────────────────────────────────────────────

let searchQuery = "";
let panelOpen = false;
let activeCategory: DocCategory | null = null;
let sectionObserver: IntersectionObserver | null = null;
let codeHighlighter: ((code: string) => string) | null = null;

// Highlighting worker + queue state
let highlightObserver: IntersectionObserver | null = null;
let highlightQueue: HTMLElement[] = [];
let highlightWorker: Worker | null = null;
let pendingRequests = new Map<string, HTMLElement>();
let sharedWorkerClient: LanguageWorkerClient | null = null;

/**
 * Set the shared language worker client for off-main-thread highlighting.
 * When set, the doc panel will use it instead of creating its own worker.
 */
export function setWorkerClient(client: LanguageWorkerClient | null): void {
  sharedWorkerClient = client;
}

const requestIdle = (cb: (deadline?: any) => void) => {
  if ((window as any).requestIdleCallback) return (window as any).requestIdleCallback(cb);
  return window.setTimeout(() => cb({ timeRemaining: () => 50 }), 50);
};

const cancelIdle = (id: number) => {
  if ((window as any).cancelIdleCallback) return (window as any).cancelIdleCallback(id);
  return window.clearTimeout(id);
};

/**
 * Set (or replace) the function used to syntax-highlight jexl-3000 code in
 * doc-panel example blocks.  Pass `null` to revert to plain-text escaping.
 * Triggers a re-render of the entry list so code blocks update immediately.
 */
export function setCodeHighlighter(
  fn: ((code: string) => string) | null,
): void {
  codeHighlighter = fn;
  // Only re-render doc entries if the panel is currently open. Otherwise
  // defer rendering until the user opens the panel to avoid doing expensive
  // highlighting work at startup while workers are still parsing.
  if (panelOpen) renderEntryList();
}

/**
 * Programmatic refresh helper — re-render category nav and entry list.
 * Useful after DOC_ENTRIES are updated dynamically (e.g. hot-reload).
 */
export function refreshDocPanel(): void {
  renderCategoryNav();
  if (panelOpen) renderEntryList();
}

// ── Anchor navigation ─────────────────────────────────────────────────────────

/**
 * Open the doc panel and scroll to (or highlight) a specific entry by id.
 * Called from the hover link handler as well as the hash fragment on load.
 */
export function openDocPanelToEntry(entryId: string): void {
  openPanel();
  requestAnimationFrame(() => {
    // If searching, clear so the entry is visible
    if (searchQuery) {
      searchQuery = "";
      const searchEl = elById<HTMLInputElement>("doc-search");
      if (searchEl) searchEl.value = "";
      renderEntryList();
    }
    scrollToEntry(entryId);
  });
}

function scrollToEntry(entryId: string): void {
  const target = document.getElementById(`doc-entry-${entryId}`);
  if (target) {
    target.scrollIntoView({ behavior: "smooth", block: "center" });
    target.classList.add("doc-entry--highlight");
    setTimeout(() => target.classList.remove("doc-entry--highlight"), 2000);
  }
}

// ── DOM Helpers ───────────────────────────────────────────────────────────────

function elById<T extends HTMLElement>(id: string): T {
  return document.getElementById(id) as T;
}

// ── Panel open/close ──────────────────────────────────────────────────────────

function openPanel(): void {
  panelOpen = true;
  elById("doc-panel").classList.add("doc-panel--open");
  elById("doc-panel-backdrop").classList.add("doc-panel-backdrop--visible");
  // Ensure panel width is applied and the resizer is visible
  panelWidth = loadPanelWidth();
  applyPanelWidth();
  // Show resizer after the panel transition finishes so it moves with the panel
  scheduleResizerVisibility(true);
  window.dispatchEvent(new Event("resize"));
  // Render the (potentially large) entry list only when the panel is opened.
  renderEntryList();
}

function closePanel(): void {
  panelOpen = false;
  elById("doc-panel").classList.remove("doc-panel--open");
  elById("doc-panel-backdrop").classList.remove("doc-panel-backdrop--visible");
  // Hide the resizer after the panel finishes closing so it doesn't snap
  // Hide resizer immediately so it doesn't remain visible while panel closes
  const resizerEl = document.getElementById("doc-panel-resizer");
  if (resizerEl) resizerEl.style.display = "none";
  window.dispatchEvent(new Event("resize"));
}

function togglePanel(): void {
  if (panelOpen) closePanel();
  else openPanel();
}

// Show or hide the resizer only after the panel's opening/closing transition
// finishes so the resizer doesn't appear at its final position mid-animation.
function scheduleResizerVisibility(show: boolean): void {
  const panel = document.getElementById("doc-panel");
  const resizer = document.getElementById("doc-panel-resizer");
  if (!panel || !resizer) return;

  const apply = () => {
    resizer.style.display = show ? "block" : "none";
  };

  // If there's no transition (or it's already at the target), apply immediately
  const cs = window.getComputedStyle(panel);
  const hasTransition = cs.transition && cs.transition !== "all 0s ease 0s";
  if (!hasTransition) {
    apply();
    return;
  }

  let done = false;
  const onEnd = (ev: TransitionEvent) => {
    if (ev.propertyName === "transform") {
      if (!done) {
        done = true;
        apply();
        panel.removeEventListener("transitionend", onEnd as any);
        clearTimeout(fallback);
      }
    }
  };

  // Fallback in case transitionend doesn't fire
  const fallback = window.setTimeout(() => {
    if (!done) {
      done = true;
      apply();
      panel.removeEventListener("transitionend", onEnd as any);
    }
  }, 350);

  panel.addEventListener("transitionend", onEnd as any);
}

// ── Category nav ──────────────────────────────────────────────────────────────

function scrollToSection(cat: DocCategory): void {
  const heading = document.getElementById(`doc-section-${cat}`);
  const list = elById("doc-entry-list");
  if (heading && list) {
    list.scrollTo({ top: heading.offsetTop - 6, behavior: "smooth" });
  }
}

function setActiveCategory(cat: DocCategory | null): void {
  activeCategory = cat;
  renderCategoryNav();
}

function renderCategoryNav(): void {
  const nav = elById("doc-category-nav");
  if (!nav) return;

  nav.innerHTML = DOC_CATEGORIES.map((c) => {
    const active = activeCategory === c.id;
    return `<button
      class="doc-cat-btn${active ? " doc-cat-btn--active" : ""}"
      data-cat="${c.id}"
      title="${c.label}">
      <span class="doc-cat-icon">${c.icon}</span>
      <span class="doc-cat-label">${c.label}</span>
    </button>`;
  }).join("");

  nav.querySelectorAll<HTMLButtonElement>(".doc-cat-btn").forEach((btn) => {
    btn.addEventListener("click", () => {
      const cat = btn.dataset.cat as DocCategory;
      // If searching, clear first so all entries are visible
      if (searchQuery) {
        searchQuery = "";
        const searchEl = elById<HTMLInputElement>("doc-search");
        if (searchEl) searchEl.value = "";
        renderEntryList(() => scrollToSection(cat));
      } else {
        scrollToSection(cat);
      }
    });
  });
}

// ── Entry rendering ───────────────────────────────────────────────────────────

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function renderEntry(entry: DocEntry): string {
  const typeBadge = (t?: string): string =>
    t ? `<span class="doc-type-badge">${escapeHtml(t)}</span>` : "";

  const io =
    entry.inputType || entry.outputType
      ? `<div class="doc-entry-io">
          ${entry.inputType ? `<span class="doc-io-label">in:</span>${typeBadge(entry.inputType)}` : ""}
          ${entry.inputType && entry.outputType ? `<span class="doc-io-arrow">→</span>` : ""}
          ${entry.outputType ? `<span class="doc-io-label">out:</span>${typeBadge(entry.outputType)}` : ""}
        </div>`
      : "";

  const syntax = entry.syntax
    ? `<pre class="doc-syntax"><code>${escapeHtml(entry.syntax)}</code></pre>`
    : "";

  const examples = entry.examples.length
    ? `<div class="doc-examples">
        <div class="doc-examples-title">Examples</div>
        ${entry.examples
          .map(
            (ex) => `
          <div class="doc-example">
            ${ex.label ? `<div class="doc-example-label">${escapeHtml(ex.label)}</div>` : ""}
            <pre class="doc-example-code"><code class="doc-code needs-highlight" data-src="${encodeURIComponent(
              ex.code,
            )}">${escapeHtml(ex.code)}</code></pre>
            ${ex.result ? `<div class="doc-example-result">→ <code>${escapeHtml(ex.result)}</code></div>` : ""}
          </div>`,
          )
          .join("")}
      </div>`
    : "";

  return `
    <div class="doc-entry" id="doc-entry-${escapeHtml(entry.id)}">
      <div class="doc-entry-header">
        <span class="doc-entry-name">${escapeHtml(entry.name)}</span>
        ${io}
      </div>
      <div class="doc-entry-summary">${escapeHtml(entry.summary)}</div>
      ${syntax}
      <div class="doc-entry-desc">${escapeHtml(entry.description)}</div>
      ${examples}
    </div>`;
}

function getFilteredEntries(): DocEntry[] {
  if (!searchQuery.trim()) return DOC_ENTRIES;
  const q = searchQuery.toLowerCase();
  return DOC_ENTRIES.filter(
    (e) =>
      e.name.toLowerCase().includes(q) ||
      e.summary.toLowerCase().includes(q) ||
      e.description.toLowerCase().includes(q) ||
      e.id.toLowerCase().includes(q) ||
      e.examples.some(
        (ex) =>
          ex.code.toLowerCase().includes(q) ||
          (ex.label ?? "").toLowerCase().includes(q),
      ),
  );
}

/**
 * Render the full entry list.
 * When not searching, entries are grouped by category with section headings.
 * When searching, all matching entries are shown flat with a result count.
 * @param afterRender optional callback fired after the DOM is updated
 */
function renderEntryList(afterRender?: () => void): void {
  const list = elById("doc-entry-list");
  if (!list) return;

  // Tear down existing observers/queues before rebuilding DOM
  sectionObserver?.disconnect();
  sectionObserver = null;
  highlightObserver?.disconnect();
  highlightObserver = null;
  highlightQueue = [];
  pendingRequests.clear();

  const entries = getFilteredEntries();

  if (entries.length === 0) {
    list.innerHTML = `<div class="doc-empty">No results for "<em>${escapeHtml(searchQuery)}</em>"</div>`;
    setupHighlighting(list);
    afterRender?.();
    return;
  }

  if (searchQuery.trim()) {
    // Flat list with match count banner
    list.innerHTML =
      `<div class="doc-search-count">${entries.length} result${entries.length === 1 ? "" : "s"}</div>` +
      entries.map(renderEntry).join("");
    setupHighlighting(list);
    afterRender?.();
    return;
  }

  // Grouped by category with section headings
  const html: string[] = [];

  // Quick tip: special variables available inside element-level expressions
  html.push(`
    <div class="doc-tip">Special variables inside element-level expressions: <strong>this</strong>, <strong>index</strong>, <strong>acc</strong> — <a href="#" class="doc-tip-link" data-open-doc="index-var">Learn more</a></div>
  `);

  for (const cat of DOC_CATEGORIES) {
    const catEntries = entries.filter((e) => e.category === cat.id);
    if (catEntries.length === 0) continue;

    html.push(`
      <div class="doc-section">
        <h3 class="doc-section-heading" id="doc-section-${cat.id}">${escapeHtml(cat.label)}</h3>
      </div>
    `);

    html.push(...catEntries.map(renderEntry));
  }

  list.innerHTML = html.join("");
  setupHighlighting(list);

  // Attach IntersectionObserver to section headings so the nav highlights
  // whichever section is currently in view
  const headings = list.querySelectorAll<HTMLElement>(".doc-section-heading");
  if (headings.length > 0) {
    sectionObserver = new IntersectionObserver(
      (entries) => {
        // Find the topmost visible heading
        const visible = entries
          .filter((e) => e.isIntersecting)
          .sort((a, b) => a.boundingClientRect.top - b.boundingClientRect.top);
        if (visible.length > 0) {
          const id = visible[0]!.target.id.replace(
            "doc-section-",
            "",
          ) as DocCategory;
          setActiveCategory(id);
        }
      },
      { root: list, threshold: 0.1 },
    );
    headings.forEach((h) => sectionObserver!.observe(h));
  }

  afterRender?.();
}

// ── Lazy highlighting (IntersectionObserver + idle queue + worker) ───────

function initHighlightWorker(): void {
  // Prefer the shared language worker client when available.
  if (sharedWorkerClient || highlightWorker) return;
  try {
    // Worker source is built by the bundler (Vite) from this TS module.
    highlightWorker = new Worker(new URL("./highlight-worker.ts", import.meta.url), { type: "module" });
    highlightWorker.addEventListener("message", (ev: MessageEvent) => {
      const data = ev.data as { id: string; html: string };
      if (!data || !data.id) return;
      const el = pendingRequests.get(data.id);
      if (el) {
        el.innerHTML = data.html;
        el.classList.remove("needs-highlight");
        pendingRequests.delete(data.id);
      }
    });
  } catch (e) {
    // Worker not available — we'll fallback to main-thread highlighting.
    highlightWorker = null;
  }
}

/**
 * Initialize the highlight worker with Tree-sitter/WASM asset URLs and the
 * highlights.scm URL so the worker can perform full tree-sitter highlighting.
 */
export function initHighlightWorkerWith(options: {
  treeSitterWasmUrl: string;
  languageWasmUrl: string;
  highlightsUrl?: string;
}): void {
  initHighlightWorker();
  if (!highlightWorker) return;
  try {
    highlightWorker.postMessage({
      type: "init",
      treeSitterWasmUrl: options.treeSitterWasmUrl,
      languageWasmUrl: options.languageWasmUrl,
      highlightsUrl: options.highlightsUrl,
    });
  } catch {
    // ignore
  }
}

function setupHighlighting(list: HTMLElement): void {
  // If there's nothing to highlight, do nothing
  const nodes = Array.from(list.querySelectorAll<HTMLElement>(".doc-code.needs-highlight"));
  if (nodes.length === 0) return;

  initHighlightWorker();

  // IntersectionObserver to detect visible code blocks
  highlightObserver = new IntersectionObserver(
    (entries) => {
      for (const e of entries) {
        if (e.isIntersecting) {
          const el = e.target as HTMLElement;
          highlightObserver!.unobserve(el);
          enqueueHighlight(el);
        }
      }
    },
    { root: list, threshold: 0.1 },
  );

  nodes.forEach((n) => highlightObserver!.observe(n));

  // Immediately enqueue elements that are already visible inside the
  // `list` container. This fixes the case where the panel is opened on
  // page load and visible blocks never intersect after observation.
  // Defer the initial visibility check so layout and any transitions finish.
  // Use viewport-relative bounding rects (list vs element) which is reliable
  // even when offsets are affected by positioned ancestors.
  try {
    const checkVisible = () => {
      try {
        const listRect = list.getBoundingClientRect();
        for (const n of nodes) {
          const r = n.getBoundingClientRect();
          if (r.bottom > listRect.top && r.top < listRect.bottom) {
            try {
              highlightObserver!.unobserve(n);
            } catch {}
            enqueueHighlight(n);
          }
        }
      } catch {
        // ignore
      }
    };

    // Run on the next two frames to be robust against CSS transitions
    requestAnimationFrame(() => {
      checkVisible();
      requestAnimationFrame(() => checkVisible());
    });
  } catch {
    // ignore - defensive for environments without RAF
  }
}

let idleHandle = 0;

function enqueueHighlight(el: HTMLElement): void {
  // Assign a stable id used for worker responses
  let hid = el.dataset.hid;
  if (!hid) {
    hid = `h-${Math.random().toString(36).slice(2, 9)}`;
    el.dataset.hid = hid;
  }
  highlightQueue.push(el);
  // Schedule processing during idle time
  if (!idleHandle) {
    idleHandle = requestIdle(() => processHighlightQueue());
  }
}

function processHighlightQueue(): void {
  idleHandle = 0;
  const batchSize = 3;
  let count = 0;
  while (highlightQueue.length && count < batchSize) {
    const el = highlightQueue.shift()!;
    const hid = el.dataset.hid!;
    const srcEnc = el.dataset.src || "";
    const code = decodeURIComponent(srcEnc);

    if (sharedWorkerClient) {
      // Use the shared language worker client (Promise-based RPC).
      void sharedWorkerClient.highlightHtml(code).then((html) => {
        el.innerHTML = html;
        el.classList.remove("needs-highlight");
      }).catch(() => {
        el.innerHTML = codeHighlighter ? codeHighlighter(code) : escapeHtml(code);
        el.classList.remove("needs-highlight");
      });
    } else if (highlightWorker) {
      pendingRequests.set(hid, el);
      highlightWorker.postMessage({ id: hid, code });
    } else {
      // Fallback: do highlighting on main thread but during idle time
      const html = codeHighlighter ? codeHighlighter(code) : escapeHtml(code);
      el.innerHTML = html;
      el.classList.remove("needs-highlight");
    }
    count += 1;
  }

  if (highlightQueue.length) {
    idleHandle = requestIdle(() => processHighlightQueue());
  } else {
    // If worker exists and no pending jobs, optionally terminate worker
    // Keep worker alive so subsequent renders are fast; if desired, call
    // highlightWorker!.terminate(); highlightWorker = null;
  }
}

// ── Initialization ────────────────────────────────────────────────────────────

export function initDocPanel(): void {
  // Close button
  elById("doc-panel-close").addEventListener("click", closePanel);

  // Backdrop click → close
  elById("doc-panel-backdrop").addEventListener("click", closePanel);

  // Toggle button in header
  elById("doc-toggle-btn").addEventListener("click", togglePanel);

  // Search — global across all entries
  const searchEl = elById<HTMLInputElement>("doc-search");
  searchEl.addEventListener("input", () => {
    searchQuery = searchEl.value;
    renderEntryList();
  });

  // Clear search
  elById("doc-search-clear").addEventListener("click", () => {
    searchQuery = "";
    searchEl.value = "";
    renderEntryList();
  });

  // Initial render
  renderCategoryNav();

  // Delegated handler for in-panel "Learn more" links that should open
  // the documentation panel to a specific entry. Elements declare the
  // target entry id via `data-open-doc="<entryId>"`.
  document.addEventListener("click", (ev: MouseEvent) => {
    const target = ev.target as HTMLElement | null;
    if (!target) return;
    const el = target.closest('[data-open-doc]') as HTMLElement | null;
    if (!el) return;
    const entryId = el.getAttribute("data-open-doc");
    if (!entryId) return;
    ev.preventDefault();
    openDocPanelToEntry(entryId);
  });

  // Note: doc-panel open/closed state is no longer persisted.

  // Apply persisted width and wire up the resizer drag handle
  const resizerEl = document.getElementById("doc-panel-resizer");
  panelWidth = loadPanelWidth();
  applyPanelWidth();
  if (resizerEl) {
    // Show only if panel is open
    resizerEl.style.display = panelOpen ? "block" : "none";

    resizerEl.addEventListener("mousedown", (startEvent) => {
      startEvent.preventDefault();

      const overlay = createDragOverlay("col-resize");
      resizerEl.classList.add("dragging");

      const startX = (startEvent as MouseEvent).clientX;
      const startW = panelWidth;

      let rafId = 0;

      function onMove(e: MouseEvent) {
        cancelAnimationFrame(rafId);
        rafId = requestAnimationFrame(() => {
          // Panel is anchored to the right. Moving the resizer to the left
          // (smaller clientX) should increase the panel width, so invert
          // the delta.
          const delta = startX - e.clientX;
          const newW = Math.min(
            MAX_PANEL_WIDTH,
            Math.max(MIN_PANEL_WIDTH, startW + delta),
          );
          panelWidth = newW;
          applyPanelWidth();
          window.dispatchEvent(new Event("resize"));
        });
      }

      function onUp() {
        cancelAnimationFrame(rafId);
        overlay.remove();
        resizerEl?.classList.remove("dragging");
        savePanelWidth(panelWidth);
        document.removeEventListener("mousemove", onMove);
        document.removeEventListener("mouseup", onUp);
      }

      document.addEventListener("mousemove", onMove);
      document.addEventListener("mouseup", onUp);
    });
  }

  // Handle ?doc={entryId} query param (from external hover links)
  const searchParams = new URLSearchParams(window.location.search);
  const docParam = searchParams.get("doc");
  if (docParam) {
    openDocPanelToEntry(docParam);
    searchParams.delete("doc");
    const newSearch = searchParams.toString();
    history.replaceState(
      null,
      "",
      window.location.pathname + (newSearch ? `?${newSearch}` : "") + window.location.hash,
    );
  }

  // Handle #doc-{entryId} hash in the URL (from hover popup links)
  const hash = window.location.hash;
  if (hash.startsWith("#doc-")) {
    const entryId = hash.slice(5);
    if (entryId) {
      openDocPanelToEntry(entryId);
      history.replaceState(
        null,
        "",
        window.location.pathname + window.location.search,
      );
    }
  }

  // Handle postMessage from hover popup links
  window.addEventListener("message", (ev: MessageEvent) => {
    if (
      ev.data &&
      typeof ev.data === "object" &&
      ev.data.type === "jexl-open-doc"
    ) {
      openDocPanelToEntry(String(ev.data.entryId));
    }
  });
  // Remove the static boot-hidden class so transitions re-enable after init.
  try {
    requestAnimationFrame(() => {
      document.getElementById("doc-panel")?.classList.remove("doc-panel-boot-hidden");
    });
  } catch {
    /* ignore */
  }
}
