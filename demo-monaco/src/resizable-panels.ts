// Resizable panel splitters for the JEXL-3000 playground.
//
// Adds draggable handles between:
//   - left panes  ↔  right panes  (vertical / x-axis, single col-resizer)
//   - top  panes  ↕  bottom panes (horizontal / y-axis, single row-resizer)
//
// <main> uses CSS grid with 3 columns × 3 rows. The resizer handles sit
// on the 5px grid tracks and drag-update the fr values of the surrounding
// tracks.
//
// Panel sizes are expressed as percentages and persisted in localStorage.

const LAYOUT_KEY = "jexl-playground-layout";

interface PanelLayout {
  /** Left column width as % of the whole main area */
  leftColumnWidth: number;
  /** Top row height as % of the whole main area */
  topHeight: number;
}

const DEFAULT_LAYOUT: PanelLayout = {
  leftColumnWidth: 50,
  topHeight: 50,
};

// ── Persistence ───────────────────────────────────────────────────────────────

function loadLayout(): PanelLayout {
  try {
    const raw = localStorage.getItem(LAYOUT_KEY);
    if (raw) {
      const parsed = JSON.parse(raw) as Record<string, unknown>;
      // Migrate old format that stored separate leftTopHeight / rightTopHeight.
      if ("leftTopHeight" in parsed && !("topHeight" in parsed)) {
        const left = (parsed.leftTopHeight as number) ?? 50;
        const right = (parsed.rightTopHeight as number) ?? 50;
        parsed.topHeight = (left + right) / 2;
        delete parsed.leftTopHeight;
        delete parsed.rightTopHeight;
      }
      return { ...DEFAULT_LAYOUT, ...(parsed as Partial<PanelLayout>) };
    }
  } catch {
    /* ignore */
  }
  return { ...DEFAULT_LAYOUT };
}

function saveLayout(layout: PanelLayout): void {
  try {
    localStorage.setItem(LAYOUT_KEY, JSON.stringify(layout));
  } catch {
    /* ignore */
  }
}

// ── Apply sizes to DOM ────────────────────────────────────────────────────────
//
// Percentages are applied as fr values on the grid tracks so the browser
// handles all sizing math. No pixel conversion needed.

function applyLayout(layout: PanelLayout): void {
  const main = document.querySelector<HTMLElement>("main");
  if (!main) return;

  const left = layout.leftColumnWidth;
  const right = 100 - left;
  main.style.gridTemplateColumns = `${left}fr 5px ${right}fr`;

  const top = layout.topHeight;
  const bottom = 100 - top;
  main.style.gridTemplateRows = `${top}fr 5px ${bottom}fr`;
}

// ── Trigger Monaco relayout ───────────────────────────────────────────────────

function relayoutEditors(): void {
  // Monaco listens to window resize; dispatching it causes all editors to
  // recalculate their dimensions.
  window.dispatchEvent(new Event("resize"));
}

// ── Drag overlay ──────────────────────────────────────────────────────────────
//
// A transparent full-screen div placed on top of everything during a drag.
// This prevents Monaco's iframe-like surfaces from swallowing mousemove/mouseup
// events, which is the root cause of "drag doesn't stop on mouse release".

function createOverlay(cursor: string): HTMLElement {
  const overlay = document.createElement("div");
  overlay.style.cssText = `
    position: fixed;
    inset: 0;
    z-index: 9999;
    cursor: ${cursor};
  `;
  document.body.appendChild(overlay);
  return overlay;
}

// ── Drag helpers ──────────────────────────────────────────────────────────────

function attachColResizer(resizer: HTMLElement): void {
  resizer.addEventListener("mousedown", (startEvent) => {
    startEvent.preventDefault();

    const overlay = createOverlay("col-resize");
    resizer.classList.add("dragging");

    const main = document.querySelector<HTMLElement>("main")!;
    const mainW = main.getBoundingClientRect().width;
    const startPct = layout.leftColumnWidth;
    const startX = startEvent.clientX;

    let rafId = 0;

    function onMove(e: MouseEvent) {
      cancelAnimationFrame(rafId);
      rafId = requestAnimationFrame(() => {
        const deltaPct = ((e.clientX - startX) / mainW) * 100;
        const newPct = Math.min(80, Math.max(20, startPct + deltaPct));
        layout = { ...layout, leftColumnWidth: newPct };
        applyLayout(layout);
        relayoutEditors();
      });
    }

    function onUp() {
      cancelAnimationFrame(rafId);
      overlay.remove();
      resizer.classList.remove("dragging");
      saveLayout(layout);
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
    }

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  });
}

function attachRowResizer(resizer: HTMLElement): void {
  resizer.addEventListener("mousedown", (startEvent) => {
    startEvent.preventDefault();

    const overlay = createOverlay("row-resize");
    resizer.classList.add("dragging");

    const main = document.querySelector<HTMLElement>("main")!;
    const mainH = main.getBoundingClientRect().height;
    const startPct = layout.topHeight;
    const startY = startEvent.clientY;

    let rafId = 0;

    function onMove(e: MouseEvent) {
      cancelAnimationFrame(rafId);
      rafId = requestAnimationFrame(() => {
        const deltaPct = ((e.clientY - startY) / mainH) * 100;
        const newPct = Math.min(90, Math.max(10, startPct + deltaPct));
        layout = { ...layout, topHeight: newPct };
        applyLayout(layout);
        relayoutEditors();
      });
    }

    function onUp() {
      cancelAnimationFrame(rafId);
      overlay.remove();
      resizer.classList.remove("dragging");
      saveLayout(layout);
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
    }

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  });
}

// ── Public initializer ────────────────────────────────────────────────────────

// Mutable layout singleton used by drag handlers
let layout: PanelLayout = { ...DEFAULT_LAYOUT };

export function initResizablePanels(): void {
  layout = loadLayout();
  applyLayout(layout);

  const colResizer = document.querySelector<HTMLElement>(".col-resizer");
  if (colResizer) attachColResizer(colResizer);

  const rowResizer = document.querySelector<HTMLElement>(".row-resizer");
  if (rowResizer) attachRowResizer(rowResizer);

  // Ensure Monaco editors fit their containers after initial layout is applied
  relayoutEditors();
}
