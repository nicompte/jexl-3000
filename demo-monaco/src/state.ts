// Shared application state and defaults for the JEXL-3000 playground.
// This module centralizes global variables so the main app can be split
// across multiple files without duplicating or re-declaring the same state.

import type {
  Evaluator,
  JsonSchema,
  TreeSitterTree,
} from "jexl-3000-monaco";
import type { LanguageService } from "jexl-wasm";
import type * as Monaco from "monaco-editor";
import type { LanguageWorkerClient } from "./worker-client.js";

// Re-export core types from the jexl-3000-monaco package so the rest of the
// demo can import them from "./state.js" without changing many files.
export type {
  Evaluator,
  JsonSchema,
  JsonSchemaType,
  TreeSitterNode,
  TreeSitterTree,
} from "jexl-3000-monaco";

// ── Default data shapes ───────────────────────────────────────────────────────

interface DefaultContext {
  customer: {
    name: string;
    age: number;
    email: string;
    tags: string[];
  };
  order: {
    id: number;
    amount: number;
    items: Array<{ name: string; price: number }>;
  };
}

// ── Application state ─────────────────────────────────────────────────────────

export interface AppState {
  // Runtime / engine
  evaluator: Evaluator | null;

  // Current values (mutated by the UI)
  currentContext: Record<string, unknown>;
  currentExpression: string;
  currentTree: TreeSitterTree | null;
  currentSchema: JsonSchema | null;

  // Configuration / flags
  loggingEnabled: boolean;
  /** Highlighting mode: "regex" (default) or "treesitter" */
  highlightMode: "regex" | "treesitter";
  /** The loaded highlights.scm query text (set after tree-sitter initializes). */
  highlightsQuery: string | null;

  // Monaco editor instances (populated by monaco-setup)
  contextEditor: Monaco.editor.IStandaloneCodeEditor | null;
  expressionEditor: Monaco.editor.IStandaloneCodeEditor | null;
  resultEditor: Monaco.editor.IStandaloneCodeEditor | null;
  schemaEditor: Monaco.editor.IStandaloneCodeEditor | null;
  /** Disposable returned when registering hover provider (so it can be disposed) */
  hoverProviderDisposable: Monaco.IDisposable | null;
  /** WASM-backed language service for completions, hover, and validation */
  languageService: LanguageService | null;
  /** Shared tree-sitter language worker client (tokenization + doc highlighting) */
  workerClient: LanguageWorkerClient | null;

  // Defaults (exported on the same object for convenience)
  DEFAULT_CONTEXT: DefaultContext;
  DEFAULT_EXPRESSION: string;
  DEFAULT_SCHEMA: JsonSchema;
}

/**
 * Central state object exported for other modules to import and mutate.
 *
 * Note: consumers import the object and mutate its properties directly so
 * references remain consistent across modules (simple shared mutable state).
 */
export const state: AppState = {
  // Runtime / engine
  evaluator: null,

  // Current values (mutated by the UI)
  currentContext: {},
  currentExpression: "",
  currentTree: null,
  currentSchema: null,

  // Configuration / flags
  loggingEnabled: true,
  // Highlighting mode: "regex" (default) or "treesitter"
  highlightMode: "treesitter",
  highlightsQuery: null,

  // Monaco editor instances (populated by monaco-setup)
  contextEditor: null,
  expressionEditor: null,
  resultEditor: null,
  schemaEditor: null,
  // Disposable returned when registering hover provider (so it can be disposed)
  hoverProviderDisposable: null,
  languageService: null,
  workerClient: null,

  // Defaults (exported on the same object for convenience)
  DEFAULT_CONTEXT: {
    customer: {
      name: "John Doe",
      age: 30,
      email: "john@example.com",
      tags: ["vip", "verified"],
    },
    order: {
      id: 123,
      amount: 99.99,
      items: [
        { name: "Widget", price: 29.99 },
        { name: "Gadget", price: 70.0 },
      ],
    },
  },

  DEFAULT_EXPRESSION: `
{
  # Résumé des informations du client
  customerSummary: {
    fullName: customer.name | uppercase,          # Nom complet en majuscules
    isAdult: if customer.age >= 18 { true } else { false },  # Indicateur de majorité
    email: customer.email || 'Non fourni',      # Email ou 'Non fourni' si null
    tags: customer.tags | map(this | trim | lowercase)  # Nettoyer et convertir les tags en minuscules
  },

  # Résumé de la commande, incluant uniquement les articles coûteux
  orderSummary: {
    orderId: order.id,
    totalAmount: order.amount,
    costlyItems: order.items
      | filter(this.price > 50)                            # Filtrer les articles avec prix > 50
      | map({name: this.name, price: this.price | round(2)})   # Transformer pour avoir le nom et le prix arrondi
  }
}
    `,

  DEFAULT_SCHEMA: {
    type: "object",
    properties: {
      customer: {
        type: "object",
        properties: {
          name: { type: "string" },
          age: { type: "number" },
          email: { type: ["string", "null"], nullable: true },
          tags: {
            type: "array",
            items: { type: "string" },
          },
        },
        required: ["name", "age"],
      },
      order: {
        type: "object",
        properties: {
          id: { type: "number" },
          amount: { type: "number" },
          items: {
            type: "array",
            items: {
              type: "object",
              properties: {
                name: { type: "string" },
                price: { type: "number" },
              },
              required: ["name", "price"],
            },
          },
        },
        required: ["id", "amount", "items"],
      },
    },
    required: ["customer", "order"],
  },
};

/**
 * Reset the mutable runtime pieces of state to their default values.
 * Does not reset editor instance references (those are managed by monaco setup).
 */
export function resetToDefaults(): void {
  state.evaluator = null;
  state.currentContext = structuredClone(
    state.DEFAULT_CONTEXT,
  ) as unknown as Record<string, unknown>;
  state.currentExpression = state.DEFAULT_EXPRESSION;
  state.currentTree = null;
  state.currentSchema = structuredClone(state.DEFAULT_SCHEMA);

  // Ensure the language service (if present) is aware of the new runtime defaults.
  // This keeps the WASM LanguageService's internal context/schema in sync with state after a reset.
  try {
    state.languageService?.setContext(state.currentContext);
    if (state.currentSchema) state.languageService?.setSchema(state.currentSchema);
  } catch (e) {
    if (state.loggingEnabled) {
      console.warn("Failed to update language service after reset:", e);
    }
  }
}

/**
 * Convenience: apply defaults into editor instances if present.
 * This keeps editor initialization logic decoupled from the defaults.
 */
export function applyDefaultsToEditors(): void {
  // Apply default text to editors as before, but also update the runtime state
  // and notify the completion provider so it can update its internal context/schema.
  if (state.contextEditor) {
    try {
      state.contextEditor.setValue(
        JSON.stringify(state.DEFAULT_CONTEXT, null, 2),
      );

      // Keep runtime state in sync with what was written to the editor.
      state.currentContext = structuredClone(
        state.DEFAULT_CONTEXT,
      ) as unknown as Record<string, unknown>;

      // Inform language service of the new context
      try {
        state.languageService?.setContext(state.currentContext);
      } catch (e) {
        if (state.loggingEnabled)
          console.warn("Failed to update language service context:", e);
      }
    } catch (e) {
      // ignore if editors are not ready or setValue fails
      if (state.loggingEnabled)
        console.warn("Failed to set context editor value:", e);
    }
  }
  if (state.expressionEditor) {
    try {
      state.expressionEditor.setValue(state.DEFAULT_EXPRESSION);

      // Update runtime expression so other parts of the app read the correct value
      state.currentExpression = state.DEFAULT_EXPRESSION;

      // Let the language service know the expression changed if it needs to react.
      try {
        state.languageService?.setContext(state.currentContext);
      } catch (e) {
        if (state.loggingEnabled)
          console.warn(
            "Failed to notify language service of expression change:",
            e,
          );
      }
    } catch (e) {
      if (state.loggingEnabled)
        console.warn("Failed to set expression editor value:", e);
    }
  }
  if (state.schemaEditor) {
    try {
      state.schemaEditor.setValue(
        JSON.stringify(state.DEFAULT_SCHEMA, null, 2),
      );

      // Update runtime schema to match editor content
      state.currentSchema = structuredClone(state.DEFAULT_SCHEMA);

      // Inform language service of the new schema
      try {
        if (state.currentSchema) state.languageService?.setSchema(state.currentSchema);
      } catch (e) {
        if (state.loggingEnabled)
          console.warn("Failed to update language service schema:", e);
      }
    } catch (e) {
      if (state.loggingEnabled)
        console.warn("Failed to set schema editor value:", e);
    }
  }
}
