// Shared types for jexl-3000-monaco.
// These interfaces are Monaco-free and can be used anywhere in the package.

// ── JSON Schema types ─────────────────────────────────────────────────────────

export type JsonSchemaType =
  | "string"
  | "number"
  | "integer"
  | "boolean"
  | "array"
  | "object"
  | "null";

export interface JsonSchema {
  type?: JsonSchemaType | JsonSchemaType[];
  properties?: Record<string, JsonSchema>;
  items?: JsonSchema;
  required?: string[];
  nullable?: boolean;
  description?: string;
  enum?: unknown[];
  [key: string]: unknown;
}

// ── Tree-sitter node (minimal interface for the parts we use) ─────────────────

export interface TreeSitterNode {
  type: string;
  text: string;
  startIndex: number;
  endIndex: number;
  startPosition: { row: number; column: number };
  endPosition: { row: number; column: number };
  isNamed: boolean;
  isMissing?: boolean;
  childCount: number;
  parent: TreeSitterNode | null;
  child(index: number): TreeSitterNode | null;
  childForFieldName(name: string): TreeSitterNode | null;
  namedDescendantForPosition(pos: {
    row: number;
    column: number;
  }): TreeSitterNode | null;
}

export interface TreeSitterTree {
  rootNode: TreeSitterNode;
  edit(edit: object): void;
}

// ── Evaluator (WASM-backed, loosely typed) ────────────────────────────────────

export interface Evaluator {
  evaluate(expression: string, context: unknown): unknown;
}
