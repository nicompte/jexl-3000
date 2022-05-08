#include <tree_sitter/parser.h>
#include <wctype.h>

enum TokenType {
  TERNARY_QMARK,
};

void *tree_sitter_jexl3000_external_scanner_create() { return NULL; }
void tree_sitter_jexl3000_external_scanner_destroy(void *p) {}
void tree_sitter_jexl3000_external_scanner_reset(void *p) {}
unsigned tree_sitter_jexl3000_external_scanner_serialize(void *p, char *buffer) { return 0; }
void tree_sitter_jexl3000_external_scanner_deserialize(void *p, const char *b, unsigned n) {}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static bool scan_ternary_qmark(TSLexer *lexer) {
  for(;;) {
    if (!iswspace(lexer->lookahead)) break;
    skip(lexer);
  }

  if (lexer->lookahead == '?') {
    advance(lexer);

    if (lexer->lookahead == '?') return false;

    lexer->mark_end(lexer);
    lexer->result_symbol = TERNARY_QMARK;

    if (lexer->lookahead == '.') {
      advance(lexer);
      if (iswdigit(lexer->lookahead)) return true;
      return false;
    }
    return true;
  }
  return false;
}

bool tree_sitter_jexl3000_external_scanner_scan(void *payload, TSLexer *lexer,
                                                  const bool *valid_symbols) {
  if (valid_symbols[TERNARY_QMARK]) {
    return scan_ternary_qmark(lexer);
  }

  return false;
}
