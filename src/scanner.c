#include <tree_sitter/parser.h>

enum TokenType {
    BLOCK_COMMENT,
};

static void advance(TSLexer *lexer)
{
    lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer)
{
    lexer->advance(lexer, true);
}

static bool scan_block_comment(TSLexer *lexer, int depth)
{
    bool pending_star = false;
    while (lexer->lookahead && depth > 0) {
        switch (lexer->lookahead) {
        case '*':
            advance(lexer);
            pending_star = true;
            break;
        case '/':
            if (pending_star) {
                advance(lexer);
                pending_star = false;
                --depth;
            } else {
                advance(lexer);
                if (lexer->lookahead != '*')
                    continue;
                advance(lexer);
                ++depth;
            }
            break;
        default:
            advance(lexer);
            pending_star = false;
        }
    }

    if (depth > 0)
        return false;

    lexer->result_symbol = BLOCK_COMMENT;
    return true;
}

void *tree_sitter_sixtyfps_external_scanner_create()
{
    return NULL;
}

void tree_sitter_sixtyfps_external_scanner_destroy(void *p) { }

unsigned tree_sitter_sixtyfps_external_scanner_serialize(void *p, char *buffer)
{
    return 0;
}

void tree_sitter_sixtyfps_external_scanner_deserialize(void *p, const char *b, unsigned n) { }

bool tree_sitter_sixtyfps_external_scanner_scan(void *payload, TSLexer *lexer,
                                                const bool *valid_symbols)
{
    while (iswspace(lexer->lookahead)) {
        skip(lexer);
    }

    if (lexer->lookahead == '/' && valid_symbols[BLOCK_COMMENT]) {
        advance(lexer);
        if (lexer->lookahead != '*')
            return false;
        advance(lexer);
        return scan_block_comment(lexer, /*depth=*/1);
    }

    return false;
}
