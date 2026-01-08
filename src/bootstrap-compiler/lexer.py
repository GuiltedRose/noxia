KEYWORDS = {
    "let", "fn", "match", "return", "if", "else", "true", "false",
    "class", "public", "private", "const", "struct", "enum", "import",
    "as", "global", "cast", "extern", "type", "while", "for", "static",
    "break", "continue"
}

# lexer will emit IDENT, not TYPE.
BUILTIN_TYPES = {
    "int", "int8", "int16", "int32", "int64",
    "uint8", "uint16", "uint32", "uint64",
    "string", "bool", "void", "ptr", "float", "error", "char", "byte",
    "null",
}

SYMBOLS = {
    '(', ')', '{', '}', '[', ']', ':', '&', ',', '=', '+', '-', '*', '%', '/',
    '<', '>', '!', '|', '.', '~', '^'
}

MULTI_CHAR_TOKENS = {
    "==", "+=", "-=", "!=", "<=", ">=", "++", "--",
    "<<", ">>", "->", "&&", "||", "=>"
}


class Token:
    def __init__(self, kind, value, line, column):
        self.kind = kind
        self.value = value
        self.line = line
        self.column = column

    def __repr__(self):
        return f"Token({self.kind}, {repr(self.value)}, line = {self.line}, col = {self.column})"


class Lexer:
    def __init__(self, source_code):
        self.src = source_code
        self.pos = 0
        self.line = 1
        self.col = 1
        self.tokens = []

    def peek(self):
        return self.src[self.pos] if self.pos < len(self.src) else None

    def peek_next(self):
        return self.src[self.pos + 1] if self.pos + 1 < len(self.src) else None

    def advance(self):
        char = self.peek()
        self.pos += 1
        if char == "\n":
            self.line += 1
            self.col = 1
        else:
            self.col += 1
        return char

    def add_token(self, kind, value, line=None, col=None):
        line = line if line is not None else self.line
        col = col if col is not None else self.col
        self.tokens.append(Token(kind, value, line, col))

    def tokenize(self):
        while self.peek() is not None:
            c = self.advance()

            if c.isspace():
                continue

            # ---- special keyword-like punctuators ----
            # &mut
            if c == '&' and self.src[self.pos:self.pos+3] == "mut":
                start_line, start_col = self.line, self.col - 1
                self.advance(); self.advance(); self.advance()  # consume m u t
                self.add_token("KEYWORD", "&mut", start_line, start_col)
                continue

            # !safe
            if c == '!' and self.src[self.pos:self.pos+4] == "safe":
                start_line, start_col = self.line, self.col - 1
                self.advance(); self.advance(); self.advance(); self.advance()  # consume s a f e
                self.add_token("KEYWORD", "!safe", start_line, start_col)
                continue

            # ---- identifiers / keywords ----
            if c.isalpha() or c == '_':
                start = self.pos - 1
                start_line, start_col = self.line, self.col - 1
                while self.peek() and (self.peek().isalnum() or self.peek() == '_'):
                    self.advance()
                value = self.src[start:self.pos]
                if value in KEYWORDS:
                    self.add_token("KEYWORD", value, start_line, start_col)
                else:
                    self.add_token("IDENT", value, start_line, start_col)
                continue

            # ---- numbers ----
            if c.isdigit():
                start = self.pos - 1
                start_line, start_col = self.line, self.col - 1
                is_float = False

                while self.peek() and self.peek().isdigit():
                    self.advance()

                if self.peek() == '.':
                    is_float = True
                    self.advance()
                    if not self.peek() or not self.peek().isdigit():
                        self.add_token("ERROR", "Invalid float format", start_line, start_col)
                        continue
                    while self.peek() and self.peek().isdigit():
                        self.advance()

                if self.peek() in ('e', 'E'):
                    is_float = True
                    self.advance()
                    if self.peek() in ('+', '-'):
                        self.advance()
                    if not self.peek() or not self.peek().isdigit():
                        self.add_token("ERROR", "Invalid float exponent", start_line, start_col)
                        continue
                    while self.peek() and self.peek().isdigit():
                        self.advance()

                value_str = self.src[start:self.pos]
                try:
                    value = float(value_str) if is_float else int(value_str)
                    kind = "FLOAT" if is_float else "INT"
                    self.add_token(kind, value, start_line, start_col)
                except ValueError:
                    self.add_token("ERROR", f"Invalid number '{value_str}'", start_line, start_col)
                continue

            # ---- string literal ----
            # ---- string literal ----
            if c == '"':
                value = ""
                start_line, start_col = self.line, self.col - 1

                escape_map = {
                    "n": "\n",
                    "r": "\r",
                    "t": "\t",
                    "0": "\0",
                    '"': '"',
                    "\\": "\\",
                }

                while self.peek() is not None and self.peek() != '"':
                    ch = self.advance()

                    if ch == "\\":  # escape sequence
                        if self.peek() is None:
                            self.add_token("ERROR", "Unterminated escape in string literal", start_line, start_col)
                            break

                        esc = self.advance()
                        if esc not in escape_map:
                            self.add_token("ERROR", f"Unknown escape \\{esc}", start_line, start_col)
                            # Either treat it literally or continue; pick one:
                            value += esc  # literal fallback
                        else:
                            value += escape_map[esc]
                    else:
                        value += ch

                if self.peek() != '"':
                    self.add_token("ERROR", "Unterminated string literal", start_line, start_col)
                    continue

                self.advance()  # closing quote
                self.add_token("STRING", value, start_line, start_col)
                continue


            # ---- char literal ----
            if c == "'":
                start_line, start_col = self.line, self.col - 1
                if self.peek() is None:
                    self.add_token("ERROR", "Unterminated char literal", start_line, start_col)
                    continue

                char_val = self.advance()
                if char_val == '\\':
                    esc = self.advance()
                    escape_map = {'n': '\n', 'r': '\r', 't': '\t', "'": "'", '"': '"', '\\': '\\'}
                    if esc not in escape_map:
                        self.add_token("ERROR", f"Unknown escape '\\{esc}'", start_line, start_col)
                        continue
                    char_val = escape_map[esc]

                if self.peek() != "'":
                    self.add_token("ERROR", "Expected closing quote for char literal", start_line, start_col)
                    continue

                self.advance()
                self.add_token("CHAR", char_val, start_line, start_col)
                continue

            # ---- comments ----
            if c == '/' and self.peek() == '/':
                while self.peek() is not None and self.peek() != '\n':
                    self.advance()
                continue

            if c == '/' and self.peek() == '*':
                self.advance()
                start_line, start_col = self.line, self.col - 2
                while self.peek() is not None:
                    if self.peek() == '*' and self.peek_next() == '/':
                        self.advance(); self.advance()
                        break
                    self.advance()
                else:
                    self.add_token("ERROR", "Unterminated block comment", start_line, start_col)
                continue

            # ---- operators / symbols ----
            if c in SYMBOLS:
                start_line, start_col = self.line, self.col - 1
                two_char = c + (self.peek() or '')
                if two_char in MULTI_CHAR_TOKENS:
                    self.advance()
                    self.add_token("OP", two_char, start_line, start_col)
                else:
                    # treat most punctuators as SYMBOL; we'll parse ops by value in parser
                    self.add_token("SYMBOL", c, start_line, start_col)
                continue

            self.add_token("ERROR", f"Unexpected character '{c}'", self.line, self.col - 1)

        self.add_token("EOF", "", self.line, self.col)
        return self.tokens
