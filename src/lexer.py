KEYWORDS = {
    "let", "fn", "match", "return", "if", "else", "true", "false", "&mut", "!safe",
    "class", "public", "private", "const", "struct", "enum", "import", "as", "global", "cast"
}

TYPES = {
    "int", "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "string", "bool", "void", "ptr", "float", "class", "error", "char", "&mut", "!safe"
}

SYMBOLS = {
    '(', ')', '{', '}', ':', '&', ',', '=', '+', '-', '*', '%', '/', '<', '>', '!'
}

MULTI_CHAR_TOKENS = {
    "==", "+=", "-=", "!=", "<=", ">=", "++", "--", "<<", ">>", "->", "&&", "||"
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

            # Keywords & identifiers
            if c.isalpha() or c == '_':
                start = self.pos - 1
                start_line, start_col = self.line, self.col - 1
                while self.peek() and (self.peek().isalnum() or self.peek() == '_'):
                    self.advance()
                value = self.src[start:self.pos]
                if value in TYPES:
                    self.add_token("TYPE", value, start_line, start_col)
                elif value in KEYWORDS:
                    self.add_token("KEYWORD", value, start_line, start_col)
                else:
                    self.add_token("IDENT", value, start_line, start_col)


            # Number or Float
            elif c.isdigit():
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

            # String literal
            elif c == '"':
                value = ''
                start_line, start_col = self.line, self.col - 1
                while self.peek() is not None and self.peek() != '"':
                    value += self.advance()
                if self.peek() != '"':
                    self.add_token("ERROR", "Unterminated string literal", start_line, start_col)
                    continue
                self.advance()  # consume closing quote
                self.add_token("STRING", value, start_line, start_col)

            # Char literal
            elif c == "'":
                start_line, start_col = self.line, self.col - 1
                if self.peek() is None:
                    self.add_token("ERROR", "Unterminated char literal", start_line, start_col)
                    continue

                char_val = self.advance()
                if char_val == '\\':
                    escape = self.advance()
                    escape_map = {
                        'n': '\n', 'r': '\r', 't': '\t',
                        "'": "'", '"': '"', '\\': '\\'
                    }
                    if escape not in escape_map:
                        self.add_token("ERROR", f"Unknown escape '\\{escape}'", start_line, start_col)
                        continue
                    char_val = escape_map[escape]

                if self.peek() != "'":
                    self.add_token("ERROR", "Expected closing quote for char literal", start_line, start_col)
                    continue

                self.advance()  # consume closing quote
                self.add_token("CHAR", char_val, start_line, start_col)

            # Line comment
            elif c == '/' and self.peek() == '/':
                while self.peek() is not None and self.peek() != '\n':
                    self.advance()

            # Block comment
            elif c == '/' and self.peek() == '*':
                self.advance()  # consume '*'
                start_line, start_col = self.line, self.col - 2
                while self.peek() is not None:
                    if self.peek() == '*' and self.peek_next() == '/':
                        self.advance()
                        self.advance()
                        break
                    self.advance()
                else:
                    self.add_token("ERROR", "Unterminated block comment", start_line, start_col)

            # Multi-char operator or symbol
            elif c in SYMBOLS:
                start_line, start_col = self.line, self.col - 1
                two_char = c + (self.peek() or '')
                if two_char in MULTI_CHAR_TOKENS:
                    self.advance()
                    self.add_token("OP", two_char, start_line, start_col)
                else:
                    self.add_token("SYMBOL", c, start_line, start_col)

            # Anything else = error
            else:
                self.add_token("ERROR", f"Unexpected character '{c}'", self.line, self.col - 1)

        return self.tokens
