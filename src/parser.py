from lexer import Token

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.current = self.tokens[self.pos] if self.tokens else None

    def peek(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def advance(self):
        self.pos += 1
        self.current = self.peek()
        return self.current

    def match(self, *kinds):
        if self.peek() and self.peek().kind in kinds:
            return self.advance()
        return None

    def expect(self, kind, value=None, error_message="Unexpected token"):
        tok = self.peek()
        if tok and tok.kind == kind and (value is None or tok.value == value):
            return self.advance()
        self.error(error_message)
        return None

    def expect_arrow(self):
        token = self.expect("OP", "->", "Expected '->'")
        if token is None or token.value != "->":
            self.error("Expected '->'")
        return token

    def error(self, message):
        line = self.current.line if self.current else '?'
        col = self.current.column if self.current else '?'
        print(f"[ParseError] Line {line}, Col {col}: {message}")

    def parse(self):
        ast = []
        while self.peek() and self.peek().kind != "EOF":
            tok = self.peek()
            if tok.kind == "KEYWORD" and tok.value == "import":
                ast.append(self.parse_import())
            elif tok.kind == "KEYWORD" and tok.value == "fn":
                ast.append(self.parse_fn())
            elif tok.kind == "KEYWORD" and tok.value == "let":
                ast.append(self.parse_let())
            elif tok.kind == "KEYWORD" and tok.value == "class":
                ast.append(self.parse_class())
            elif tok.kind == "KEYWORD" and tok.value == "struct":
                ast.append(self.parse_struct())
            elif tok.kind == "KEYWORD" and tok.value == "enum":
                ast.append(self.parse_enum())
            elif tok.kind == "KEYWORD" and tok.value == "!safe":
                ast.append(self.parse_unsafe_block())
            else:
                self.error(f"Unexpected token '{tok.value}'")
                self.advance()

        # EOF enforcement I forgot this and it might fix the issue.
        if self.peek() and self.peek().kind != "EOF":
            self.error("Expected end of file")

        return ast
