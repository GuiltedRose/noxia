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

    def expect(self, kind, error_message):
        if self.peek() and self.peek().kind == kind:
            return self.advance()
        self.error(error_message)
        return None
    
    def parse_enum(self):
        self.advance()  # consume 'enum'
        name_token = self.expect("IDENT", "Expected enum name after 'enum'")
        name = name_token.value if name_token else "<error>"

        self.expect("SYMBOL", "Expected '{' to start enum body")
        variants = []

        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            variant_token = self.expect("IDENT", "Expected enum variant name")
            if variant_token:
                variants.append(variant_token.value)

            # Optional comma
            if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ",":
                self.advance()

        self.expect("SYMBOL", "Expected '}' to close enum body")

        return {
            "type": "EnumDecl",
            "name": name,
            "variants": variants
        }

    def error(self, message):
        line = self.current.line if self.current else '?'
        col = self.current.column if self.current else '?'
        print(f"[ParseError] Line {line}, Col {col}: {message}")

    def parse(self):
        ast = []
        while self.peek():
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
        return ast

    def parse_block(self):
        block = []
        self.expect("SYMBOL", "Expected '{' to start block")
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            tok = self.peek()
            if tok.kind == "KEYWORD" and tok.value == "let":
                stmt = self.parse_let()
                block.append(stmt)
            elif tok.kind == "KEYWORD" and tok.value == "return":
                stmt = self.parse_return()
                block.append(stmt)
            elif tok.kind == "KEYWORD" and tok.value == "if":
                stmt = self.parse_if()
                block.append(stmt)
            elif tok.kind == "KEYWORD" and tok.value == "match":
                stmt = self.parse_match()
                block.append(stmt)
            else:
                expr = self.parse_expr()
                block.append({"type": "ExprStatement", "expr": expr})
        self.expect("SYMBOL", "Expected '}' at end of block")
        return block

    def parse_match(self):
        self.advance()  # consume 'match'
        expr = self.parse_expr()
        self.expect("SYMBOL", "Expected '{' after match expression")
        arms = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            pattern = self.parse_expr()
            self.expect("SYMBOL", "Expected '=>' after match pattern")
            if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == '{':
                body = self.parse_block()
            else:
                body = [self.parse_expr()]
            arms.append({"pattern": pattern, "body": body})
        self.expect("SYMBOL", "Expected '}' to close match block")
        return {"type": "MatchStatement", "expr": expr, "arms": arms}

    def parse_return(self):
        self.advance()  # consume 'return'
        expr = self.parse_expr()
        return {"type": "ReturnStatement", "value": expr}

    def parse_if(self):
        self.advance()  # consume 'if'
        condition = self.parse_expr()
        self.expect("SYMBOL", "Expected '{' after 'if' condition")
        then_block = self.parse_block()
        else_block = None
        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value == "else":
            self.advance()
            self.expect("SYMBOL", "Expected '{' after 'else'")
            else_block = self.parse_block()
        return {
            "type": "IfStatement",
            "condition": condition,
            "then": then_block,
            "else": else_block
        }

    def parse_unsafe_block(self):
        self.advance()  # consume '!safe'
        self.expect("SYMBOL", "Expected '{' after '!safe'")
        body = self.parse_block()
        return {
            "type": "UnsafeBlock",
            "body": body
        }

    def parse_params(self):
        params = []
        if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ')':
            return params
        while True:
            name_token = self.expect("IDENT", "Expected parameter name")
            name = name_token.value if name_token else "<error>"
            self.expect("SYMBOL", "Expected ':' after parameter name")
            type_token = self.expect("TYPE", "Expected type after ':'")
            param_type = type_token.value if type_token else "<error>"
            params.append({"name": name, "type": param_type})
            if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ',':
                self.advance()
                continue
            break
        return params

    def parse_expr(self, precedence=0):
        token = self.peek()
        if token.kind in {"INT", "FLOAT", "STRING", "CHAR"}:
            self.advance()
            left = {"type": "Literal", "value": token.value}
        elif token.kind == "IDENT":
            self.advance()
            left = {"type": "Variable", "name": token.value}
        elif token.kind == "SYMBOL" and token.value == '(':
            self.advance()
            left = self.parse_expr()
            self.expect("SYMBOL", "Expected ')' after expression")
        else:
            self.error(f"Unexpected token in expression: {token}")
            self.advance()
            return {"type": "Error"}
        while self.peek() and self.get_precedence(self.peek()) > precedence:
            op_token = self.advance()
            right = self.parse_expr(self.get_precedence(op_token))
            left = {"type": "BinaryExpr", "op": op_token.value, "left": left, "right": right}
        return left
    
    def parse_import(self):
        self.advance()  # consume 'import'
        path_token = self.expect("STRING", "Expected module path as string in import")
        return {
            "type": "ImportStatement",
            "path": path_token.value
        }

    def get_precedence(self, token):
        if token.kind not in {"OP", "SYMBOL"}:
            return 0
        precedences = {
            '||': 1, '&&': 2,
            '==': 3, '!=': 3,
            '<': 4, '<=': 4, '>': 4, '>=': 4,
            '+': 5, '-': 5,
            '*': 6, '/': 6, '%': 6
        }
        return precedences.get(token.value, 0)

    def parse_fn(self):
        self.advance()  # consume 'fn'
        visibility = "private"
        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value in {"public", "private"}:
            visibility = self.advance().value
        name_token = self.expect("IDENT", "Expected function name")
        name = name_token.value if name_token else "<error>"
        self.expect("SYMBOL", "Expected '(' after function name")
        params = self.parse_params()
        self.expect("SYMBOL", "Expected ')' after parameter list")
        self.expect("SYMBOL", "Expected '->'")
        return_type = self.expect("TYPE", "Expected return type after '->'")
        return_type_val = return_type.value if return_type else "<error>"
        body = self.parse_block()
        return {
            "type": "FunctionDecl",
            "name": name,
            "visibility": visibility,
            "return_type": return_type_val,
            "body": body,
            "params": params
        }
