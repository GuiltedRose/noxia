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

    def parse_enum(self):
        self.advance()
        name_token = self.expect("IDENT", "Expected enum name after 'enum'")
        name = name_token.value if name_token else "<error>"
        self.expect("SYMBOL", "{", "Expected '{' to start enum body")
        variants = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            variant_token = self.expect("IDENT", "Expected enum variant name")
            if variant_token:
                variants.append(variant_token.value)
            self.match("SYMBOL", ",")
        self.expect("SYMBOL", "}", "Expected '}' to close enum body")
        return {"type": "EnumDecl", "name": name, "variants": variants}

    def parse_class(self):
        self.advance()
        name_token = self.expect("IDENT", "Expected class name after 'class'")
        name = name_token.value if name_token else "<error>"
        self.expect("SYMBOL", "{", "Expected '{' to start class body")
        methods = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            methods.append(self.parse_fn())
        self.expect("SYMBOL", "}", "Expected '}' to end class body")
        return {"type": "ClassDecl", "name": name, "methods": methods}

    def parse_struct(self):
        self.advance()
        name_token = self.expect("IDENT", "Expected struct name after 'struct'")
        name = name_token.value if name_token else "<error>"
        self.expect("SYMBOL", "{", "Expected '{' to start struct body")
        fields = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            field_name = self.expect("IDENT", "Expected field name")
            self.expect("SYMBOL", ":", "Expected ':' after field name")
            field_type = self.parse_type()
            fields.append({"name": field_name.value if field_name else "<error>", "type": field_type})
            self.match("SYMBOL", ",")
        self.expect("SYMBOL", "}", "Expected '}' to end struct body")
        return {"type": "StructDecl", "name": name, "fields": fields}

    def parse_block(self):
        block = []
        self.expect("SYMBOL", "{", "Expected '{' to start block")
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            tok = self.peek()
            if tok.kind == "KEYWORD" and tok.value == "let":
                block.append(self.parse_let())
            elif tok.kind == "KEYWORD" and tok.value == "return":
                block.append(self.parse_return())
            elif tok.kind == "KEYWORD" and tok.value == "if":
                block.append(self.parse_if())
            elif tok.kind == "KEYWORD" and tok.value == "match":
                block.append(self.parse_match())
            elif tok.kind == "KEYWORD" and tok.value == "global":
                block.append(self.parse_global())
            else:
                expr = self.parse_expr()
                block.append({"type": "ExprStatement", "expr": expr})
        self.expect("SYMBOL", "}", "Expected '}' at end of block")
        return block

    def parse_fn(self):
        self.advance()
        visibility = "private"
        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value in {"public", "private"}:
            visibility = self.advance().value
        name_token = self.expect("IDENT", "Expected function name")
        name = name_token.value if name_token else "<error>"
        self.expect("SYMBOL", "(", "Expected '(' after function name")
        params = self.parse_params()
        self.expect("SYMBOL", ")", "Expected ')' after parameter list")
        self.expect_arrow()
        return_type = self.parse_type()
        body = self.parse_block()
        return {
            "type": "FunctionDecl",
            "name": name,
            "visibility": visibility,
            "return_type": return_type,
            "body": body,
            "params": params
        }

    def parse_params(self):
        params = []
        if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ')':
            return params
        while True:
            name_token = self.expect("IDENT", "Expected parameter name")
            self.expect("SYMBOL", ":", "Expected ':' after parameter name")
            param_type = self.parse_type()
            params.append({
                "name": name_token.value if name_token else "<error>",
                "type": param_type
            })
            if not self.match("SYMBOL", ","):
                break
        return params

    def parse_type(self):
        token = self.peek()
        if not token:
            self.error("Expected type but got EOF")
            return {"type": "Error"}

        if token.kind == "KEYWORD" and token.value == "&mut":
            self.advance()
            base = self.expect("TYPE", "Expected type after '&mut'")
            return {"type": "RefType", "mutable": True, "inner": base.value if base else "<error>"}
        elif token.kind == "SYMBOL" and token.value == "&":
            self.advance()
            base = self.expect("TYPE", "Expected type after '&'")
            return {"type": "RefType", "mutable": False, "inner": base.value if base else "<error>"}
        elif token.kind == "SYMBOL" and token.value == "*":
            self.advance()
            base = self.expect("TYPE", "Expected type after '*'")
            return {"type": "PtrType", "inner": base.value if base else "<error>"}
        elif token.kind == "TYPE":
            return {"type": "Type", "name": self.advance().value}
        elif token.kind == "IDENT":
            return {"type": "Type", "name": self.advance().value}

        self.error(f"Unexpected token in type: {token}")
        return {"type": "Error"}

    def parse_let(self):
        self.expect("KEYWORD", None, "Expected 'let'")
        name_token = self.expect("IDENT", "Expected variable name")
        name = name_token.value if name_token else "<error>"
        if self.match("SYMBOL", ":"):
            var_type = self.parse_type()
        else:
            var_type = None
        self.expect("SYMBOL", "=", "Expected '=' in let statement")
        expr = self.parse_expr()
        return {"type": "LetStatement", "name": name, "var_type": var_type, "value": expr}

    def parse_global(self):
        self.advance()
        name_token = self.expect("IDENT", "Expected variable name after 'global'")
        self.expect("SYMBOL", ":", "Expected ':' after global name")
        type_node = self.parse_type()
        self.expect("SYMBOL", "=", "Expected '=' after type")
        value = self.parse_expr()
        return {
            "type": "GlobalDecl",
            "name": name_token.value if name_token else "<error>",
            "var_type": type_node,
            "value": value
        }

    def parse_if(self):
        self.advance()
        condition = self.parse_expr()
        self.expect("SYMBOL", "{", "Expected '{' after 'if' condition")
        then_block = self.parse_block()
        else_block = None
        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value == "else":
            self.advance()
            self.expect("SYMBOL", "{", "Expected '{' after 'else'")
            else_block = self.parse_block()
        return {"type": "IfStatement", "condition": condition, "then": then_block, "else": else_block}

    def parse_return(self):
        self.advance()
        return {"type": "ReturnStatement", "value": self.parse_expr()}

    def parse_match(self):
        self.advance()
        expr = self.parse_expr()
        self.expect("SYMBOL", "{", "Expected '{' after match expression")
        arms = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            pattern = self.parse_expr()
            self.expect("SYMBOL", "=>", "Expected '=>' after match pattern")
            body = self.parse_block() if self.peek().value == '{' else [self.parse_expr()]
            arms.append({"pattern": pattern, "body": body})
        self.expect("SYMBOL", "}", "Expected '}' to close match block")
        return {"type": "MatchStatement", "expr": expr, "arms": arms}

    def parse_unsafe_block(self):
        self.advance()
        self.expect("SYMBOL", "{", "Expected '{' after '!safe'")
        return {"type": "UnsafeBlock", "body": self.parse_block()}

    def parse_expr(self, precedence=0):
        token = self.peek()
        if not token:
            self.error("Unexpected EOF in expression")
            return {"type": "Error"}

        if token.kind == "KEYWORD" and token.value == "cast":
            self.advance()
            self.expect("SYMBOL", "(", "Expected '(' after 'cast'")
            expr = self.parse_expr()
            self.expect("KEYWORD", "as", "Expected 'as' in cast expression")
            target_type = self.parse_type()
            self.expect("SYMBOL", ")", "Expected ')' to close cast expression")
            return {"type": "CastExpr", "expr": expr, "target_type": target_type}
        elif token.kind == "KEYWORD" and token.value == "!safe":
            self.advance()
            return {"type": "UnsafeBlockExpr", "body": self.parse_block()}
        elif token.kind in {"INT", "FLOAT", "STRING", "CHAR"}:
            self.advance()
            return {"type": "Literal", "value": token.value}
        elif token.kind == "IDENT":
            self.advance()
            return {"type": "Variable", "name": token.value}
        elif token.kind == "SYMBOL" and token.value == '(':
            self.advance()
            expr = self.parse_expr()
            self.expect("SYMBOL", ")", "Expected ')' after expression")
            return expr
        else:
            self.error(f"Unexpected token in expression: {token}")
            self.advance()
            return {"type": "Error"}

    def parse_import(self):
        self.advance()
        path_token = self.expect("STRING", "Expected module path as string in import")
        return {"type": "ImportStatement", "path": path_token.value if path_token else "<error>"}

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
