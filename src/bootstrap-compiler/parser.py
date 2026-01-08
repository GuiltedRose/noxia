# parser.py (clean master) — bitwise + shift support (bootstrap-friendly)
# Adds:
#   - unary ~
#   - bitwise &: |
#   - shift: << >>
#
# Intentionally DOES NOT add '^' (XOR) because you wanted to pick 1.
# Keeps existing grammar + AST shapes intact.

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.current = self.tokens[self.pos] if self.tokens else None
        self.errors = []

    # ---------- helpers ----------

    def had_errors(self) -> bool:
        return len(self.errors) > 0

    def peek(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def advance(self):
        tok = self.peek()
        self.pos += 1
        self.current = self.peek()
        return tok

    def error(self, message):
        tok = self.peek() or self.current
        line = getattr(tok, "line", "?") if tok else "?"
        col = getattr(tok, "column", "?") if tok else "?"
        msg = f"[ParseError] Line {line}, Col {col}: {message}"
        print(msg)
        self.errors.append(msg)

    def expect(self, kind, msg="Unexpected token", value=None):
        tok = self.peek()
        if tok and tok.kind == kind and (value is None or tok.value == value):
            return self.advance()
        self.error(msg)
        return None

    def match(self, kind, value=None):
        tok = self.peek()
        if tok and tok.kind == kind and (value is None or tok.value == value):
            return self.advance()
        return None

    def expect_arrow(self):
        tok = self.peek()
        if tok and tok.kind == "OP" and tok.value == "->":
            return self.advance()
        self.error("Expected '->'")
        return None

    # ---------- program ----------

    def parse(self):
        ast = []
        while self.peek() and self.peek().kind != "EOF":
            tok = self.peek()

            if tok.kind == "KEYWORD" and tok.value == "import":
                ast.append(self.parse_import())
            elif tok.kind == "KEYWORD" and tok.value == "fn":
                ast.append(self.parse_fn())
            elif tok.kind == "KEYWORD" and tok.value == "type":
                ast.append(self.parse_type_alias())
            elif tok.kind == "KEYWORD" and tok.value == "const":
                ast.append(self.parse_const())
            elif tok.kind == "KEYWORD" and tok.value == "global":
                ast.append(self.parse_global())
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

    # ---------- declarations ----------

    def parse_import(self):
        self.advance()  # import
        path = self.expect("STRING", "Expected module path as string in import")
        return {"type": "ImportStatement", "path": path.value if path else "<error>"}

    def parse_type_alias(self):
        self.advance()  # type
        name = self.expect("IDENT", "Expected type alias name")
        self.expect("SYMBOL", "Expected '=' in type alias", value="=")
        aliased = self.parse_type()
        return {"type": "TypeAlias", "name": name.value if name else "<error>", "aliased": aliased}

    def parse_const(self):
        self.advance()  # const
        name = self.expect("IDENT", "Expected const name")
        self.expect("SYMBOL", "Expected '=' after const name", value="=")
        value = self.parse_expr()
        return {"type": "ConstDecl", "name": name.value if name else "<error>", "value": value}

    def parse_enum(self):
        self.advance()  # enum
        name = self.expect("IDENT", "Expected enum name after 'enum'")
        self.expect("SYMBOL", "Expected '{' to start enum body", value="{")
        variants = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            v = self.expect("IDENT", "Expected enum variant name")
            if v:
                variants.append(v.value)
            self.match("SYMBOL", ",")
        self.expect("SYMBOL", "Expected '}' to close enum body", value="}")
        return {"type": "EnumDecl", "name": name.value if name else "<error>", "variants": variants}

    def parse_class(self):
        self.advance()  # class
        name = self.expect("IDENT", "Expected class name after 'class'")
        self.expect("SYMBOL", "Expected '{' to start class body", value="{")
        methods = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            methods.append(self.parse_fn())
        self.expect("SYMBOL", "Expected '}' to end class body", value="}")
        return {"type": "ClassDecl", "name": name.value if name else "<error>", "methods": methods}

    def parse_struct(self):
        self.advance()  # struct
        name = self.expect("IDENT", "Expected struct name after 'struct'")
        self.expect("SYMBOL", "Expected '{' to start struct body", value="{")
        fields = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            fname = self.expect("IDENT", "Expected field name")
            self.expect("SYMBOL", "Expected ':' after field name", value=":")
            ftype = self.parse_type()
            fields.append({"name": fname.value if fname else "<error>", "type": ftype})
            self.match("SYMBOL", ",")
        self.expect("SYMBOL", "Expected '}' to end struct body", value="}")
        return {"type": "StructDecl", "name": name.value if name else "<error>", "fields": fields}

    def parse_fn(self):
        self.advance()  # fn

        visibility = "private"
        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value in {"public", "private"}:
            visibility = self.advance().value

        name = self.expect("IDENT", "Expected function name")

        self.expect("SYMBOL", "Expected '(' after function name", value="(")
        params = self.parse_params()
        self.expect("SYMBOL", "Expected ')' after parameter list", value=")")

        self.expect_arrow()
        ret = self.parse_type()

        body = self.parse_block()
        return {
            "type": "FunctionDecl",
            "name": name.value if name else "<error>",
            "visibility": visibility,
            "params": params,
            "return_type": ret,
            "body": body,
        }

    def parse_params(self):
        params = []
        if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ")":
            return params

        while True:
            pname = self.expect("IDENT", "Expected parameter name")
            self.expect("SYMBOL", "Expected ':' after parameter name", value=":")
            ptype = self.parse_type()
            params.append({"name": pname.value if pname else "<error>", "type": ptype})
            if not self.match("SYMBOL", ","):
                break
        return params

    # ---------- types ----------

    def parse_type(self):
        tok = self.peek()
        if not tok:
            self.error("Expected type but got EOF")
            return {"type": "ErrorType"}

        # &mut T
        if tok.kind == "KEYWORD" and tok.value == "&mut":
            self.advance()
            inner = self.parse_type()
            return {"type": "RefType", "mutable": True, "inner": inner}

        # &T
        if tok.kind == "SYMBOL" and tok.value == "&":
            self.advance()
            inner = self.parse_type()
            return {"type": "RefType", "mutable": False, "inner": inner}

        # *T
        if tok.kind == "SYMBOL" and tok.value == "*":
            self.advance()
            inner = self.parse_type()
            return {"type": "PtrType", "inner": inner}

        # fn(...) -> Ret
        if tok.kind == "KEYWORD" and tok.value == "fn":
            self.advance()
            self.expect("SYMBOL", "Expected '(' after fn in type", value="(")
            tparams = []
            if not (self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ")"):
                while True:
                    if self.peek() and self.peek().kind in {"IDENT", "KEYWORD"}:
                        name_tok = self.advance()
                        if self.match("SYMBOL", ":"):
                            ptype = self.parse_type()
                            tparams.append({"name": name_tok.value, "type": ptype})
                        else:
                            tparams.append({"name": None, "type": {"type": "TypeName", "name": name_tok.value}})
                    else:
                        ptype = self.parse_type()
                        tparams.append({"name": None, "type": ptype})

                    if not self.match("SYMBOL", ","):
                        break
            self.expect("SYMBOL", "Expected ')' after fn type params", value=")")
            self.expect_arrow()
            ret = self.parse_type()
            return {"type": "FnType", "params": tparams, "return": ret}

        # Named types:
        if tok.kind in {"IDENT", "KEYWORD"}:
            if tok.kind == "KEYWORD" and tok.value in {
                "fn", "let", "return", "if", "else", "while", "for", "break", "continue",
                "match", "import", "type", "const", "global", "static", "struct", "enum",
                "class", "!safe", "cast", "as", "public", "private"
            }:
                self.error(f"Unexpected keyword in type: {tok.value}")
                self.advance()
                return {"type": "ErrorType"}

            self.advance()
            return {"type": "TypeName", "name": tok.value}

        self.error(f"Unexpected token in type: {tok}")
        self.advance()
        return {"type": "ErrorType"}

    # ---------- blocks / statements ----------

    def parse_block(self):
        stmts = []
        self.expect("SYMBOL", "Expected '{' to start block", value="{")
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            tok = self.peek()

            if tok.kind == "KEYWORD" and tok.value == "let":
                stmts.append(self.parse_let())
            elif tok.kind == "KEYWORD" and tok.value == "return":
                stmts.append(self.parse_return())
            elif tok.kind == "KEYWORD" and tok.value == "if":
                stmts.append(self.parse_if())
            elif tok.kind == "KEYWORD" and tok.value == "while":
                stmts.append(self.parse_while())
            elif tok.kind == "KEYWORD" and tok.value == "for":
                stmts.append(self.parse_for())
            elif tok.kind == "KEYWORD" and tok.value == "break":
                self.advance()
                stmts.append({"type": "BreakStatement"})
            elif tok.kind == "KEYWORD" and tok.value == "continue":
                self.advance()
                stmts.append({"type": "ContinueStatement"})
            elif tok.kind == "KEYWORD" and tok.value == "const":
                stmts.append(self.parse_const())
            elif tok.kind == "KEYWORD" and tok.value == "static":
                stmts.append(self.parse_static())
            elif tok.kind == "KEYWORD" and tok.value == "global":
                stmts.append(self.parse_global())
            elif tok.kind == "KEYWORD" and tok.value == "match":
                stmts.append(self.parse_match())
            else:
                expr = self.parse_expr()
                stmts.append({"type": "ExprStatement", "expr": expr})

            # Optional separator; you currently don't lex ';' anyway, so this is harmless.
            self.match("SYMBOL", ";")

        self.expect("SYMBOL", "Expected '}' at end of block", value="}")
        return stmts

    def parse_let(self):
        self.expect("KEYWORD", "Expected 'let'", value="let")
        name = self.expect("IDENT", "Expected variable name")

        vtype = None
        if self.match("SYMBOL", ":"):
            vtype = self.parse_type()

        value = None
        if self.match("SYMBOL", "="):
            value = self.parse_expr()

        return {
            "type": "LetStatement",
            "name": name.value if name else "<error>",
            "var_type": vtype,
            "value": value,
        }

    def parse_static(self):
        self.advance()  # static
        name = self.expect("IDENT", "Expected static name")
        self.expect("SYMBOL", "Expected ':' after static name", value=":")
        vtype = self.parse_type()
        self.expect("SYMBOL", "Expected '=' after static type", value="=")
        value = self.parse_expr()
        return {"type": "StaticDecl", "name": name.value if name else "<error>", "var_type": vtype, "value": value}

    def parse_global(self):
        self.advance()  # global
        name = self.expect("IDENT", "Expected variable name after 'global'")
        self.expect("SYMBOL", "Expected ':' after global name", value=":")
        vtype = self.parse_type()
        self.expect("SYMBOL", "Expected '=' after type", value="=")
        value = self.parse_expr()
        return {"type": "GlobalDecl", "name": name.value if name else "<error>", "var_type": vtype, "value": value}

    def parse_if(self):
        self.advance()  # if
        cond = self.parse_expr()
        then_block = self.parse_block()
        else_block = None
        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value == "else":
            self.advance()
            else_block = self.parse_block()
        return {"type": "IfStatement", "condition": cond, "then": then_block, "else": else_block}

    def parse_while(self):
        self.advance()  # while
        cond = self.parse_expr()
        body = self.parse_block()
        return {"type": "WhileStatement", "condition": cond, "body": body}

    def parse_for(self):
        self.advance()  # for

        if self.peek() and self.peek().kind == "KEYWORD" and self.peek().value == "let":
            init = self.parse_let()
        else:
            init = self.parse_expr()

        self.expect("SYMBOL", "Expected ',' after for init", value=",")

        cond = self.parse_expr()
        self.expect("SYMBOL", "Expected ',' after for condition", value=",")

        step = self.parse_expr()
        body = self.parse_block()
        return {"type": "ForStatement", "init": init, "condition": cond, "step": step, "body": body}

    def parse_return(self):
        self.advance()  # return
        if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == "}":
            return {"type": "ReturnStatement", "value": None}
        return {"type": "ReturnStatement", "value": self.parse_expr()}

    def parse_match(self):
        self.advance()  # match
        expr = self.parse_expr()
        self.expect("SYMBOL", "Expected '{' after match expression", value="{")

        arms = []
        while self.peek() and not (self.peek().kind == "SYMBOL" and self.peek().value == "}"):
            pat = self.parse_expr()
            if not (self.peek() and self.peek().kind == "OP" and self.peek().value == "=>"):
                self.error("Expected '=>' after match pattern")
            else:
                self.advance()

            if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == "{":
                body = self.parse_block()
            else:
                body = [self.parse_expr()]

            arms.append({"pattern": pat, "body": body})
            self.match("SYMBOL", ",")
        self.expect("SYMBOL", "Expected '}' to close match block", value="}")
        return {"type": "MatchStatement", "expr": expr, "arms": arms}

    def parse_unsafe_block(self):
        self.advance()  # !safe
        return {"type": "UnsafeBlock", "body": self.parse_block()}

    # ---------- expressions ----------
    #
    # Precedence (tight -> loose):
    #   postfix
    #   unary      (- ! & * ~)
    #   mul        (* / %)
    #   add        (+ -)
    #   shift      (<< >>)
    #   relational (< > <= >=)
    #   bit_and    (&)          <-- NOTE: tighter than equality (better ergonomics than C)
    #   bit_or     (|)
    #   equality   (== !=)
    #   logic_and  (&&)
    #   logic_or   (||)
    #   assign     (=)

    def parse_expr(self):
        return self.parse_assignment()

    def parse_assignment(self):
        left = self.parse_logic_or()
        if self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == "=":
            self.advance()
            right = self.parse_assignment()
            return {"type": "AssignExpr", "target": left, "value": right}
        return left

    def parse_logic_or(self):
        node = self.parse_logic_and()
        while self.peek() and self.peek().kind == "OP" and self.peek().value == "||":
            op = self.advance().value
            rhs = self.parse_logic_and()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_logic_and(self):
        node = self.parse_equality()
        while self.peek() and self.peek().kind == "OP" and self.peek().value == "&&":
            op = self.advance().value
            rhs = self.parse_equality()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_equality(self):
        node = self.parse_bit_or()
        while self.peek() and self.peek().kind == "OP" and self.peek().value in {"==", "!="}:
            op = self.advance().value
            rhs = self.parse_bit_or()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_bit_or(self):
        node = self.parse_bit_and()
        while self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == "|":
            op = self.advance().value
            rhs = self.parse_bit_and()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_bit_and(self):
        node = self.parse_relational()
        while self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == "&":
            op = self.advance().value
            rhs = self.parse_relational()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_relational(self):
        node = self.parse_shift()
        while True:
            tok = self.peek()
            if tok and (
                (tok.kind == "SYMBOL" and tok.value in {"<", ">"})
                or (tok.kind == "OP" and tok.value in {"<=", ">="})
            ):
                op = self.advance().value
                rhs = self.parse_shift()
                node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
            else:
                break
        return node

    def parse_shift(self):
        node = self.parse_add()
        while True:
            tok = self.peek()
            if tok and tok.kind == "OP" and tok.value in {"<<", ">>"}:
                op = self.advance().value
                rhs = self.parse_add()
                node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
            else:
                break
        return node

    def parse_add(self):
        node = self.parse_mul()
        while self.peek() and self.peek().kind == "SYMBOL" and self.peek().value in {"+", "-"}:
            op = self.advance().value
            rhs = self.parse_mul()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_mul(self):
        node = self.parse_unary()
        while self.peek() and self.peek().kind == "SYMBOL" and self.peek().value in {"*", "/", "%"}:
            op = self.advance().value
            rhs = self.parse_unary()
            node = {"type": "BinaryExpr", "op": op, "left": node, "right": rhs}
        return node

    def parse_unary(self):
        tok = self.peek()
        if tok and tok.kind == "SYMBOL" and tok.value in {"-", "!", "&", "*", "~"}:
            op = self.advance().value
            expr = self.parse_unary()
            return {"type": "UnaryExpr", "op": op, "expr": expr}
        return self.parse_postfix()

    def parse_postfix(self):
        node = self.parse_primary()

        while True:
            tok = self.peek()

            if tok and tok.kind == "SYMBOL" and tok.value == "(":
                self.advance()
                args = []
                if not (self.peek() and self.peek().kind == "SYMBOL" and self.peek().value == ")"):
                    while True:
                        args.append(self.parse_expr())
                        if not self.match("SYMBOL", ","):
                            break
                self.expect("SYMBOL", "Expected ')' after arguments", value=")")
                node = {"type": "CallExpr", "callee": node, "args": args}
                continue

            if tok and tok.kind == "SYMBOL" and tok.value == "[":
                self.advance()
                idx = self.parse_expr()
                self.expect("SYMBOL", "Expected ']' after index", value="]")
                node = {"type": "IndexExpr", "target": node, "index": idx}
                continue

            if tok and tok.kind == "SYMBOL" and tok.value == ".":
                self.advance()
                field = self.expect("IDENT", "Expected field name after '.'")
                node = {
                    "type": "MemberExpr",
                    "object": node,
                    "member": field.value if field else "<error>",
                }
                continue

            break

        return node

    def parse_primary(self):
        tok = self.peek()
        if not tok:
            self.error("Unexpected EOF in expression")
            return {"type": "Error"}

        # cast(expr as T) ONLY
        if tok.kind == "KEYWORD" and tok.value == "cast":
            self.advance()
            self.expect("SYMBOL", "Expected '(' after 'cast'", value="(")
            expr = self.parse_expr()
            self.expect("KEYWORD", "Expected 'as' in cast expression", value="as")
            t = self.parse_type()
            self.expect("SYMBOL", "Expected ')' to close cast expression", value=")")
            return {"type": "CastExpr", "expr": expr, "target_type": t}

        if tok.kind == "KEYWORD" and tok.value == "!safe":
            self.advance()
            return {"type": "UnsafeBlockExpr", "body": self.parse_block()}

        if tok.kind == "KEYWORD" and tok.value in {"true", "false"}:
            self.advance()
            return {"type": "Literal", "value": (tok.value == "true")}

        if (tok.kind == "IDENT" and tok.value == "null") or (tok.kind == "KEYWORD" and tok.value == "null"):
            self.advance()
            return {"type": "NullLiteral"}

        if tok.kind in {"INT", "FLOAT", "STRING", "CHAR"}:
            self.advance()
            return {"type": "Literal", "value": tok.value}

        if tok.kind == "IDENT":
            self.advance()
            return {"type": "Variable", "name": tok.value}

        if tok.kind == "SYMBOL" and tok.value == "(":
            self.advance()
            expr = self.parse_expr()
            self.expect("SYMBOL", "Expected ')' after expression", value=")")
            return expr

        self.error(f"Unexpected token in expression: {tok}")
        self.advance()
        return {"type": "Error"}
