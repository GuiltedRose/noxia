class SemanticAnalyzer:
    def __init__(self, ast):
        self.ast = ast
        self.global_scope = {}
        self.errors = []
        self.scope_stack = []
        self.current_class = None
        self.current_return_type = None

        # ---------------------------
        # Builtins / intrinsics
        # ---------------------------
        # Treat syscall as a builtin function so stdlib can call it freely.
        # Return type "int" is a reasonable default; adjust later if needed.
        self.global_scope["syscall"] = {
            "type": "FunctionDecl",
            "name": "syscall",
            "params": [],          # variadic-ish: don't check arg count/types
            "return_type": "int",
            "builtin": True,
        }

    # ---------------------------
    # Driver
    # ---------------------------

    def analyze(self):
        for node in self.ast:
            self.visit(node)
        return self.errors

    def visit(self, node):
        if not isinstance(node, dict):
            return
        method = getattr(self, f"visit_{node.get('type')}", self.generic_visit)
        return method(node)

    def generic_visit(self, node):
        self.errors.append(f"[SemanticError] Unknown node type: {node.get('type')}")

    # ---------------------------
    # Scopes / symbols
    # ---------------------------

    def enter_scope(self):
        self.scope_stack.append({})

    def exit_scope(self):
        self.scope_stack.pop()

    def declare_symbol(self, name, value):
        if self.scope_stack:
            scope = self.scope_stack[-1]
            if name in scope:
                self.errors.append(f"[SemanticError] Symbol '{name}' already declared in local scope")
            scope[name] = value
        else:
            if name in self.global_scope:
                self.errors.append(f"[SemanticError] Symbol '{name}' already declared globally")
            self.global_scope[name] = value

    def lookup_symbol(self, name):
        for scope in reversed(self.scope_stack):
            if name in scope:
                return scope[name]

        symbol = self.global_scope.get(name)
        if symbol is not None:  # important: don't use `if symbol`
            return symbol

        # allow enum variants to resolve as values
        for enum in self.global_scope.values():
            if isinstance(enum, dict) and enum.get("type") == "EnumDecl":
                if name in enum.get("variants", []):
                    return {"type": "EnumVariant", "enum": enum["name"], "name": name}

        return None

    # ---------------------------
    # Type normalization (parser-aligned)
    # ---------------------------

    def type_to_str(self, t):
        """
        Convert parser type nodes into canonical string forms.
        Parser type nodes:
          - {"type":"TypeName","name":"int"}
          - {"type":"RefType","mutable":bool,"inner": <type-node>}
          - {"type":"PtrType","inner": <type-node>}
          - {"type":"FnType","params":[{"name":..., "type": <type-node>}], "return": <type-node>}
        """
        if t is None:
            return None
        if isinstance(t, str):
            return t
        if not isinstance(t, dict):
            return None

        tt = t.get("type")

        if tt == "TypeName":
            return t.get("name")

        if tt == "RefType":
            inner = self.type_to_str(t.get("inner"))
            if inner is None:
                return None
            return f"&mut {inner}" if t.get("mutable") else f"&{inner}"

        if tt == "PtrType":
            inner = self.type_to_str(t.get("inner"))
            return f"*{inner}" if inner else "*<error>"

        if tt == "FnType":
            ps = []
            for p in t.get("params", []):
                ps.append(self.type_to_str(p.get("type")) or "<error>")
            ret = self.type_to_str(t.get("return")) or "<error>"
            return f"fn({', '.join(ps)}) -> {ret}"

        if tt == "ErrorType":
            return "<error>"

        return None

    def expr_to_name(self, expr):
        """
        Extract a simple identifier name from an expression node.
        For now, only Variable(...) is callable: foo(...)
        """
        if isinstance(expr, str):
            return expr
        if isinstance(expr, dict) and expr.get("type") == "Variable":
            return expr.get("name")
        return None

    def is_zero_literal(self, node):
        return (
            isinstance(node, dict)
            and node.get("type") == "Literal"
            and isinstance(node.get("value"), int)
            and node.get("value") == 0
        )

    def is_stmt_node(self, n):
        return isinstance(n, dict) and n.get("type") in {
            "LetStatement", "ReturnStatement", "IfStatement", "WhileStatement",
            "ForStatement", "ExprStatement", "UnsafeBlock", "UnsafeBlockExpr",
            "FunctionDecl", "ConstDecl", "StaticDecl", "GlobalDecl",
            "TypeAlias", "EnumDecl", "StructDecl", "ClassDecl", "ImportStatement"
        }

    def _promote_numeric_pair(self, a, b):
        """
        Very small numeric tower for now:
          byte < int < float
        Returns: (promoted_a, promoted_b, result_type_for_arith)
        """
        rank = {"byte": 0, "int": 1, "float": 2}
        if a not in rank or b not in rank:
            return None
        r = max(rank[a], rank[b])
        out = "byte" if r == 0 else "int" if r == 1 else "float"
        return out, out, out

    # ---------------------------
    # Type inference (parser-aligned)
    # ---------------------------

    def infer_type(self, node):
        if not node or not isinstance(node, dict):
            return None

        node_type = node.get("type")

        # Guard: never try to infer a "type" for statement/decl nodes
        if node_type in {
            "LetStatement", "ReturnStatement", "IfStatement", "WhileStatement",
            "ForStatement", "ExprStatement", "UnsafeBlock", "UnsafeBlockExpr",
            "FunctionDecl", "ConstDecl", "StaticDecl", "GlobalDecl",
            "TypeAlias", "EnumDecl", "StructDecl", "ClassDecl", "ImportStatement"
        }:
            return None

        if node_type == "Literal":
            val = node.get("value")
            # IMPORTANT: bool is subclass of int in Python
            if isinstance(val, bool):
                return "bool"
            if isinstance(val, int):
                return "int"
            if isinstance(val, float):
                return "float"
            if isinstance(val, str):
                # convenience: 1-char strings behave like byte
                if len(val) == 1:
                    return "byte"
                return "string"
            return None

        if node_type == "NullLiteral":
            # pragmatic early-stage model: null is a ptr
            return "ptr"

        if node_type == "Variable":
            symbol = self.lookup_symbol(node.get("name"))
            if not symbol:
                return None

            # IMPORTANT FIX:
            # Never fall back to `symbol["type"]` (that's the AST node kind, like "LetStatement").
            vt = symbol.get("var_type")
            return self.type_to_str(vt)

        if node_type == "CastExpr":
            return self.type_to_str(node.get("target_type"))

        if node_type == "UnaryExpr":
            return self.infer_type(node.get("expr"))

        if node_type == "BinaryExpr":
            # never type-check against statement nodes
            if self.is_stmt_node(node.get("left")) or self.is_stmt_node(node.get("right")):
                return None

            op = node.get("op")
            ltype = self.infer_type(node.get("left"))
            rtype = self.infer_type(node.get("right"))

            # If either side is unknown, don't emit a mismatch yet
            if ltype is None or rtype is None:
                return None

            # string concatenation convenience
            if op == "+":
                if ltype == "string" and rtype in {"int", "byte"}:
                    return "string"
                if rtype == "string" and ltype in {"int", "byte"}:
                    return "string"

            # numeric promotion (byte/int/float)
            promo = self._promote_numeric_pair(ltype, rtype)
            if promo is not None:
                _, _, arith_out = promo
                if op in {"==", "!=", "<", ">", "<=", ">="}:
                    return "bool"
                if op in {"+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>"}:
                    return arith_out

            if ltype == rtype:
                # comparisons produce bool
                if op in {"==", "!=", "<", ">", "<=", ">="}:
                    return "bool"
                return ltype

            # pointer +/- integer => pointer
            if self.is_pointer(ltype) and rtype in {"int", "byte"} and op in {"+", "-"}:
                return ltype
            if self.is_pointer(rtype) and ltype in {"int", "byte"} and op in {"+", "-"}:
                return rtype

            # pointer comparisons
            if op in {"==", "!=", "<", ">", "<=", ">="}:
                if self.is_pointer(ltype) and self.is_pointer(rtype):
                    return "bool"
                if self.is_pointer(ltype) and rtype in {"int", "byte"}:
                    return "bool"
                if self.is_pointer(rtype) and ltype in {"int", "byte"}:
                    return "bool"

            self.errors.append(f"[TypeError] Mismatched operands: {ltype} and {rtype}")
            return None

        if node_type == "AssignExpr":
            # never type-check statement nodes
            if self.is_stmt_node(node.get("target")) or self.is_stmt_node(node.get("value")):
                return None

            ttype = self.infer_type(node.get("target"))
            vtype = self.infer_type(node.get("value"))

            if ttype and vtype and ttype != vtype:
                # allow ptr = 0
                if self.is_pointer(ttype) and vtype == "int" and self.is_zero_literal(node.get("value")):
                    return ttype
                # allow ptr = ptr (covers null since null infers to ptr)
                if self.is_pointer(ttype) and vtype == "ptr":
                    return ttype
                # allow byte = int (very permissive; tighten later if you want range checks)
                if ttype == "byte" and vtype == "int":
                    return ttype

                self.errors.append(f"[TypeError] Cannot assign {vtype} to {ttype}")
            return ttype

        if node_type == "IndexExpr":
            # TODO: later infer pointer/array indexing result types
            return None

        if node_type == "CallExpr":
            return self._infer_call_expr(node)

        if node_type == "MatchStatement":
            return self.visit_MatchStatement(node)

        return None

    def _infer_call_expr(self, node):
        callee_name = self.expr_to_name(node.get("callee"))
        if not callee_name:
            self.errors.append("[SemanticError] Call target must be a named function (like foo(...))")
            return None

        callee = self.lookup_symbol(callee_name)
        if not callee:
            self.errors.append(f"[SemanticError] Undefined function '{callee_name}'")
            return None

        # Builtins are variadic / special: skip strict checks for now
        if isinstance(callee, dict) and callee.get("builtin"):
            return self.type_to_str(callee.get("return_type"))

        expected = callee.get("params", [])
        args = node.get("args", [])

        if len(expected) != len(args):
            self.errors.append(
                f"[TypeError] Function '{callee_name}' expects {len(expected)} args, got {len(args)}"
            )
        else:
            for e, a in zip(expected, args):
                atype = self.infer_type(a)
                etype = self.type_to_str(e.get("type"))

                # If we don't know one side, don't scream yet.
                if not etype or atype is None:
                    continue

                if etype != atype:
                    # ptr params accept ANY pointer-like arg (*T, &T, ptr)
                    if etype == "ptr" and self.is_pointer(atype):
                        continue
                    # ptr param accepts literal 0 (null pointer)
                    if etype == "ptr" and atype == "int" and self.is_zero_literal(a):
                        continue
                    # allow ptr param to accept string (stdlib convenience)
                    if etype == "ptr" and atype == "string":
                        continue
                    # allow C-string decay: string -> *byte
                    if etype == "*byte" and atype == "string":
                        continue

                    self.errors.append(f"[TypeError] Argument mismatch: expected {etype}, got {atype}")

        return self.type_to_str(callee.get("return_type"))

    # ---------------------------
    # Declaration visitors
    # ---------------------------

    def visit_FunctionDecl(self, node):
        node["return_type"] = self.type_to_str(node.get("return_type"))
        for p in node.get("params", []):
            p["type"] = self.type_to_str(p.get("type"))

        self.declare_symbol(node["name"], node)

        self.enter_scope()
        for param in node.get("params", []):
            self.declare_symbol(param["name"], {"var_type": param.get("type")})

        self.current_return_type = node.get("return_type")
        self.visit_block(node.get("body", []))
        self.current_return_type = None
        self.exit_scope()

    def visit_TypeAlias(self, node):
        node["aliased"] = self.type_to_str(node.get("aliased"))
        self.declare_symbol(node["name"], node)

    def visit_ConstDecl(self, node):
        self.visit(node.get("value"))
        node["var_type"] = self.infer_type(node.get("value"))
        self.declare_symbol(node["name"], node)

    def visit_StaticDecl(self, node):
        node["var_type"] = self.type_to_str(node.get("var_type"))
        value = node.get("value")
        self.visit(value)
        inferred = self.infer_type(value)
        if node["var_type"] and inferred and node["var_type"] != inferred:
            if node["var_type"] == "ptr" and inferred == "int" and self.is_zero_literal(value):
                pass  # allow ptr = 0
            elif self.is_pointer(node["var_type"]) and inferred == "ptr":
                pass  # allow ptr = ptr (covers null)
            else:
                self.errors.append(
                    f"[TypeError] Cannot assign {inferred} to static '{node['name']}' of type {node['var_type']}"
                )
        self.declare_symbol(node["name"], node)

    def visit_GlobalDecl(self, node):
        node["var_type"] = self.type_to_str(node.get("var_type"))
        value = node.get("value")
        self.visit(value)
        inferred = self.infer_type(value)
        if node["var_type"] and inferred and node["var_type"] != inferred:
            if node["var_type"] == "ptr" and inferred == "int" and self.is_zero_literal(value):
                pass  # allow ptr = 0
            elif self.is_pointer(node["var_type"]) and inferred == "ptr":
                pass  # allow ptr = ptr (covers null)
            else:
                self.errors.append(
                    f"[TypeError] Cannot assign {inferred} to global '{node['name']}' of type {node['var_type']}"
                )
        self.declare_symbol(node["name"], node)

    def visit_EnumDecl(self, node):
        name = node["name"]
        if name in self.global_scope:
            self.errors.append(f"[SemanticError] Enum '{name}' already defined")
        self.global_scope[name] = node

        seen = set()
        for variant in node.get("variants", []):
            if variant in seen:
                self.errors.append(f"[SemanticError] Duplicate variant '{variant}' in enum '{name}'")
            seen.add(variant)

    def visit_StructDecl(self, node):
        self.declare_symbol(node["name"], node)
        for f in node.get("fields", []):
            f["type"] = self.type_to_str(f.get("type"))

    def visit_ClassDecl(self, node):
        self.declare_symbol(node["name"], node)

        prev = self.current_class
        self.current_class = node["name"]

        for m in node.get("methods", []):
            self.visit(m)

        self.current_class = prev

    def visit_ImportStatement(self, node):
        if not isinstance(node.get("path"), str):
            self.errors.append("[SemanticError] Invalid import path")

    # ---------------------------
    # Statement visitors
    # ---------------------------

    def visit_block(self, block):
        for stmt in block:
            self.visit(stmt)

    def visit_LetStatement(self, node):
        declared = self.type_to_str(node.get("var_type"))
        node["var_type"] = declared

        value_expr = node.get("value")
        inferred = self.infer_type(value_expr)

        if declared and inferred and declared != inferred:
            if declared == "ptr" and inferred == "int" and self.is_zero_literal(value_expr):
                pass  # allow ptr = 0
            elif self.is_pointer(declared) and inferred == "ptr":
                pass  # allow ptr = ptr (covers null)
            elif declared == "byte" and inferred == "int":
                pass  # permissive for now; tighten later if you want range checks
            else:
                self.errors.append(f"[TypeError] Cannot assign {inferred} to '{node['name']}' of type {declared}")

        if not declared:
            node["var_type"] = inferred

        self.declare_symbol(node["name"], node)
        self.visit(value_expr)

    def visit_ReturnStatement(self, node):
        value = node.get("value")
        if value is None:
            if self.current_return_type and self.current_return_type != "void":
                self.errors.append(
                    f"[TypeError] Return type mismatch: expected {self.current_return_type}, got void"
                )
            return

        self.visit(value)
        ret_type = self.infer_type(value)
        if self.current_return_type and ret_type and ret_type != self.current_return_type:
            self.errors.append(
                f"[TypeError] Return type mismatch: expected {self.current_return_type}, got {ret_type}"
            )

    def visit_ExprStatement(self, node):
        self.visit(node.get("expr"))

    def visit_IfStatement(self, node):
        self.visit(node.get("condition"))

        self.enter_scope()
        self.visit_block(node.get("then", []))
        self.exit_scope()

        else_block = node.get("else")
        if else_block is not None:
            self.enter_scope()
            self.visit_block(else_block)
            self.exit_scope()

    def visit_WhileStatement(self, node):
        self.visit(node.get("condition"))
        self.enter_scope()
        self.visit_block(node.get("body", []))
        self.exit_scope()

    def visit_ForStatement(self, node):
        self.enter_scope()
        self.visit(node.get("init"))
        self.visit(node.get("condition"))
        self.visit(node.get("step"))
        self.visit_block(node.get("body", []))
        self.exit_scope()

    def _arm_result_type(self, body):
        if not body:
            return None
        last = body[-1]
        if not isinstance(last, dict):
            return None

        if last.get("type") == "ExprStatement":
            return self.infer_type(last.get("expr"))

        if last.get("type") in {
            "Literal", "NullLiteral", "Variable", "BinaryExpr", "UnaryExpr",
            "CallExpr", "CastExpr", "AssignExpr", "IndexExpr"
        }:
            return self.infer_type(last)

        return None

    def visit_MatchStatement(self, node):
        self.visit(node.get("expr"))
        result_type = None

        for arm in node.get("arms", []):
            self.enter_scope()

            self.visit(arm.get("pattern"))

            body = arm.get("body", [])
            for stmt in body:
                self.visit(stmt)

            rtype = self._arm_result_type(body)
            if result_type is None:
                result_type = rtype
            elif rtype != result_type:
                self.errors.append(
                    f"[TypeError] Mismatched match arm results: {result_type} vs {rtype}"
                )

            self.exit_scope()

        return result_type

    def visit_UnsafeBlock(self, node):
        self.enter_scope()
        self.visit_block(node.get("body", []))
        self.exit_scope()

    def visit_UnsafeBlockExpr(self, node):
        self.enter_scope()
        self.visit_block(node.get("body", []))
        self.exit_scope()

    # ---------------------------
    # Expression visitors (so visit() doesn't spam Unknown node types)
    # ---------------------------

    def visit_Literal(self, node):
        return

    def visit_NullLiteral(self, node):
        return

    def visit_Variable(self, node):
        name = node.get("name")
        if not name:
            return
        if name == "syscall":  # builtin
            return
        if self.lookup_symbol(name) is None:
            self.errors.append(f"[SemanticError] Undefined symbol '{name}'")

    def visit_UnaryExpr(self, node):
        self.visit(node.get("expr"))
        self.infer_type(node)

    def visit_BinaryExpr(self, node):
        self.visit(node.get("left"))
        self.visit(node.get("right"))
        self.infer_type(node)

    def visit_AssignExpr(self, node):
        self.visit(node.get("target"))
        self.visit(node.get("value"))
        self.infer_type(node)

    def visit_CallExpr(self, node):
        self.visit(node.get("callee"))
        for a in node.get("args", []):
            self.visit(a)
        self.infer_type(node)

    def visit_IndexExpr(self, node):
        self.visit(node.get("target"))
        self.visit(node.get("index"))

    def visit_CastExpr(self, node):
        self.visit(node.get("expr"))
        self.infer_type(node)

    # ---------------------------
    # Utilities
    # ---------------------------

    def is_numeric(self, t):
        return isinstance(t, str) and (t in {"byte", "int", "float"} or t.startswith("int") or t.startswith("uint"))

    def is_pointer(self, t):
        return isinstance(t, str) and (t == "ptr" or t.startswith("&") or "*" in t)
