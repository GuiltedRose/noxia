# semantic.py (master) — FULL COPY/PASTE
#
# Int-family upgrade:
# - Accepts BOTH naming styles: u8/i8/u16/... AND uint8/int8/uint16/...
# - Canonicalizes to uint*/int* internally so types compare cleanly
# - Allows implicit narrowing ONLY for int LITERALS that fit the destination bit width
# - Fixes string -> *uint8 bootstrap (since byte canonicalizes to uint8)
# - Keeps pointer/bootstrap rules intact
#
# BOOTSTRAP ADDITION:
# - Allow assigning int -> smaller int-family (uint8, uint16, etc.) even when RHS isn't a literal.
#   This is to unblock early stdlib/boot code where math produces 'int' everywhere.
#   Tighten later once you have better typing + diagnostics.

class SemanticAnalyzer:
    def __init__(self, ast):
        self.ast = ast

        # ---------------------------
        # Separate namespaces
        # ---------------------------
        self.global_scope = {}   # values/functions
        self.type_scope = {}     # TypeAlias/Struct/Enum/Class

        self.errors = []
        self.scope_stack = []
        self.current_class = None
        self.current_return_type = None

        # Track legality of 'break'
        self.loop_depth = 0

        # Native machine int width (amd64 bootstrap)
        self.native_int_bits = 64

        # ---------------------------
        # Builtins / intrinsics
        # ---------------------------
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
        """
        Split namespaces:
          - TypeAlias / StructDecl / EnumDecl / ClassDecl => type_scope
          - Everything else => value scopes
        """
        is_type_decl = isinstance(value, dict) and value.get("type") in {
            "TypeAlias", "StructDecl", "EnumDecl", "ClassDecl"
        }

        if is_type_decl:
            if value.get("type") == "TypeAlias":
                value["aliased"] = (
                    value.get("aliased")
                    if isinstance(value.get("aliased"), str)
                    else self.type_to_str(value.get("aliased"))
                )

            if name in self.type_scope:
                prev = self.type_scope[name]
                if prev.get("type") == value.get("type"):
                    if value.get("type") == "TypeAlias" and prev.get("aliased") == value.get("aliased"):
                        return
                    if value.get("type") == "StructDecl" and prev.get("fields", []) == value.get("fields", []):
                        return
                    if value.get("type") == "EnumDecl" and prev.get("variants", []) == value.get("variants", []):
                        return
                    if value.get("type") == "ClassDecl" and prev.get("methods", []) == value.get("methods", []):
                        return

                self.errors.append(f"[SemanticError] Type '{name}' already declared")
                return

            self.type_scope[name] = value
            return

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
        if symbol is not None:
            return symbol

        for enum in self.type_scope.values():
            if isinstance(enum, dict) and enum.get("type") == "EnumDecl":
                if name in enum.get("variants", []):
                    return {"type": "EnumVariant", "enum": enum["name"], "name": name}

        return None

    def lookup_type(self, name):
        return self.type_scope.get(name)

    # ---------------------------
    # Canonical type names
    # ---------------------------

    def canonical_type(self, t):
        """
        Normalize spelling so comparisons don't explode.
        Canonicalize to lexer-style names where possible:
          u8  -> uint8
          i8  -> int8
          ...
          byte -> uint8  (internal canonical)
        Works through pointer/ref prefixes too: *u8 -> *uint8, &u16 -> &uint16, etc.
        """
        if not isinstance(t, str) or t == "":
            return t

        if t.startswith("&mut "):
            inner = self.canonical_type(t[len("&mut "):])
            return f"&mut {inner}"
        if t.startswith("&"):
            inner = self.canonical_type(t[1:])
            return f"&{inner}"
        if t.startswith("*"):
            inner = self.canonical_type(t[1:])
            return f"*{inner}"

        alias = {
            "byte": "uint8",
            "u8": "uint8",
            "i8": "int8",
            "u16": "uint16",
            "i16": "int16",
            "u32": "uint32",
            "i32": "int32",
            "u64": "uint64",
            "i64": "int64",
        }
        return alias.get(t, t)

    # ---------------------------
    # Type normalization (parser-aligned)
    # ---------------------------

    def type_to_str(self, t):
        if t is None:
            return None
        if isinstance(t, str):
            return self.canonical_type(t)
        if not isinstance(t, dict):
            return None

        tt = t.get("type")

        if tt == "TypeName":
            return self.canonical_type(t.get("name"))

        if tt == "RefType":
            inner = self.type_to_str(t.get("inner"))
            if inner is None:
                return None
            out = f"&mut {inner}" if t.get("mutable") else f"&{inner}"
            return self.canonical_type(out)

        if tt == "PtrType":
            inner = self.type_to_str(t.get("inner"))
            out = f"*{inner}" if inner else "*<error>"
            return self.canonical_type(out)

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

    def is_int_literal(self, node):
        return (
            isinstance(node, dict)
            and node.get("type") == "Literal"
            and isinstance(node.get("value"), int)
        )

    def is_stmt_node(self, n):
        return isinstance(n, dict) and n.get("type") in {
            "LetStatement", "ReturnStatement", "IfStatement", "WhileStatement",
            "ForStatement", "BreakStatement",
            "ExprStatement", "UnsafeBlock", "UnsafeBlockExpr",
            "FunctionDecl", "ConstDecl", "StaticDecl", "GlobalDecl",
            "TypeAlias", "EnumDecl", "StructDecl", "ClassDecl", "ImportStatement",
            "Error",
        }

    # ---------------------------
    # Integer family helpers
    # ---------------------------

    def _int_kind(self, t):
        """
        Return (bits, signed) for an int-family type string, else None.
        Supports BOTH:
          - u8/i8/u16/... aliases
          - uint8/int8/uint16/int16/uint32/int32/uint64/int64
          - byte (alias of uint8)
          - int (native)
        """
        if not isinstance(t, str):
            return None

        t = self.canonical_type(t)

        if t == "int":
            return (self.native_int_bits, True)

        if t == "uint8":
            return (8, False)
        if t == "int8":
            return (8, True)
        if t == "uint16":
            return (16, False)
        if t == "int16":
            return (16, True)
        if t == "uint32":
            return (32, False)
        if t == "int32":
            return (32, True)
        if t == "uint64":
            return (64, False)
        if t == "int64":
            return (64, True)

        return None

    def is_int_family(self, t):
        return self._int_kind(t) is not None

    def promote_int(self, t):
        t = self.canonical_type(t)
        k = self._int_kind(t)
        if k is None:
            return t
        bits, _signed = k
        if bits < self.native_int_bits:
            return "int"
        if t in {"int64", "uint64"}:
            return "int"
        return t

    def _int_literal_fits(self, dst_type, value):
        """
        Allow implicit narrowing ONLY when the RHS is an int literal that fits dst.
        """
        k = self._int_kind(dst_type)
        if k is None:
            return False
        bits, signed = k
        if dst_type == "int":
            return True

        if signed:
            lo = -(1 << (bits - 1))
            hi = (1 << (bits - 1)) - 1
        else:
            lo = 0
            hi = (1 << bits) - 1
        return lo <= value <= hi

    def _promote_numeric_pair(self, a, b):
        a = self.canonical_type(a)
        b = self.canonical_type(b)

        if a == "float" or b == "float":
            if self.is_numeric(a) and self.is_numeric(b):
                return "float", "float", "float"
            return None

        if self.is_int_family(a) and self.is_int_family(b):
            a2 = self.promote_int(a)
            b2 = self.promote_int(b)
            return a2, b2, "int"

        return None

    def allow_assign(self, dst, src, src_node=None):
        dst = self.canonical_type(dst)
        src = self.canonical_type(src)

        if dst == src:
            return True

        # pointer bootstrap rules
        if self.is_pointer(dst) and src == "int" and self.is_zero_literal(src_node):
            return True
        if self.is_pointer(dst) and src == "ptr":
            return True
        if dst == "ptr" and self.is_pointer(src):
            return True
        if isinstance(dst, str) and dst.startswith("*") and src == "ptr":
            return True

        # string to ptr-ish bootstrap
        if dst == "ptr" and src == "string":
            return True
        if dst in {"*uint8", "*byte"} and src == "string":
            return True

        # widening: any int-family -> int
        if dst == "int" and self.is_int_family(src):
            return True

        # narrowing ONLY from int literals that fit
        if self.is_int_family(dst) and src == "int" and self.is_int_literal(src_node):
            v = src_node.get("value")
            if self._int_literal_fits(dst, v):
                return True

        # ---------------------------------------------------------
        # BOOTSTRAP RELAXATION:
        # allow int -> int-family even when RHS isn't a literal.
        # This avoids "Cannot assign int to uint8" for early stdlib.
        # Tighten later once typing is richer.
        # ---------------------------------------------------------
        if self.is_int_family(dst) and src == "int":
            return True

        return False

    # ---------------------------
    # Type inference
    # ---------------------------

    def infer_type(self, node):
        if not node or not isinstance(node, dict):
            return None

        node_type = node.get("type")

        if self.is_stmt_node(node):
            return None

        if node_type == "Literal":
            val = node.get("value")
            if isinstance(val, bool):
                return "bool"
            if isinstance(val, int):
                return "int"
            if isinstance(val, float):
                return "float"
            if isinstance(val, str):
                return "uint8" if len(val) == 1 else "string"
            return None

        if node_type == "NullLiteral":
            return "ptr"

        if node_type == "Variable":
            name = node.get("name")
            symbol = self.lookup_symbol(name)
            if symbol:
                vt = symbol.get("var_type")
                return self.type_to_str(vt)

            # bootstrap: allow type-name used as value
            if name and self.lookup_type(name) is not None:
                return name

            return None

        if node_type == "CastExpr":
            return self.type_to_str(node.get("target_type"))

        if node_type == "UnaryExpr":
            op = node.get("op")
            expr = node.get("expr")
            inner_t = self.infer_type(expr)

            if inner_t is None:
                return None

            inner_t = self.canonical_type(inner_t)

            if op == "&":
                return self.canonical_type(f"*{inner_t}")

            if op == "*":
                if isinstance(inner_t, str):
                    if inner_t.startswith("*"):
                        return self.canonical_type(inner_t[1:])
                    if inner_t.startswith("&mut "):
                        return self.canonical_type(inner_t[len("&mut "):])
                    if inner_t.startswith("&"):
                        return self.canonical_type(inner_t[1:])
                return None

            return inner_t

        if node_type == "BinaryExpr":
            if self.is_stmt_node(node.get("left")) or self.is_stmt_node(node.get("right")):
                return None

            op = node.get("op")
            ltype = self.canonical_type(self.infer_type(node.get("left")))
            rtype = self.canonical_type(self.infer_type(node.get("right")))

            if ltype is None or rtype is None:
                return None

            if op == "+":
                if ltype == "string" and rtype in {"int", "uint8"}:
                    return "string"
                if rtype == "string" and ltype in {"int", "uint8"}:
                    return "string"

            promo = self._promote_numeric_pair(ltype, rtype)
            if promo is not None:
                _l2, _r2, arith_out = promo

                if op in {"==", "!=", "<", ">", "<=", ">="}:
                    return "bool"

                if op in {"<<", ">>"}:
                    return "int"

                if op in {"+", "-", "*", "/", "%", "&", "|", "^"}:
                    return arith_out

            if ltype == rtype:
                if op in {"==", "!=", "<", ">", "<=", ">="}:
                    return "bool"
                return ltype

            if self.is_pointer(ltype) and self.is_int_family(rtype) and op in {"+", "-"}:
                return ltype
            if self.is_pointer(rtype) and self.is_int_family(ltype) and op in {"+", "-"}:
                return rtype

            if op in {"==", "!=", "<", ">", "<=", ">="}:
                if self.is_pointer(ltype) and self.is_pointer(rtype):
                    return "bool"
                if self.is_pointer(ltype) and self.is_int_family(rtype):
                    return "bool"
                if self.is_pointer(rtype) and self.is_int_family(ltype):
                    return "bool"

            self.errors.append(f"[TypeError] Mismatched operands: {ltype} and {rtype}")
            return None

        if node_type == "AssignExpr":
            if self.is_stmt_node(node.get("target")) or self.is_stmt_node(node.get("value")):
                return None

            ttype = self.canonical_type(self.infer_type(node.get("target")))
            vtype = self.canonical_type(self.infer_type(node.get("value")))

            if ttype and vtype and ttype != vtype:
                if self.allow_assign(ttype, vtype, node.get("value")):
                    return ttype
                self.errors.append(f"[TypeError] Cannot assign {vtype} to {ttype}")
            return ttype

        if node_type == "IndexExpr":
            return None

        if node_type == "MemberExpr":
            base_t = self.canonical_type(self.infer_type(node.get("object")))
            member = node.get("member")
            if not base_t or not member:
                return None

            root = self.unwrap_pointerish(base_t)
            if not root:
                return None

            tdecl = self.lookup_type(root)
            if not isinstance(tdecl, dict):
                return None

            if tdecl.get("type") == "StructDecl":
                for f in tdecl.get("fields", []):
                    if f.get("name") == member:
                        return self.canonical_type(f.get("type"))
                self.errors.append(f"[TypeError] Unknown field '{member}' on struct '{root}'")
                return None

            return None

        if node_type == "CallExpr":
            return self._infer_call_expr(node)

        if node_type == "MatchStatement":
            return self.visit_MatchStatement(node)

        if node_type == "Error":
            return None

        return None

    def _infer_call_expr(self, node):
        callee = node.get("callee")

        if isinstance(callee, dict) and callee.get("type") == "MemberExpr":
            self.visit_MemberExpr(callee)
            return None

        callee_name = self.expr_to_name(callee)
        if not callee_name:
            self.errors.append("[SemanticError] Call target must be a named function (like foo(...))")
            return None

        callee_sym = self.lookup_symbol(callee_name)
        if not callee_sym:
            self.errors.append(f"[SemanticError] Undefined function '{callee_name}'")
            return None

        if isinstance(callee_sym, dict) and callee_sym.get("builtin"):
            return self.type_to_str(callee_sym.get("return_type"))

        expected = callee_sym.get("params", [])
        args = node.get("args", [])

        if len(expected) != len(args):
            self.errors.append(
                f"[TypeError] Function '{callee_name}' expects {len(expected)} args, got {len(args)}"
            )
        else:
            for e, a in zip(expected, args):
                atype = self.canonical_type(self.infer_type(a))
                etype = self.canonical_type(self.type_to_str(e.get("type")))

                if not etype or atype is None:
                    continue

                if etype != atype:
                    if etype == "ptr" and self.is_pointer(atype):
                        continue
                    if isinstance(etype, str) and etype.startswith("*") and atype == "ptr":
                        continue
                    if etype == "ptr" and atype == "int" and self.is_zero_literal(a):
                        continue
                    if etype == "ptr" and atype == "string":
                        continue
                    if etype in {"*uint8", "*byte"} and atype == "string":
                        continue
                    if etype == "int" and self.is_int_family(atype):
                        continue

                    if self.is_int_family(etype) and atype == "int" and self.is_int_literal(a):
                        if self._int_literal_fits(etype, a.get("value")):
                            continue

                    self.errors.append(f"[TypeError] Argument mismatch: expected {etype}, got {atype}")

        return self.type_to_str(callee_sym.get("return_type"))

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
            if self.allow_assign(node["var_type"], inferred, value):
                pass
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
            if self.allow_assign(node["var_type"], inferred, value):
                pass
            else:
                self.errors.append(
                    f"[TypeError] Cannot assign {inferred} to global '{node['name']}' of type {node['var_type']}"
                )
        self.declare_symbol(node["name"], node)

    def visit_EnumDecl(self, node):
        name = node["name"]
        if name in self.type_scope:
            self.errors.append(f"[SemanticError] Enum '{name}' already defined")
        self.type_scope[name] = node

        seen = set()
        for variant in node.get("variants", []):
            if variant in seen:
                self.errors.append(f"[SemanticError] Duplicate variant '{variant}' in enum '{name}'")
            seen.add(variant)

    def visit_StructDecl(self, node):
        for f in node.get("fields", []):
            f["type"] = self.type_to_str(f.get("type"))
        self.declare_symbol(node["name"], node)

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
        if value_expr is not None:
            self.visit(value_expr)

        inferred = self.infer_type(value_expr)

        if declared and inferred and declared != inferred:
            if self.allow_assign(declared, inferred, value_expr):
                pass
            else:
                self.errors.append(f"[TypeError] Cannot assign {inferred} to '{node['name']}' of type {declared}")

        if not declared:
            node["var_type"] = inferred

        self.declare_symbol(node["name"], node)

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
            if self.current_return_type == "ptr" and ret_type == "int":
                return
            if self.current_return_type == "int" and self.is_int_family(ret_type):
                return

            if self.current_return_type == "ptr":
                if self.is_zero_literal(value):
                    return
                if isinstance(value, dict) and value.get("type") == "CallExpr":
                    callee = value.get("callee")
                    if isinstance(callee, dict) and callee.get("type") == "Variable" and callee.get("name") == "syscall":
                        return

            self.errors.append(
                f"[TypeError] Return type mismatch: expected {self.current_return_type}, got {ret_type}"
            )

    def visit_BreakStatement(self, node):
        if self.loop_depth <= 0:
            self.errors.append("[SemanticError] 'break' used outside of a loop")

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
        self.loop_depth += 1
        self.visit_block(node.get("body", []))
        self.loop_depth -= 1
        self.exit_scope()

    def visit_ForStatement(self, node):
        self.enter_scope()
        self.visit(node.get("init"))
        self.visit(node.get("condition"))
        self.visit(node.get("step"))
        self.loop_depth += 1
        self.visit_block(node.get("body", []))
        self.loop_depth -= 1
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
            "CallExpr", "CastExpr", "AssignExpr", "IndexExpr", "MemberExpr"
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
    # Expression visitors
    # ---------------------------

    def visit_Literal(self, node):
        return

    def visit_NullLiteral(self, node):
        return

    def visit_Error(self, node):
        return

    def visit_Variable(self, node):
        name = node.get("name")
        if not name:
            return
        if name == "syscall":
            return

        if self.lookup_type(name) is not None:
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

    def visit_MemberExpr(self, node):
        self.visit(node.get("object"))
        self.infer_type(node)

    # ---------------------------
    # Utilities
    # ---------------------------

    def unwrap_pointerish(self, t):
        t = self.canonical_type(t)
        if not isinstance(t, str):
            return None
        if t.startswith("&mut "):
            return t[len("&mut "):]
        if t.startswith("&"):
            return t[1:]
        if t.startswith("*"):
            return t[1:]
        if t == "ptr":
            return None
        return t

    def is_numeric(self, t):
        t = self.canonical_type(t)
        return isinstance(t, str) and (t == "float" or self.is_int_family(t))

    def is_pointer(self, t):
        t = self.canonical_type(t)
        return isinstance(t, str) and (t == "ptr" or t.startswith("&") or "*" in t)
