from lexer import Lexer

class SemanticAnalyzer:
    def __init__(self, ast):
        self.ast = ast
        self.global_scope = {}
        self.errors = []
        self.scope_stack = []
        self.current_class = None
        self.current_return_type = None

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
        if symbol:
            return symbol
        for enum in self.global_scope.values():
            if enum.get("type") == "EnumDecl" and name in enum.get("variants", []):
                return {"type": "EnumVariant", "enum": enum["name"], "name": name}
        return None

    def infer_type(self, node):
        if not node:
            return None
        node_type = node.get("type")

        if node_type == "Literal":
            val = node.get("value")
            if isinstance(val, int): return "int"
            if isinstance(val, float): return "float"
            if isinstance(val, str): return "string"

        elif node_type == "Variable":
            symbol = self.lookup_symbol(node["name"])
            if symbol:
                return symbol.get("var_type")

        elif node_type == "RefType":
            inner = node.get("inner")
            return f"&mut {inner}" if node.get("mutable") else f"&{inner}"

        elif node_type == "BinaryExpr":
            ltype = self.infer_type(node["left"])
            rtype = self.infer_type(node["right"])
            if ltype == rtype:
                return ltype
            self.errors.append(f"[TypeError] Mismatched operands: {ltype} and {rtype}")

        elif node_type == "CallExpr":
            callee = self.lookup_symbol(node["callee"])
            if not callee:
                self.errors.append(f"[SemanticError] Undefined function '{node['callee']}'")
                return None
            expected = callee.get("params", [])
            args = node.get("args", [])
            if len(expected) != len(args):
                self.errors.append(f"[TypeError] Function '{node['callee']}' expects {len(expected)} args, got {len(args)}")
            else:
                for e, a in zip(expected, args):
                    atype = self.infer_type(a)
                    etype = e.get("type")
                    if etype != atype:
                        self.errors.append(f"[TypeError] Argument mismatch: expected {etype}, got {atype}")
            return callee.get("return_type")

        elif node_type == "CastExpr":
            expr_type = self.infer_type(node["expr"])
            target_node = node.get("target_type")
            target_type = self.rewrite_type(target_node)

            if expr_type == target_type:
                return target_type
            if self.is_numeric(expr_type) and self.is_numeric(target_type):
                return target_type
            if self.is_pointer(expr_type) and self.is_pointer(target_type):
                return target_type

            self.errors.append(f"[TypeError] Invalid cast from {expr_type} to {target_type}")
            return target_type

        elif node_type == "MatchStatement":
            return self.visit_MatchStatement(node)

        return None

    def visit_FunctionDecl(self, node):
        self.declare_symbol(node["name"], node)
        self.enter_scope()
        for param in node["params"]:
            self.declare_symbol(param["name"], param)
            self.visit_type(param["type"])
        self.current_return_type = node["return_type"]
        self.visit_block(node["body"])
        self.exit_scope()
        self.current_return_type = None

    def visit_ReturnStatement(self, node):
        self.visit(node["value"])
        ret_type = self.infer_type(node["value"])
        if self.current_return_type and ret_type and ret_type != self.current_return_type:
            self.errors.append(f"[TypeError] Return type mismatch: expected {self.current_return_type}, got {ret_type}")

    def visit_LetStatement(self, node):
        declared = node.get("var_type")
        value_expr = node.get("value")
        inferred = self.infer_type(value_expr)

        if isinstance(declared, dict):  # if it's a RefType or Type node
            parsed = self.rewrite_type(declared)
            node["var_type"] = parsed
            declared = parsed

        if declared and inferred and declared != inferred:
            self.errors.append(f"[TypeError] Cannot assign {inferred} to '{node['name']}' of type {declared}")

        if not declared:
            node["var_type"] = inferred

        self.declare_symbol(node["name"], node)
        self.visit(value_expr)

    def visit_GlobalDecl(self, node):
        name = node["name"]
        self.visit(node["value"])
        inferred = self.infer_type(node["value"])
        node["var_type"] = inferred
        self.declare_symbol(name, node)

    def visit_MatchStatement(self, node):
        self.visit(node["expr"])
        expected = self.infer_type(node["expr"])
        result_type = None
        for arm in node["arms"]:
            self.enter_scope()
            self.visit(arm["pattern"])
            for stmt in arm["body"]:
                self.visit(stmt)
            rtype = self.infer_type(arm["body"][-1]) if arm["body"] else None
            if result_type is None:
                result_type = rtype
            elif rtype != result_type:
                self.errors.append(f"[TypeError] Mismatched match arm results: {result_type} vs {rtype}")
            self.exit_scope()
        return result_type

    def visit_type(self, node):
        if isinstance(node, dict) and node.get("type") == "RefType":
            self.visit_type({"type": "Type", "name": node.get("inner")})

    def rewrite_type(self, node):
        if isinstance(node, dict):
            if node.get("type") == "RefType":
                return f"&mut {node['inner']}" if node["mutable"] else f"&{node['inner']}"
            elif node.get("type") == "Type":
                return node["name"]
        return node

    def is_numeric(self, t):
        return isinstance(t, str) and t.startswith("int") or t.startswith("uint") or t == "float"

    def is_pointer(self, t):
        return isinstance(t, str) and (t.startswith("&") or "*" in t)

    def visit_block(self, block):
        for stmt in block:
            self.visit(stmt)

    def visit_CastExpr(self, node):
        expr_type = self.infer_type(node["expr"])
        target_type = self.rewrite_type(node.get("target_type"))
        if expr_type != target_type:
            self.errors.append(f"[TypeError] Cannot cast from {expr_type} to {target_type}")
        self.visit(node["expr"])

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

    def visit_ImportStatement(self, node):
        if not isinstance(node.get("path"), str):
            self.errors.append("[SemanticError] Invalid import path")

    def visit_UnsafeBlock(self, node):
        self.enter_scope()
        for stmt in node["body"]:
            self.visit(stmt)
        self.exit_scope()

    def visit_UnsafeBlockExpr(self, node):
        self.enter_scope()
        for stmt in node["body"]:
            self.visit(stmt)
        self.exit_scope()
