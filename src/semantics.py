from lexer import Lexer

class SemanticAnalyzer:
    def __init__(self, ast):
        self.ast = ast
        self.global_scope = {}
        self.errors = []
        self.scope_stack = []  # stack of local scopes
        self.current_class = None
        self.current_return_type = None

    def analyze(self):
        for node in self.ast:
            self.visit(node)
        return self.errors

    def visit(self, node):
        if not isinstance(node, dict):
            return
        node_type = node.get("type")
        method = getattr(self, f"visit_{node_type}", self.generic_visit)
        return method(node)

    def generic_visit(self, node):
        self.errors.append(f"[SemanticError] Unknown node type: {node.get('type')}")

    def enter_scope(self):
        self.scope_stack.append({})

    def exit_scope(self):
        self.scope_stack.pop()

    def declare_symbol(self, name, value):
        if self.scope_stack:
            current_scope = self.scope_stack[-1]
            if name in current_scope:
                self.errors.append(f"[SemanticError] Symbol '{name}' already declared in local scope")
            current_scope[name] = value
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

        # Try to resolve as enum variant
        for enum in self.global_scope.values():
            if enum.get("type") == "EnumDecl":
                if name in enum.get("variants", []):
                    return {"type": "EnumVariant", "enum": enum["name"], "name": name}
        return None


    def infer_type(self, node):
        if node is None:
            return None
        node_type = node.get("type")
        if node_type == "Literal":
            val = node.get("value")
            if isinstance(val, int):
                return "int"
            elif isinstance(val, float):
                return "float"
            elif isinstance(val, str):
                return "string"
        elif node_type == "Variable":
            symbol = self.lookup_symbol(node["name"])
            if symbol:
                return symbol.get("var_type")
        elif node_type == "BinaryExpr":
            left_type = self.infer_type(node["left"])
            right_type = self.infer_type(node["right"])
            if left_type == right_type:
                return left_type
            else:
                self.errors.append(f"[TypeError] Mismatched operand types: {left_type} and {right_type}")
        elif node_type == "CallExpr":
            callee = self.lookup_symbol(node["callee"])
            if not callee:
                self.errors.append(f"[SemanticError] Undefined function '{node['callee']}'")
                return None
            expected = callee.get("params", [])
            args = node.get("args", [])
            if len(expected) != len(args):
                self.errors.append(f"[TypeError] Function '{node['callee']}' expects {len(expected)} arguments, got {len(args)}")
            else:
                for e, a in zip(expected, args):
                    arg_type = self.infer_type(a)
                    if arg_type and arg_type != e["type"]:
                        self.errors.append(f"[TypeError] Argument type mismatch: expected {e['type']}, got {arg_type}")
            return callee.get("return_type")
        elif node_type == "MatchStatement":
            return self.visit_MatchStatement(node)
        return None

    def visit_FunctionDecl(self, node):
        name = node["name"]
        self.declare_symbol(name, node)
        self.enter_scope()
        for param in node.get("params", []):
            self.declare_symbol(param["name"], param)
            self.visit_type(param.get("type"))
        self.current_return_type = node.get("return_type")
        self.visit_block(node["body"])
        self.exit_scope()
        self.current_return_type = None

    def visit_ReturnStatement(self, node):
        self.visit(node["value"])
        inferred = self.infer_type(node["value"])
        if self.current_return_type and inferred and inferred != self.current_return_type:
            self.errors.append(f"[TypeError] Return type mismatch: expected {self.current_return_type}, got {inferred}")

    def visit_ClassDecl(self, node):
        name = node["name"]
        self.declare_symbol(name, node)
        field_names = set()
        method_names = set()
        self.current_class = node
        for field in node.get("fields", []):
            fname = field["name"]
            if fname in field_names:
                self.errors.append(f"[SemanticError] Duplicate field '{fname}' in class '{name}'")
            field_names.add(fname)
            self.visit_type(field["type"])
        for method in node.get("methods", []):
            mname = method["name"]
            if mname in method_names:
                self.errors.append(f"[SemanticError] Duplicate method '{mname}' in class '{name}'")
            method_names.add(mname)
            self.visit(method)
        self.current_class = None

    def visit_EnumDecl(self, node):
        name = node["name"]
        if name in self.global_scope:
            self.errors.append(f"[SemanticError] Enum '{name}' already defined")
        self.global_scope[name] = node
        variant_names = set()
        for variant in node.get("variants", []):
            if variant in variant_names:
                self.errors.append(f"[SemanticError] Duplicate variant '{variant}' in enum '{name}'")
            variant_names.add(variant)

    def visit_LetStatement(self, node):
        declared_type = node.get("var_type")
        inferred_type = self.infer_type(node.get("init"))
        if declared_type and inferred_type and declared_type != inferred_type:
            self.errors.append(f"[TypeError] Cannot assign {inferred_type} to variable '{node['name']}' of type {declared_type}")
        if not declared_type:
            node["var_type"] = inferred_type
        self.declare_symbol(node["name"], node)
        if node.get("init"):
            self.visit(node.get("init"))

    def visit_MatchStatement(self, node):
        match_expr_type = self.infer_type(node["expr"])
        self.visit(node["expr"])
        result_type = None
        for arm in node["arms"]:
            self.enter_scope()
            self.visit(arm["pattern"])
            for stmt in arm["body"]:
                self.visit(stmt)
            arm_type = self.infer_type(arm["body"][-1]) if arm["body"] else None
            if result_type is None:
                result_type = arm_type
            elif arm_type != result_type:
                self.errors.append(f"[TypeError] Mismatched match arm result types: {result_type} vs {arm_type}")
            self.exit_scope()
        return result_type

    def visit_type(self, type_node):
        if isinstance(type_node, dict):
            if type_node.get("type") in {"UnsafeType", "PointerType"}:
                self.visit_type(type_node.get("inner"))

    def visit_block(self, block):
        for stmt in block:
            self.visit(stmt)