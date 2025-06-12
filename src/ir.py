class IRBuilder:
    def __init__(self):
        self.instructions = []
        self.register_count = 0
        self.label_count = 0
        self.symbol_table = {}
        self.functions = []

    def new_reg(self):
        reg = f"r{self.register_count}"
        self.register_count += 1
        return reg

    def new_label(self, base="L"):
        label = f"{base}{self.label_count}"
        self.label_count += 1
        return label

    def emit(self, instr):
        self.instructions.append(instr)

    def build(self, ast):
        for node in ast:
            if node["type"] == "FunctionDecl":
                self.compile_FunctionDecl(node)
            elif node["type"] == "GlobalDecl":
                self.compile_GlobalDecl(node)
        return self.functions

    def compile_Literal(self, node):
        reg = self.new_reg()
        self.emit({ "op": "const", "dest": reg, "value": node["value"] })
        return reg

    def compile_Variable(self, node):
        name = node["name"]
        if name in self.symbol_table:
            ref = self.symbol_table[name]
            return ref.split(":", 1)[1] if ref.startswith("global:") else ref
        raise Exception(f"[IRBuilderError] Unknown variable '{name}'")

    def compile_BinaryExpr(self, node):
        left = self.compile_expr(node["left"])
        right = self.compile_expr(node["right"])
        dest = self.new_reg()
        self.emit({ "op": node["op"], "dest": dest, "src1": left, "src2": right })
        return dest

    def compile_expr(self, node):
        kind = node["type"]
        if kind == "Literal":
            return self.compile_Literal(node)
        elif kind == "Variable":
            return self.compile_Variable(node)
        elif kind == "BinaryExpr":
            return self.compile_BinaryExpr(node)
        elif kind == "CallExpr":
            return self.compile_CallExpr(node)
        elif kind == "CastExpr":
            return self.compile_CastExpr(node)
        elif kind == "UnsafeBlockExpr":
            return self.compile_UnsafeBlockExpr(node)

        print(f"[debug] compile_expr → unknown kind: {kind} → {node}")
        raise Exception(f"[IRBuilderError] Unknown expr type: {kind}")

    def compile_LetStatement(self, node):
        dest = self.compile_expr(node["init"])  # ← FIXED from "value" to "init"
        self.symbol_table[node["name"]] = dest

    def compile_ReturnStatement(self, node):
        reg = self.compile_expr(node["value"])
        self.emit({ "op": "ret", "value": reg })

    def compile_ExprStatement(self, node):
        self.compile_expr(node["expr"])  # Could optionally save reg here

    def compile_block(self, block):
        for stmt in block:
            match stmt["type"]:
                case "LetStatement":
                    self.compile_LetStatement(stmt)
                case "ReturnStatement":
                    self.compile_ReturnStatement(stmt)
                case "ExprStatement":
                    self.compile_ExprStatement(stmt)
                case "IfStatement":
                    self.compile_IfStatement(stmt)
                case "MatchStatement":
                    self.compile_MatchStatement(stmt)
                case "UnsafeBlock":
                    self.compile_block(stmt["body"])
                case _:
                    raise Exception(f"[IRBuilderError] Unknown stmt: {stmt['type']}")

    def compile_FunctionDecl(self, node):
        self.instructions = []
        self.symbol_table = {}
        self.register_count = 0

        if node["name"] == "_start":
            node["params"] = []  # ← ensure no params for _start

        param_regs = []
        for i, param in enumerate(node["params"]):
            reg = f"arg{i}"
            self.symbol_table[param["name"]] = reg
            param_regs.append(reg)

        self.compile_block(node["body"])

        self.functions.append({
            "type": "Function",
            "name": node["name"],
            "params": param_regs,
            "body": self.instructions
        })

    def compile_CallExpr(self, node):
        callee = node["callee"]
        args = [self.compile_expr(arg) for arg in node["args"]]
        dest = self.new_reg()

        if callee == "syscall":
            self.emit({
                "op": "syscall",
                "number": args[0],
                "args": args[1:],
                "dest": dest
            })
        else:
            self.emit({
                "op": "call",
                "func": callee,
                "args": args,
                "dest": dest
            })

        return dest

    def compile_IfStatement(self, node):
        cond_reg = self.compile_expr(node["condition"])
        else_label = self.new_label("else")
        end_label = self.new_label("endif")

        self.emit({ "op": "jeq", "src1": cond_reg, "src2": "0", "label": else_label })
        self.compile_block(node["then"])
        self.emit({ "op": "jmp", "label": end_label })

        self.emit({ "op": "label", "name": else_label })
        if node["else"]:
            self.compile_block(node["else"])

        self.emit({ "op": "label", "name": end_label })

    def compile_MatchStatement(self, node):
        match_reg = self.compile_expr(node["expr"])
        end_label = self.new_label("endmatch")

        for arm in node["arms"]:
            arm_label = self.new_label("arm")
            pattern_reg = self.compile_expr(arm["pattern"])
            self.emit({ "op": "jeq", "src1": match_reg, "src2": pattern_reg, "label": arm_label })

            self.emit({ "op": "label", "name": arm_label })
            self.compile_block(arm["body"])
            self.emit({ "op": "jmp", "label": end_label })

        self.emit({ "op": "label", "name": end_label })

    def format_type(self, t):
        if isinstance(t, str): return t
        if isinstance(t, dict) and t.get("type") == "Type": return t["name"]
        return str(t)

    def compile_CastExpr(self, node):
        src_reg = self.compile_expr(node["expr"])
        dest = self.new_reg()
        type_str = self.format_type(node["target_type"])
        self.emit({ "op": "cast", "src": src_reg, "to": type_str, "dest": dest })
        return dest

    def compile_UnsafeBlockExpr(self, node):
        result_reg = None
        for stmt in node["body"]:
            if stmt["type"] == "ExprStatement":
                result_reg = self.compile_expr(stmt["expr"])
            else:
                result_reg = self.compile_expr(stmt)
        return result_reg

    def compile_GlobalDecl(self, node):
        name = node["name"]
        value_reg = self.compile_expr(node["value"])
        self.symbol_table[name] = f"global:{name}"
        self.functions.append({
            "type": "Global",
            "name": name,
            "value": value_reg
        })
