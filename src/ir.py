from semantics import SemanticAnalyzer

class IRBuilder:
    def __init__(self):
        self.instructions = []
        self.register_count = 0
        self.label_count = 0
        self.symbol_table = {}  # maps variable names → registers
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
        return self.functions

    def compile_Literal(self, node):
        reg = self.new_reg()
        self.emit({ "op": "const", "dest": reg, "value": node["value"] })
        return reg

    def compile_Variable(self, node):
        name = node["name"]
        if name in self.symbol_table:
            return self.symbol_table[name]
        raise Exception(f"[IRBuilderError] Unknown variable '{name}'")

    def compile_BinaryExpr(self, node):
        left = self.compile_expr(node["left"])
        right = self.compile_expr(node["right"])
        dest = self.new_reg()
        self.emit({ "op": node["op"], "dest": dest, "src1": left, "src2": right })
        return dest

    def compile_expr(self, node):
        if node["type"] == "Literal":
            return self.compile_Literal(node)
        elif node["type"] == "Variable":
            return self.compile_Variable(node)
        elif node["type"] == "BinaryExpr":
            return self.compile_BinaryExpr(node)
        elif node["type"] == "CallExpr":
            return self.compile_CallExpr(node)
        raise Exception(f"[IRBuilderError] Unknown expr type: {node['type']}")

    def compile_LetStatement(self, node):
        dest = self.compile_expr(node["init"])
        self.symbol_table[node["name"]] = dest  # link var name to register

    def compile_ReturnStatement(self, node):
        reg = self.compile_expr(node["value"])
        self.emit({ "op": "ret", "value": reg })

    def compile_ExprStatement(self, node):
        self.compile_expr(node["expr"])  # result is unused

    def compile_block(self, block):
        for stmt in block:
            kind = stmt["type"]
            if kind == "LetStatement":
                self.compile_LetStatement(stmt)
            elif kind == "ReturnStatement":
                self.compile_ReturnStatement(stmt)
            elif kind == "ExprStatement":
                self.compile_ExprStatement(stmt)
            elif kind == "IfStatement":
                self.compile_IfStatement(stmt)
            elif kind == "MatchStatement":
                self.compile_MatchStatement(stmt)
            else:
                raise Exception(f"[IRBuilderError] Unknown stmt: {kind}")

    def compile_FunctionDecl(self, node):
        self.instructions = []
        self.symbol_table = {}
        self.register_count = 0

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
            # First arg is syscall number, remaining are syscall args
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

        # If condition fails, jump to else
        self.emit({ "op": "jeq", "src1": cond_reg, "src2": "0", "label": else_label })

        # Then block
        self.compile_block(node["then"])
        self.emit({ "op": "jmp", "label": end_label })

        # Else block
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

            self.emit({
                "op": "jeq", "src1": match_reg, "src2": pattern_reg, "label": arm_label
            })

            self.emit({ "op": "label", "name": arm_label })
            self.compile_block(arm["body"])
            self.emit({ "op": "jmp", "label": end_label })

        self.emit({ "op": "label", "name": end_label })
