class IRBuilder:
    def __init__(self):
        self.instructions = []
        self.register_count = 0
        self.label_count = 0

        self.symbol_table = {}
        self.global_symbols = {}
        self.in_function = False
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

    # ----------------------------
    # Build / Top-level
    # ----------------------------
    def build(self, ast):
        for node in ast:
            t = node.get("type")
            if t == "FunctionDecl":
                self.compile_FunctionDecl(node)
            elif t == "GlobalDecl":
                self.compile_GlobalDecl(node)
            elif t == "ConstDecl":
                self.compile_ConstDecl(node)
        return self.functions

    # ----------------------------
    # Const folding
    # ----------------------------
    def try_eval_const(self, node):
        if node is None or not isinstance(node, dict):
            return None

        t = node.get("type")

        if t == "Literal":
            return node.get("value")

        if t == "NullLiteral":
            return 0

        if t == "UnaryExpr":
            v = self.try_eval_const(node.get("expr"))
            if v is None:
                return None
            op = node.get("op")
            if op == "-":
                return -v
            if op == "!":
                return 0 if v else 1
            return None

        if t == "BinaryExpr":
            a = self.try_eval_const(node.get("left"))
            b = self.try_eval_const(node.get("right"))
            if a is None or b is None:
                return None

            op = node.get("op")
            try:
                if op == "+":  return a + b
                if op == "-":  return a - b
                if op == "*":  return a * b
                if op == "/":  return a // b
                if op == "%":  return a % b
                if op == "|":  return a | b
                if op == "&":  return a & b
                if op == "^":  return a ^ b
                if op == "<<": return a << b
                if op == ">>": return a >> b

                if op == "==": return 1 if a == b else 0
                if op == "!=": return 1 if a != b else 0
                if op == "<":  return 1 if a < b else 0
                if op == "<=": return 1 if a <= b else 0
                if op == ">":  return 1 if a > b else 0
                if op == ">=": return 1 if a >= b else 0

                # Logical ops (no short-circuit in const fold)
                if op == "&&": return 1 if (a != 0 and b != 0) else 0
                if op == "||": return 1 if (a != 0 or b != 0) else 0
            except Exception:
                return None

        if t == "Variable":
            name = node.get("name")
            entry = self.symbol_table.get(name)
            if isinstance(entry, tuple) and entry[0] == "const":
                return entry[1]

        return None

    def compile_ConstDecl(self, node):
        name = node["name"]
        value_node = node["value"]

        folded = self.try_eval_const(value_node)
        if folded is not None:
            entry = ("const", folded)
            self.symbol_table[name] = entry
            if not self.in_function:
                self.global_symbols[name] = entry
            return

        reg = self.compile_expr(value_node)
        self.symbol_table[name] = reg
        if not self.in_function:
            self.global_symbols[name] = reg

    # ----------------------------
    # Expressions
    # ----------------------------
    def compile_Literal(self, node):
        reg = self.new_reg()
        self.emit({"op": "const", "dest": reg, "value": node["value"]})
        return reg

    def compile_NullLiteral(self, node):
        reg = self.new_reg()
        self.emit({"op": "const", "dest": reg, "value": 0})
        return reg

    def compile_Variable(self, node):
        name = node["name"]
        if name in self.symbol_table:
            ref = self.symbol_table[name]

            # Inline consts
            if isinstance(ref, tuple) and ref[0] == "const":
                reg = self.new_reg()
                self.emit({"op": "const", "dest": reg, "value": ref[1]})
                return reg

            # IMPORTANT: keep global: prefix so codegen can treat as memory
            if isinstance(ref, str) and ref.startswith("global:"):
                return ref

            return ref

        raise Exception(f"[IRBuilderError] Unknown variable '{name}'")

    def compile_IndexExpr(self, node):
        base_reg = self.compile_expr(node["target"])
        idx_reg = self.compile_expr(node["index"])
        dest = self.new_reg()
        self.emit({"op": "load_index", "dest": dest, "base": base_reg, "index": idx_reg})
        return dest

    def compile_AssignExpr(self, node):
        rhs = self.compile_expr(node["value"])
        target = node["target"]

        # assignment to variable
        if isinstance(target, dict) and target.get("type") == "Variable":
            name = target["name"]
            ref = self.symbol_table.get(name)

            if isinstance(ref, str) and ref.startswith("global:"):
                self.emit({"op": "gstore", "name": name, "value": rhs})
            else:
                self.symbol_table[name] = rhs
            return rhs

        # assignment to index: a[i] = rhs
        if isinstance(target, dict) and target.get("type") == "IndexExpr":
            base_reg = self.compile_expr(target["target"])
            idx_reg = self.compile_expr(target["index"])
            self.emit({"op": "store_index", "base": base_reg, "index": idx_reg, "value": rhs})
            return rhs

        raise Exception(f"[IRBuilderError] Unsupported assignment target: {target}")

    # normalize ops + const fold + logical &&/||
    def compile_BinaryExpr(self, node):
        # Constant-fold whole binary expression if possible
        folded = self.try_eval_const(node)
        if folded is not None:
            reg = self.new_reg()
            self.emit({"op": "const", "dest": reg, "value": folded})
            return reg

        op = node["op"]

        # ---- Logical ops: lower to boolean ops (no short-circuit yet) ----
        if op in ("&&", "||"):
            left = self.compile_expr(node["left"])
            right = self.compile_expr(node["right"])

            # l_bool = (left != 0)
            l_bool = self.new_reg()
            self.emit({"op": "ne", "dest": l_bool, "src1": left, "src2": "0"})

            # r_bool = (right != 0)
            r_bool = self.new_reg()
            self.emit({"op": "ne", "dest": r_bool, "src1": right, "src2": "0"})

            dest = self.new_reg()
            if op == "&&":
                self.emit({"op": "and", "dest": dest, "src1": l_bool, "src2": r_bool})
            else:
                self.emit({"op": "or", "dest": dest, "src1": l_bool, "src2": r_bool})
            return dest

        # ---- Normal binary ops ----
        left = self.compile_expr(node["left"])
        right = self.compile_expr(node["right"])
        dest = self.new_reg()

        op_map = {
            "+": "add",
            "-": "sub",
            "*": "mul",
            "/": "div",
            "%": "mod",

            "|": "or",
            "&": "and",
            "^": "xor",
            "<<": "shl",
            ">>": "shr",

            "==": "eq",
            "!=": "ne",
            "<": "lt",
            "<=": "le",
            ">": "gt",
            ">=": "ge",
        }

        ir_op = op_map.get(op)
        if ir_op is None:
            raise Exception(f"[IRBuilderError] Unsupported binary op: {op}")

        self.emit({"op": ir_op, "dest": dest, "src1": left, "src2": right})
        return dest

    def compile_expr(self, node):
        kind = node["type"]
        if kind == "Literal":
            return self.compile_Literal(node)
        elif kind == "NullLiteral":
            return self.compile_NullLiteral(node)
        elif kind == "Variable":
            return self.compile_Variable(node)
        elif kind == "IndexExpr":
            return self.compile_IndexExpr(node)
        elif kind == "AssignExpr":
            return self.compile_AssignExpr(node)
        elif kind == "BinaryExpr":
            return self.compile_BinaryExpr(node)
        elif kind == "CallExpr":
            return self.compile_CallExpr(node)
        elif kind == "CastExpr":
            return self.compile_CastExpr(node)
        elif kind == "UnsafeBlockExpr":
            return self.compile_UnsafeBlockExpr(node)

        raise Exception(f"[IRBuilderError] Unknown expr type: {kind}")

    # ----------------------------
    # Statements
    # ----------------------------
    def compile_LetStatement(self, node):
        expr_node = node.get("value", node.get("init"))
        dest = self.compile_expr(expr_node)
        self.symbol_table[node["name"]] = dest

    def compile_StaticDecl(self, node):
        dest = self.compile_expr(node["value"])
        self.symbol_table[node["name"]] = dest

    def compile_ForStatement(self, node):
        init = node.get("init")
        if isinstance(init, dict) and init.get("type") == "LetStatement":
            self.compile_LetStatement(init)
        elif init is not None:
            self.compile_expr(init)

        cond_label = self.new_label("for_cond")
        end_label = self.new_label("endfor")

        self.emit({"op": "label", "name": cond_label})

        cond_reg = self.compile_expr(node["condition"])
        self.emit({"op": "jeq", "src1": cond_reg, "src2": "0", "label": end_label})

        self.compile_block(node["body"])

        step = node.get("step")
        if step is not None:
            self.compile_expr(step)

        self.emit({"op": "jmp", "label": cond_label})
        self.emit({"op": "label", "name": end_label})

    def compile_WhileStatement(self, node):
        start_label = self.new_label("while")
        end_label = self.new_label("endwhile")

        self.emit({"op": "label", "name": start_label})

        cond_reg = self.compile_expr(node["condition"])
        self.emit({"op": "jeq", "src1": cond_reg, "src2": "0", "label": end_label})

        self.compile_block(node["body"])

        self.emit({"op": "jmp", "label": start_label})
        self.emit({"op": "label", "name": end_label})

    def compile_ReturnStatement(self, node):
        if node.get("value") is None:
            self.emit({"op": "ret", "value": None})
            return
        reg = self.compile_expr(node["value"])
        self.emit({"op": "ret", "value": reg})

    def compile_ExprStatement(self, node):
        self.compile_expr(node["expr"])

    def compile_block(self, block):
        for stmt in block:
            match stmt["type"]:
                case "LetStatement":
                    self.compile_LetStatement(stmt)
                case "StaticDecl":
                    self.compile_StaticDecl(stmt)
                case "ForStatement":
                    self.compile_ForStatement(stmt)
                case "WhileStatement":
                    self.compile_WhileStatement(stmt)
                case "ConstDecl":
                    self.compile_ConstDecl(stmt)
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

    # ----------------------------
    # Function / Call
    # ----------------------------
    def compile_FunctionDecl(self, node):
        self.instructions = []
        self.symbol_table = dict(self.global_symbols)
        self.register_count = 0
        self.in_function = True

        if node["name"] == "_start":
            node["params"] = []

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

        self.in_function = False

    def compile_CallExpr(self, node):
        callee_node = node["callee"]
        if isinstance(callee_node, dict) and callee_node.get("type") == "Variable":
            callee_name = callee_node["name"]
        else:
            raise Exception(f"[IRBuilderError] Unsupported call callee")

        args = [self.compile_expr(arg) for arg in node["args"]]
        dest = self.new_reg()

        if callee_name == "syscall":
            self.emit({
                "op": "syscall",
                "number": args[0],
                "args": args[1:],
                "dest": dest
            })
        else:
            self.emit({
                "op": "call",
                "func": callee_name,
                "args": args,
                "dest": dest
            })

        return dest

    # ----------------------------
    # Control flow
    # ----------------------------
    def compile_IfStatement(self, node):
        cond_reg = self.compile_expr(node["condition"])
        else_label = self.new_label("else")
        end_label = self.new_label("endif")

        self.emit({"op": "jeq", "src1": cond_reg, "src2": "0", "label": else_label})
        self.compile_block(node["then"])
        self.emit({"op": "jmp", "label": end_label})

        self.emit({"op": "label", "name": else_label})
        if node["else"]:
            self.compile_block(node["else"])

        self.emit({"op": "label", "name": end_label})

    def compile_MatchStatement(self, node):
        match_reg = self.compile_expr(node["expr"])
        end_label = self.new_label("endmatch")

        arms = node.get("arms", [])
        if not arms:
            self.emit({"op": "label", "name": end_label})
            return

        arm_labels = [self.new_label("arm") for _ in arms]
        nomatch_label = self.new_label("nomatch")

        # Compare chain first
        for i, arm in enumerate(arms):
            pattern_reg = self.compile_expr(arm["pattern"])
            self.emit({"op": "jeq", "src1": match_reg, "src2": pattern_reg, "label": arm_labels[i]})

        # If nothing matched, skip all arms
        self.emit({"op": "jmp", "label": nomatch_label})

        # Arm bodies
        for i, arm in enumerate(arms):
            self.emit({"op": "label", "name": arm_labels[i]})
            self.compile_block(arm["body"])
            self.emit({"op": "jmp", "label": end_label})

        self.emit({"op": "label", "name": nomatch_label})
        self.emit({"op": "label", "name": end_label})

    # ----------------------------
    # Cast / Unsafe
    # ----------------------------
    def format_type(self, t):
        if isinstance(t, str):
            return t
        if isinstance(t, dict) and t.get("type") == "Type":
            return t["name"]
        return str(t)

    def compile_CastExpr(self, node):
        src_reg = self.compile_expr(node["expr"])
        dest = self.new_reg()
        type_str = self.format_type(node["target_type"])
        self.emit({"op": "cast", "src": src_reg, "to": type_str, "dest": dest})
        return dest

    def compile_UnsafeBlockExpr(self, node):
        result_reg = None
        for stmt in node["body"]:
            if stmt["type"] == "ExprStatement":
                result_reg = self.compile_expr(stmt["expr"])
            else:
                result_reg = self.compile_expr(stmt)
        return result_reg

    # ----------------------------
    # Globals (.data-friendly)
    # ----------------------------
    def compile_GlobalDecl(self, node):
        name = node["name"]

        # For .data globals, require constant init for now
        folded = self.try_eval_const(node["value"])
        if folded is None:
            raise Exception(f"[IRBuilderError] Global '{name}' initializer must be a constant for now")

        # Record symbol as global memory ref
        self.symbol_table[name] = f"global:{name}"
        self.global_symbols[name] = f"global:{name}"

        # Emit a Global IR record (not an rN temp)
        self.functions.append({
            "type": "Global",
            "name": name,
            "init": folded
        })
