# ir.py (master)

class IRBuilder:
    def __init__(self, debug: bool = False):
        self.debug = debug

        self.instructions = []
        self.register_count = 0
        self.label_count = 0

        self.symbol_table = {}
        self.global_symbols = {}
        self.extern_symbols = set()   # track auto-externs
        self.in_function = False
        self.functions = []

        # loop end-label stack for break
        self.loop_end_stack = []

        # Minimal struct layout info
        self.structs = {}

        # ✅ Track locals that were auto-allocated as struct storage.
        # For these, the variable already holds an ADDRESS (a pointer).
        # So "&var" must return var (not the address of the pointer slot).
        self.struct_local_ptrs = set()

        self.type_sizes = {
            "byte": 1,
            "bool": 1,
            "int": 8,
            "ptr": 8,
        }

    def dbg(self, *args):
        if self.debug:
            print(*args)

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
        # reset outputs for a fresh build
        self.instructions = []
        self.register_count = 0
        self.label_count = 0
        self.symbol_table = {}
        self.global_symbols = {}
        self.extern_symbols = set()
        self.in_function = False
        self.functions = []
        self.loop_end_stack = []
        self.structs = {}
        self.struct_local_ptrs = set()   # ✅ reset

        # Pass 1: gather struct layouts so MemberExpr works
        for node in ast:
            if isinstance(node, dict) and node.get("type") == "StructDecl":
                self.collect_struct(node)

        # Pass 2: compile all top-level data/const first
        for node in ast:
            if not isinstance(node, dict):
                continue
            t = node.get("type")

            if t == "ConstDecl":
                self.compile_ConstDecl(node)
            elif t == "GlobalDecl":
                self.compile_GlobalDecl(node)
            elif t == "StaticDecl":
                self.compile_StaticDecl(node)
            elif t == "StructDecl":
                pass

        # Pass 3: compile functions
        for node in ast:
            if isinstance(node, dict) and node.get("type") == "FunctionDecl":
                self.compile_FunctionDecl(node)

        return self.functions

    # ----------------------------
    # Struct layout
    # ----------------------------
    def sizeof(self, t):
        # Returns byte-size of a type string
        if not isinstance(t, str):
            return 8
        if t.startswith("*") or t.startswith("&"):
            return 8
        if t in self.structs:
            return int(self.structs[t]["size"])
        return int(self.type_sizes.get(t, 8))

    def collect_struct(self, node):
        name = node["name"]
        fields = []
        offsets = {}
        off = 0
        for f in node.get("fields", []):
            fname = f.get("name")
            ftype = f.get("type")
            if isinstance(ftype, dict):
                # e.g. {"type":"Type","name":"int"}
                ftype = ftype.get("name") if isinstance(ftype, dict) else str(ftype)
            if not isinstance(ftype, str):
                ftype = str(ftype)

            fields.append((fname, ftype))
            offsets[fname] = off
            off += self.sizeof(ftype)

        self.structs[name] = {"fields": fields, "offsets": offsets, "size": off}

        # Make `sizeof(structname)` work via type_sizes too (handy for debugging)
        self.type_sizes[name] = off

    def field_offset(self, struct_name, field_name):
        s = self.structs.get(struct_name)
        if not s:
            return None
        return s["offsets"].get(field_name)

    # ----------------------------
    # Helpers for AST shape drift
    # ----------------------------
    def member_parts(self, node):
        obj = node.get("object")
        if obj is None:
            obj = node.get("target")
        if obj is None:
            obj = node.get("base")

        mem = node.get("member")
        if mem is None:
            mem = node.get("field")
        if mem is None:
            mem = node.get("name")

        return obj, mem

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
            if op == "~":
                return ~v
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
        """
        Bootstrap rule: const MUST fold (no top-level IR).
        """
        name = node["name"]
        value_node = node["value"]

        folded = self.try_eval_const(value_node)
        if folded is None:
            raise Exception(f"[IRBuilderError] Const '{name}' must be a constant for now")

        entry = ("const", folded)
        self.symbol_table[name] = entry
        if not self.in_function:
            # overwrite any extern placeholder
            self.global_symbols[name] = entry
            self.extern_symbols.discard(name)

    # ----------------------------
    # Extern handling
    # ----------------------------
    def declare_extern(self, name: str):
        if name in self.global_symbols:
            return  # already known (const/global/etc)
        # IMPORTANT: don't extern a struct type name
        if name in self.structs:
            return
        self.global_symbols[name] = f"global:{name}"
        self.extern_symbols.add(name)
        self.functions.append({"type": "Extern", "name": name})

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

    def compile_Error(self, node):
        reg = self.new_reg()
        self.emit({"op": "const", "dest": reg, "value": 0})
        return reg

    def compile_Variable(self, node):
        name = node["name"]

        # FIX: referencing a struct type name as a value => sizeof(Struct)
        if name in self.structs:
            reg = self.new_reg()
            self.emit({"op": "const", "dest": reg, "value": int(self.structs[name]["size"])})
            return reg

        if name in self.symbol_table:
            ref = self.symbol_table[name]

            if isinstance(ref, tuple) and ref[0] == "const":
                reg = self.new_reg()
                self.emit({"op": "const", "dest": reg, "value": ref[1]})
                return reg

            if isinstance(ref, str) and ref.startswith("global:"):
                return ref

            return ref

        # Bootstrap/linker-friendly behavior:
        # If a variable isn’t in scope, treat it as an extern global symbol.
        self.declare_extern(name)
        self.symbol_table[name] = f"global:{name}"
        return f"global:{name}"

    def compile_IndexExpr(self, node):
        """
        Bootstrap typing hack:

        - argv is an array of pointers (ptr* / char** conceptually).
          So argv[i] must load a qword pointer at base + i*8.

        - Everything else continues to use byte indexing for now
          (strings/buffers).
        """
        tgt = node.get("target")

        # ✅ Special-case argv[index] => pointer indexing
        if isinstance(tgt, dict) and tgt.get("type") == "Variable" and tgt.get("name") == "argv":
            base_reg = self.compile_expr(tgt)
            idx_reg = self.compile_expr(node["index"])
            dest = self.new_reg()
            self.emit({"op": "load_ptr_index", "dest": dest, "base": base_reg, "index": idx_reg})
            return dest

        # Default: byte indexing
        base_reg = self.compile_expr(node["target"])
        idx_reg = self.compile_expr(node["index"])
        dest = self.new_reg()
        self.emit({"op": "load_index", "dest": dest, "base": base_reg, "index": idx_reg})
        return dest

    def compile_MemberExpr(self, node):
        obj, member = self.member_parts(node)
        if obj is None or not member:
            return self.compile_Error(node)

        if isinstance(obj, dict) and obj.get("type") == "UnaryExpr" and obj.get("op") == "*":
            base_ptr = self.compile_expr(obj.get("expr"))
            struct_name = self.infer_struct_name(obj.get("expr"))
        else:
            base_ptr = self.compile_expr(obj)
            struct_name = self.infer_struct_name(obj)

        if struct_name is None:
            raise Exception(f"[IRBuilderError] Cannot resolve struct type for MemberExpr .{member}")

        off = self.field_offset(struct_name, member)
        if off is None:
            raise Exception(f"[IRBuilderError] Unknown field '{member}' on struct '{struct_name}'")

        dest = self.new_reg()
        self.emit({"op": "load_field", "dest": dest, "base": base_ptr, "offset": off})
        return dest

    def compile_AssignExpr(self, node):
        rhs = self.compile_expr(node["value"])
        target = node["target"]

        if isinstance(target, dict) and target.get("type") == "Variable":
            name = target["name"]
            ref = self.symbol_table.get(name)

            if isinstance(ref, str) and ref.startswith("global:"):
                self.emit({"op": "gstore", "name": name, "value": rhs})
                return rhs

            if ref is None:
                self.declare_extern(name)
                self.emit({"op": "gstore", "name": name, "value": rhs})
                self.symbol_table[name] = f"global:{name}"
                return rhs

            self.emit({"op": "mov", "dest": ref, "src": rhs})
            return ref

        if isinstance(target, dict) and target.get("type") == "IndexExpr":
            base_reg = self.compile_expr(target["target"])
            idx_reg = self.compile_expr(target["index"])
            self.emit({"op": "store_index", "base": base_reg, "index": idx_reg, "value": rhs})
            return rhs

        if isinstance(target, dict) and target.get("type") == "MemberExpr":
            obj, member = self.member_parts(target)
            if obj is None or not member:
                return rhs

            if isinstance(obj, dict) and obj.get("type") == "UnaryExpr" and obj.get("op") == "*":
                base_ptr = self.compile_expr(obj.get("expr"))
                struct_name = self.infer_struct_name(obj.get("expr"))
            else:
                base_ptr = self.compile_expr(obj)
                struct_name = self.infer_struct_name(obj)

            if struct_name is None:
                raise Exception(f"[IRBuilderError] Cannot resolve struct type for field store .{member}")

            off = self.field_offset(struct_name, member)
            if off is None:
                raise Exception(f"[IRBuilderError] Unknown field '{member}' on struct '{struct_name}'")

            self.emit({"op": "store_field", "base": base_ptr, "offset": off, "value": rhs})
            return rhs

        raise Exception(f"[IRBuilderError] Unsupported assignment target: {target}")

    def compile_BinaryExpr(self, node):
        folded = self.try_eval_const(node)
        if folded is not None:
            reg = self.new_reg()
            self.emit({"op": "const", "dest": reg, "value": folded})
            return reg

        op = node["op"]

        if op in ("&&", "||"):
            left = self.compile_expr(node["left"])
            right = self.compile_expr(node["right"])

            l_bool = self.new_reg()
            self.emit({"op": "ne", "dest": l_bool, "src1": left, "src2": "0"})

            r_bool = self.new_reg()
            self.emit({"op": "ne", "dest": r_bool, "src1": right, "src2": "0"})

            dest = self.new_reg()
            self.emit({"op": "and" if op == "&&" else "or", "dest": dest, "src1": l_bool, "src2": r_bool})
            return dest

        left = self.compile_expr(node["left"])
        right = self.compile_expr(node["right"])
        dest = self.new_reg()

        op_map = {
            "+": "add", "-": "sub", "*": "mul", "/": "div", "%": "mod",
            "|": "or", "&": "and", "^": "xor", "<<": "shl", ">>": "shr",
            "==": "eq", "!=": "ne", "<": "lt", "<=": "le", ">": "gt", ">=": "ge",
        }

        ir_op = op_map.get(op)
        if ir_op is None:
            raise Exception(f"[IRBuilderError] Unsupported binary op: {op}")

        self.emit({"op": ir_op, "dest": dest, "src1": left, "src2": right})
        return dest

    def compile_UnaryExpr(self, node):
        op = node.get("op")
        expr = node.get("expr")

        # FIX: "&x" should produce an address
        if op == "&":
            if isinstance(expr, dict) and expr.get("type") == "Variable":
                name = expr.get("name")

                # ✅ If we auto-allocated a struct local, the var already holds an address.
                # So "&var" must just be "var" (avoid creating Buffer**).
                if name in self.struct_local_ptrs:
                    return self.compile_Variable(expr)

                # Global -> addressable global marker
                if (
                    name in self.global_symbols or
                    (name in self.symbol_table and isinstance(self.symbol_table[name], str) and self.symbol_table[name].startswith("global:"))
                ):
                    return f"addr_global:{name}"

                ref = self.symbol_table.get(name)

                # Local slot -> addr_local
                if isinstance(ref, str) and ref.startswith("r"):
                    out = self.new_reg()
                    self.emit({"op": "addr_local", "dest": out, "src": ref})
                    return out

            # fallback
            return self.compile_expr(expr)

        # "*" in this bootstrap IR means "already a pointer expression"
        if op == "*":
            return self.compile_expr(expr)

        src = self.compile_expr(expr)
        dest = self.new_reg()

        if op == "-":
            self.emit({"op": "neg", "dest": dest, "src": src})
            return dest

        if op == "!":
            self.emit({"op": "eq", "dest": dest, "src1": src, "src2": "0"})
            return dest

        # ✅ bitwise NOT: ~x  ==>  x xor -1
        if op == "~":
            mask = self.new_reg()
            self.emit({"op": "const", "dest": mask, "value": -1})
            self.emit({"op": "xor", "dest": dest, "src1": src, "src2": mask})
            return dest

        raise Exception(f"[IRBuilderError] Unsupported unary op: {op}")

    def compile_expr(self, node):
        if node is None:
            raise Exception("[IRBuilderError] compile_expr got None (missing initializer or malformed AST)")

        kind = node["type"]
        if kind == "Literal":
            return self.compile_Literal(node)
        if kind == "NullLiteral":
            return self.compile_NullLiteral(node)
        if kind == "Error":
            return self.compile_Error(node)
        if kind == "Variable":
            return self.compile_Variable(node)
        if kind == "IndexExpr":
            return self.compile_IndexExpr(node)
        if kind == "MemberExpr":
            return self.compile_MemberExpr(node)
        if kind == "AssignExpr":
            return self.compile_AssignExpr(node)
        if kind == "BinaryExpr":
            return self.compile_BinaryExpr(node)
        if kind == "UnaryExpr":
            return self.compile_UnaryExpr(node)
        if kind == "CallExpr":
            return self.compile_CallExpr(node)
        if kind == "CastExpr":
            return self.compile_CastExpr(node)
        if kind == "UnsafeBlockExpr":
            return self.compile_UnsafeBlockExpr(node)

        raise Exception(f"[IRBuilderError] Unknown expr type: {kind}")

    # ----------------------------
    # Statements
    # ----------------------------
    def _declared_type_name(self, node):
        """
        Best-effort extract of a declared type name for LetStatement.
        """
        for k in ("var_type", "decl_type", "annot", "annotation", "value_type"):
            if k in node:
                t = node[k]
                if isinstance(t, dict):
                    if t.get("type") == "Type" and "name" in t:
                        return t["name"]
                    if "name" in t:
                        return t["name"]
                if isinstance(t, str):
                    return t
        return None

    def compile_LetStatement(self, node):
        expr_node = node.get("value")
        if expr_node is None:
            expr_node = node.get("init")

        declared_t = self._declared_type_name(node)

        # FIX: struct local with no initializer => allocate stack storage and bind var to pointer
        if expr_node is None and declared_t in self.structs:
            size = int(self.structs[declared_t]["size"])
            ptr = self.new_reg()
            self.emit({"op": "alloca", "dest": ptr, "size": size})
            self.symbol_table[node["name"]] = ptr
            self.struct_local_ptrs.add(node["name"])   # ✅ mark as "already an address"
            return

        if expr_node is None:
            rhs = self.new_reg()
            self.emit({"op": "const", "dest": rhs, "value": 0})
        else:
            rhs = self.compile_expr(expr_node)

        slot = self.new_reg()
        self.emit({"op": "mov", "dest": slot, "src": rhs})
        self.symbol_table[node["name"]] = slot

    def compile_StaticDecl(self, node):
        name = node["name"]

        # Top-level static: must be .data-friendly constant init (like GlobalDecl)
        if not self.in_function:
            folded = self.try_eval_const(node["value"])
            if folded is None:
                raise Exception(f"[IRBuilderError] Static '{name}' initializer must be a constant for now")

            self.symbol_table[name] = f"global:{name}"
            self.global_symbols[name] = f"global:{name}"
            self.extern_symbols.discard(name)
            self.functions.append({"type": "Global", "name": name, "init": folded})
            return

        rhs = self.compile_expr(node["value"])
        slot = self.new_reg()
        self.emit({"op": "mov", "dest": slot, "src": rhs})
        self.symbol_table[name] = slot

    def compile_BreakStatement(self, node):
        if not self.loop_end_stack:
            raise Exception("[IRBuilderError] 'break' used outside of a loop")
        self.emit({"op": "jmp", "label": self.loop_end_stack[-1]})

    def compile_ForStatement(self, node):
        init = node.get("init")
        if isinstance(init, dict) and init.get("type") == "LetStatement":
            self.compile_LetStatement(init)
        elif init is not None:
            self.compile_expr(init)

        cond_label = self.new_label("for_cond")
        end_label = self.new_label("endfor")

        self.loop_end_stack.append(end_label)

        self.emit({"op": "label", "name": cond_label})
        cond_reg = self.compile_expr(node["condition"])
        self.emit({"op": "jeq", "src1": cond_reg, "src2": "0", "label": end_label})

        self.compile_block(node["body"])

        step = node.get("step")
        if step is not None:
            self.compile_expr(step)

        self.emit({"op": "jmp", "label": cond_label})
        self.emit({"op": "label", "name": end_label})

        self.loop_end_stack.pop()

    def compile_WhileStatement(self, node):
        start_label = self.new_label("while")
        end_label = self.new_label("endwhile")

        self.loop_end_stack.append(end_label)

        self.emit({"op": "label", "name": start_label})

        cond_reg = self.compile_expr(node["condition"])
        self.emit({"op": "jeq", "src1": cond_reg, "src2": "0", "label": end_label})

        self.compile_block(node["body"])

        self.emit({"op": "jmp", "label": start_label})
        self.emit({"op": "label", "name": end_label})

        self.loop_end_stack.pop()

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
                case "BreakStatement":
                    self.compile_BreakStatement(stmt)
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
        self.loop_end_stack = []
        self.struct_local_ptrs = set()   # ✅ reset per-function locals

        # NOTE: Do NOT strip params from _start.
        # We need to support argc/argv for bootstrapping and for the linker.
        # (Codegen handles pulling them from the entry stack.)
        param_regs = []
        for i, param in enumerate(node["params"]):
            arg = f"arg{i}"
            param_regs.append(arg)

            slot = self.new_reg()
            self.emit({"op": "mov", "dest": slot, "src": arg})
            self.symbol_table[param["name"]] = slot

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
            raise Exception("[IRBuilderError] Unsupported call callee")

        args = [self.compile_expr(arg) for arg in node["args"]]
        dest = self.new_reg()

        if callee_name == "syscall":
            self.emit({"op": "syscall", "number": args[0], "args": args[1:], "dest": dest})
        else:
            self.emit({"op": "call", "func": callee_name, "args": args, "dest": dest})

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
        if node.get("else"):
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

        for i, arm in enumerate(arms):
            pattern_reg = self.compile_expr(arm["pattern"])
            self.emit({"op": "jeq", "src1": match_reg, "src2": pattern_reg, "label": arm_labels[i]})

        self.emit({"op": "jmp", "label": nomatch_label})

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

        folded = self.try_eval_const(node["value"])
        if folded is None:
            raise Exception(f"[IRBuilderError] Global '{name}' initializer must be a constant for now")

        self.symbol_table[name] = f"global:{name}"
        self.global_symbols[name] = f"global:{name}"
        self.extern_symbols.discard(name)

        self.functions.append({"type": "Global", "name": name, "init": folded})

    # ----------------------------
    # Type inference helpers (bootstrap)
    # ----------------------------
    def infer_struct_name(self, expr):
        if len(self.structs) == 1:
            return next(iter(self.structs.keys()))
        return None


__all__ = ["IRBuilder"]
