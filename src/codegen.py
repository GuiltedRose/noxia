import re

class CodeGenerator:
    def __init__(self):
        self.output = []
        self.rodata = []      # lines for section .rodata
        self.strings = {}     # python_str -> label
        self.str_count = 0

        # ✅ System V AMD64 integer arg registers
        # arg0..arg5 map to rdi,rsi,rdx,rcx,r8,r9
        self.fixed_map = {
            "arg0": "rdi",
            "arg1": "rsi",
            "arg2": "rdx",
            "arg3": "rcx",
            "arg4": "r8",
            "arg5": "r9",
            "ret": "rax",
        }

        self.low8_map = {
            "rax": "al",  "rbx": "bl",  "rcx": "cl",  "rdx": "dl",
            "rsi": "sil", "rdi": "dil", "rbp": "bpl", "rsp": "spl",
            "r8": "r8b",  "r9": "r9b",  "r10": "r10b", "r11": "r11b",
            "r12": "r12b","r13": "r13b","r14": "r14b","r15": "r15b",
        }

        self.tmp1 = "r11"
        self.tmp2 = "r10"

        self.slot_map = {}
        self.stack_size = 0

        # optional: prevent duplicate "global foo"
        self._globals_emitted = set()

    def emit(self, line):
        self.output.append(line)

    def is_imm(self, x) -> bool:
        return isinstance(x, str) and re.fullmatch(r"-?\d+", x) is not None

    def is_vreg(self, x) -> bool:
        return isinstance(x, str) and re.fullmatch(r"r\d+", x) is not None

    def is_phys_reg(self, x) -> bool:
        return isinstance(x, str) and re.fullmatch(r"[a-z][a-z0-9]{1,3}", x) is not None

    def is_global_ref(self, x) -> bool:
        return isinstance(x, str) and x.startswith("global:")

    def global_name(self, x: str) -> str:
        return x.split(":", 1)[1]

    # ----------------------------
    # String literal pooling
    # ----------------------------
    def intern_string(self, s: str) -> str:
        """
        Put a string into .rodata and return its label.
        Emits as: label: db <bytes>, 0
        """
        if s in self.strings:
            return self.strings[s]

        label = f"str_{self.str_count}"
        self.str_count += 1
        self.strings[s] = label

        b = s.encode("utf-8")
        byte_list = ", ".join(str(x) for x in b) + ", 0"
        self.rodata.append(f"{label}: db {byte_list}")
        return label

    # ----------------------------
    # Stack slot allocation
    # ----------------------------
    def alloc_slot(self, vreg: str) -> int:
        if vreg in self.slot_map:
            return self.slot_map[vreg]
        self.stack_size += 8
        self.slot_map[vreg] = self.stack_size
        return self.stack_size

    def stack_ref(self, vreg: str) -> str:
        off = self.alloc_slot(vreg)
        return f"qword [rbp - {off}]"

    # ----------------------------
    # Value load/store helpers
    # ----------------------------
    def load(self, value, into_reg: str) -> str:
        if value is None:
            raise Exception("Tried to load None")

        # immediate integer
        if self.is_imm(value):
            self.emit(f"mov {into_reg}, {value}")
            return into_reg

        # fixed names (args/ret)
        if value in self.fixed_map:
            phys = self.fixed_map[value]
            if phys != into_reg:
                self.emit(f"mov {into_reg}, {phys}")
            return into_reg

        # globals: load from memory
        if self.is_global_ref(value):
            sym = self.global_name(value)
            self.emit(f"mov {into_reg}, qword [rel {sym}]")
            return into_reg

        # vreg
        if self.is_vreg(value):
            self.emit(f"mov {into_reg}, {self.stack_ref(value)}")
            return into_reg

        # already phys reg -> copy if needed
        if self.is_phys_reg(value) and value != into_reg:
            self.emit(f"mov {into_reg}, {value}")
            return into_reg

        return value

    def store(self, dest, from_reg: str):
        if dest is None:
            return

        if dest in self.fixed_map:
            phys = self.fixed_map[dest]
            if phys != from_reg:
                self.emit(f"mov {phys}, {from_reg}")
            return

        if self.is_global_ref(dest):
            sym = self.global_name(dest)
            self.emit(f"mov qword [rel {sym}], {from_reg}")
            return

        if self.is_vreg(dest):
            self.emit(f"mov {self.stack_ref(dest)}, {from_reg}")
            return

        self.emit(f"mov {dest}, {from_reg}")

    # ----------------------------
    # Jump helpers
    # ----------------------------
    def jmp_near(self, label: str):
        self.emit(f"jmp near {label}")

    def jcc_near(self, cc: str, label: str):
        # cc like "e", "ne", "l", "le", "g", "ge"
        self.emit(f"j{cc} near {label}")

    # ----------------------------
    # Top-level codegen entrypoint
    # ----------------------------
    def generate(self, ir):
        self.output = []
        self.rodata = []
        self.strings = {}
        self.str_count = 0
        self._globals_emitted = set()

        globals_ir = []
        funcs_ir = []

        for item in ir:
            if not isinstance(item, dict):
                continue
            if item.get("type") == "Global":
                globals_ir.append(item)
            elif item.get("type") == "Function":
                funcs_ir.append(item)

        # ---- .data globals ----
        if globals_ir:
            self.emit("section .data")
            for g in globals_ir:
                name = g["name"]
                init = g.get("init", g.get("value", 0))
                if isinstance(init, str) and re.fullmatch(r"-?\d+", init):
                    init = int(init)
                if not isinstance(init, int):
                    raise Exception(f"Global '{name}' init must be int for now, got: {init!r}")
                self.emit(f"{name}: dq {init}")
            self.emit("")

        # ---- .text ----
        self.emit("section .text")
        for func in funcs_ir:
            self.generate_function(func)

        # ---- .rodata ----
        if self.rodata:
            self.emit("")
            self.emit("section .rodata")
            for line in self.rodata:
                self.emit(line)

        return "\n".join(self.output)

    def generate_function(self, func):
        self.slot_map = {}
        self.stack_size = 0

        name = func["name"]

        # avoid duplicate globals
        if name not in self._globals_emitted:
            self.emit(f"global {name}")
            self._globals_emitted.add(name)

        self.emit(f"{name}:")

        self.emit("push rbp")
        self.emit("mov rbp, rsp")

        body = func.get("body", [])
        for ins in body:
            for k in ("dest", "src", "src1", "src2", "base", "index", "value", "number"):
                if k in ins and self.is_vreg(ins[k]):
                    self.alloc_slot(ins[k])
            if "args" in ins:
                for a in ins["args"]:
                    if self.is_vreg(a):
                        self.alloc_slot(a)

        # 16-byte alignment: after push rbp => rsp is 8 mod 16
        frame = self.stack_size
        if frame % 16 != 8:
            frame += (8 - (frame % 16)) % 16
        self.stack_size = frame

        if self.stack_size:
            self.emit(f"sub rsp, {self.stack_size}")

        for instr in body:
            self.generate_instr(instr)

        if not body or body[-1].get("op") != "ret":
            self.emit("mov rsp, rbp")
            self.emit("pop rbp")
            self.emit("ret")

        self.emit("")

    # ----------------------------
    # Instruction selection
    # ----------------------------
    def generate_instr(self, instr):
        op = instr["op"]

        if op in ("eq", "ne", "lt", "le", "gt", "ge"):
            op = {"eq":"==","ne":"!=","lt":"<","le":"<=","gt":">","ge":">="}[op]

        if op == "and": op = "&"
        elif op == "or": op = "|"
        elif op == "xor": op = "^"
        elif op == "shl": op = "<<"
        elif op == "shr": op = ">>"
        if op == "mod": op = "%"

        # const
        if op == "const":
            v = instr["value"]

            # string literal => address in register
            if isinstance(v, str) and not self.is_imm(v):
                label = self.intern_string(v)
                self.emit(f"lea {self.tmp1}, [rel {label}]")
                self.store(instr["dest"], self.tmp1)
                return

            tmp = self.load(str(v), self.tmp1)
            self.store(instr["dest"], tmp)
            return

        # mov
        if op == "mov":
            tmp = self.load(instr["src"], self.tmp1)
            self.store(instr["dest"], tmp)
            return

        # load/store index (still qword*8 for now)
        if op == "load_index":
            base = self.load(instr["base"], self.tmp1)
            idx  = self.load(instr["index"], self.tmp2)
            if idx != "rcx":
                self.emit(f"mov rcx, {idx}")
            self.emit(f"mov {self.tmp1}, qword [{base} + rcx*8]")
            self.store(instr["dest"], self.tmp1)
            return

        if op == "store_index":
            base = self.load(instr["base"], self.tmp1)
            idx  = self.load(instr["index"], self.tmp2)
            val  = self.load(instr["value"], self.tmp2)
            if idx != "rcx":
                self.emit(f"mov rcx, {idx}")
            self.emit(f"mov qword [{base} + rcx*8], {val}")
            return

        # arithmetic / bitwise
        if op in ("add", "sub", "mul", "&", "|", "^"):
            a = self.load(instr["src1"], self.tmp1)
            b = self.load(instr["src2"], self.tmp2)

            if op == "add":
                self.emit(f"add {a}, {b}")
            elif op == "sub":
                self.emit(f"sub {a}, {b}")
            elif op == "mul":
                self.emit(f"imul {a}, {b}")
            elif op == "&":
                self.emit(f"and {a}, {b}")
            elif op == "|":
                self.emit(f"or {a}, {b}")
            elif op == "^":
                self.emit(f"xor {a}, {b}")

            self.store(instr["dest"], a)
            return

        if op in ("<<", ">>"):
            a = self.load(instr["src1"], self.tmp1)
            b = self.load(instr["src2"], self.tmp2)
            if b != "rcx":
                self.emit(f"mov rcx, {b}")
            if op == "<<":
                self.emit(f"shl {a}, cl")
            else:
                self.emit(f"sar {a}, cl")
            self.store(instr["dest"], a)
            return

        if op == "div" or op == "%":
            self.load(instr["src1"], "rax")
            self.emit("cqo")
            b = self.load(instr["src2"], self.tmp1)
            if b == "rdx":
                self.emit("mov rcx, rdx")
                b = "rcx"
            self.emit(f"idiv {b}")
            self.store(instr["dest"], "rax" if op == "div" else "rdx")
            return

        # comparisons
        if op in ("==", "!=", "<", "<=", ">", ">="):
            a = self.load(instr["src1"], self.tmp1)
            b = self.load(instr["src2"], self.tmp2)
            self.emit(f"cmp {a}, {b}")

            if "label" in instr:
                cc = {
                    "==":"e", "!=":"ne",
                    "<":"l", "<=":"le",
                    ">":"g", ">=":"ge",
                }[op]
                # ✅ force near to avoid "label changed during code generation"
                self.jcc_near(cc, instr["label"])
                return

            cc = {
                "==":"e", "!=":"ne",
                "<":"l", "<=":"le",
                ">":"g", ">=":"ge",
            }[op]
            self.emit(f"set{cc} al")
            self.emit(f"movzx {self.tmp1}, al")
            self.store(instr["dest"], self.tmp1)
            return

        # labels / jumps
        if op == "label":
            self.emit(f"{instr['name']}:")
            return

        if op == "jmp":
            # ✅ force near
            self.jmp_near(instr["label"])
            return

        if op == "jeq":
            a = self.load(instr["src1"], self.tmp1)
            b = self.load(instr["src2"], self.tmp2)
            self.emit(f"cmp {a}, {b}")
            # ✅ force near
            self.jcc_near("e", instr["label"])
            return

        # call
        if op == "call":
            arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
            for i, arg in enumerate(instr["args"]):
                if i >= len(arg_regs):
                    raise Exception("call: >6 args not supported yet")
                self.load(arg, arg_regs[i])
            self.emit(f"call {instr['func']}")
            self.store(instr["dest"], "rax")
            return

        # syscall
        if op == "syscall":
            self.load(instr["number"], "rax")
            arg_regs = ["rdi", "rsi", "rdx", "r10", "r8", "r9"]
            for i, a in enumerate(instr["args"]):
                if i >= len(arg_regs):
                    raise Exception("syscall: >6 args not supported yet")
                self.load(a, arg_regs[i])
            self.emit("syscall")
            self.store(instr["dest"], "rax")
            return

        # ret
        if op == "ret":
            v = instr.get("value")
            if v is not None:
                self.load(v, "rax")
            self.emit("mov rsp, rbp")
            self.emit("pop rbp")
            self.emit("ret")
            return

        # cast
        if op == "cast":
            tmp = self.load(instr["src"], self.tmp1)
            self.store(instr["dest"], tmp)
            return

        # gstore
        if op == "gstore":
            sym = instr["name"]
            val = self.load(instr["value"], self.tmp1)
            self.emit(f"mov qword [rel {sym}], {val}")
            return

        raise Exception(f"Unknown opcode: {op}")
