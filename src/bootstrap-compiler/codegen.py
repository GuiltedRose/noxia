# codegen.py (master) - ABI-safe temps + correct prologue/epilogue + _start safety
import re

class CodeGenerator:
    def __init__(self):
        self.output = []
        self.rodata = []      # lines for section .rodata
        self.strings = {}     # python_str -> label
        self.str_count = 0

        # System V AMD64 integer arg registers
        self.fixed_map = {
            "arg0": "rdi", "arg1": "rsi", "arg2": "rdx",
            "arg3": "rcx", "arg4": "r8",  "arg5": "r9",
            "ret": "rax",
        }

        self.low8_map = {
            "rax": "al",  "rbx": "bl",  "rcx": "cl",  "rdx": "dl",
            "rsi": "sil", "rdi": "dil", "rbp": "bpl", "rsp": "spl",
            "r8": "r8b",  "r9": "r9b",  "r10": "r10b", "r11": "r11b",
            "r12": "r12b","r13": "r13b","r14": "r14b","r15": "r15b",
        }

        # ------------------------------------------------------------
        # ABI FIX: use callee-saved temps so calls don't clobber them
        # ------------------------------------------------------------
        self.tmp1 = "r12"
        self.tmp2 = "r13"

        self.slot_map = {}
        self.stack_size = 0

        # fixed-size allocas reserved in the frame
        self.alloca_map = {}   # dest_vreg -> stack offset (bytes below rbp)

        self._globals_emitted = set()

        # global tracking
        self._used_globals = set()
        self._defined_data_globals = set()
        self._defined_bss_globals = set()
        self._defined_text_globals = set()

        # globals treated as ADDRESSABLE OBJECTS (struct/array base)
        self._addr_globals = set()

        # current function label prefix for IR labels
        self._fn_prefix = ""

        # current function name (for _start special-case)
        self._cur_func = ""

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

    # address-of global form: addr_global:NAME
    def is_addr_global(self, x) -> bool:
        return isinstance(x, str) and x.startswith("addr_global:")

    def addr_global_name(self, x: str) -> str:
        return x.split(":", 1)[1]

    # ----------------------------
    # Label mangling
    # ----------------------------
    def mangle_label(self, label: str) -> str:
        if not isinstance(label, str):
            return label
        if label == "":
            return label
        if label.startswith("fn_") and "__" in label:
            return label
        return f"{self._fn_prefix}{label}"

    # ----------------------------
    # String literal pooling
    # ----------------------------
    def intern_string(self, s: str) -> str:
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

        if self.is_imm(value):
            self.emit(f"mov {into_reg}, {value}")
            return into_reg

        # address-of global: always lea
        if self.is_addr_global(value):
            sym = self.addr_global_name(value)
            self._used_globals.add(sym)
            self.emit(f"lea {into_reg}, [rel {sym}]")
            return into_reg

        if value in self.fixed_map:
            phys = self.fixed_map[value]
            if phys != into_reg:
                self.emit(f"mov {into_reg}, {phys}")
            return into_reg

        if self.is_global_ref(value):
            sym = self.global_name(value)
            self._used_globals.add(sym)
            if sym in self._addr_globals or sym in self._defined_bss_globals:
                self.emit(f"lea {into_reg}, [rel {sym}]")
            else:
                self.emit(f"mov {into_reg}, qword [rel {sym}]")
            return into_reg

        if self.is_vreg(value):
            self.emit(f"mov {into_reg}, {self.stack_ref(value)}")
            return into_reg

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
            self._used_globals.add(sym)
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
        self.emit(f"jmp {self.mangle_label(label)}")

    def jcc_near(self, cc: str, label: str):
        self.emit(f"j{cc} {self.mangle_label(label)}")

    # ----------------------------
    # Pre-scan IR to find globals used as address bases
    # ----------------------------
    def scan_addr_globals(self, funcs_ir):
        self._addr_globals = set()
        base_ops = {
            "load_field", "store_field",
            "load_index", "store_index",
            "load_ptr_index", "store_ptr_index",
        }

        for func in funcs_ir:
            for ins in func.get("body", []):
                if not isinstance(ins, dict):
                    continue
                op = ins.get("op")
                if op in base_ops:
                    base = ins.get("base")
                    if self.is_global_ref(base):
                        self._addr_globals.add(self.global_name(base))

    # ----------------------------
    # Top-level codegen entrypoint
    # ----------------------------
    def generate(self, ir):
        self.output = []
        self.rodata = []
        self.strings = {}
        self.str_count = 0
        self._globals_emitted = set()

        self._used_globals = set()
        self._defined_data_globals = set()
        self._defined_bss_globals = set()
        self._defined_text_globals = set()
        self._fn_prefix = ""
        self._cur_func = ""

        globals_ir = []
        funcs_ir = []

        for item in ir:
            if not isinstance(item, dict):
                continue
            if item.get("type") == "Global":
                globals_ir.append(item)
            elif item.get("type") == "Function":
                funcs_ir.append(item)

        for f in funcs_ir:
            self._defined_text_globals.add(f["name"])

        self.scan_addr_globals(funcs_ir)

        data_lines = []
        bss_lines = []

        for g in globals_ir:
            name = g["name"]
            init = g.get("init", g.get("value", 0))
            if isinstance(init, str) and re.fullmatch(r"-?\d+", init):
                init = int(init)

            if name in self._addr_globals:
                self._defined_bss_globals.add(name)
                bss_lines.append(f"{name}: resb 256")
                continue

            if not isinstance(init, int):
                raise Exception(f"Global '{name}' init must be int for now, got: {init!r}")

            self._defined_data_globals.add(name)
            data_lines.append(f"{name}: dq {init}")

        if data_lines:
            self.emit("section .data")
            for line in data_lines:
                self.emit(line)
            self.emit("")

        self.emit("section .text")
        for func in funcs_ir:
            self.generate_function(func)

        unresolved = sorted(
            (self._used_globals - self._defined_data_globals - self._defined_bss_globals) - self._defined_text_globals
        )

        if bss_lines or unresolved:
            self.emit("")
            self.emit("section .bss")
            self.emit("align 8")
            for line in bss_lines:
                self.emit(line)
            for sym in unresolved:
                self.emit(f"{sym}: resb 256")

        if self.rodata:
            self.emit("")
            self.emit("section .rodata")
            for line in self.rodata:
                self.emit(line)

        return "\n".join(self.output)

    # ----------------------------
    # Function epilogue
    # ----------------------------
    def emit_epilogue_and_ret(self):
        # unwind locals first
        if self.stack_size:
            self.emit(f"add rsp, {self.stack_size}")
        # then restore frame + saved regs (reverse of prologue pushes)
        self.emit("pop rbp")
        self.emit("pop r13")
        self.emit("pop r12")
        self.emit("ret")

    def generate_function(self, func):
        self.slot_map = {}
        self.stack_size = 0
        self.alloca_map = {}

        name = func["name"]
        self._cur_func = name
        self._fn_prefix = f"fn_{name}__"

        if name not in self._globals_emitted:
            self.emit(f"global {name}")
            self._globals_emitted.add(name)

        self.emit(f"{name}:")

        # ------------------------------------------------------------
        # Prologue (order matters!)
        # Save callee-saved temps BEFORE establishing rbp,
        # so rbp-based argument offsets can be correct & stable.
        # ------------------------------------------------------------
        self.emit("push r12")
        self.emit("push r13")
        self.emit("push rbp")
        self.emit("mov rbp, rsp")

        # _start entry mapping:
        # After pushes above, rbp points at saved rbp.
        # [rbp+24] = argc, [rbp+32] = argv[0]
        if name == "_start":
            self.emit("mov rdi, qword [rbp + 24]")   # argc
            self.emit("lea rsi, [rbp + 32]")        # argv as &argv[0]
            self.emit("")

        body = func.get("body", [])

        # Pass 1: allocate stack slots (+ fixed-size allocas)
        for ins in body:
            for k in ("dest", "src", "src1", "src2", "base", "index", "value", "number"):
                if k in ins and self.is_vreg(ins[k]):
                    self.alloc_slot(ins[k])
            if "args" in ins:
                for a in ins["args"]:
                    if self.is_vreg(a):
                        self.alloc_slot(a)

            if ins.get("op") == "alloca":
                dest = ins["dest"]
                size = ins.get("size", 0)
                if isinstance(size, str) and self.is_imm(size):
                    size = int(size)
                if not isinstance(size, int) or size <= 0:
                    raise Exception(f"alloca: size must be positive int, got {size!r}")

                size_aligned = (size + 7) & ~7
                self.stack_size += size_aligned
                self.alloca_map[dest] = self.stack_size

        # ------------------------------------------------------------
        # SysV ABI: stack alignment
        # Goal: BEFORE any 'call', RSP % 16 == 8.
        # With our prologue (3 pushes = 24 bytes), we are at:
        #   entry rsp%16 == 8  (true at process entry and for normal calls)
        #   after 24 bytes push => still 8
        # So we must choose locals size so rsp%16 remains 8.
        # That means stack_size % 16 must be 0.
        # ------------------------------------------------------------
        frame = self.stack_size
        rem = frame % 16
        if rem != 0:
            frame += (16 - rem)
        self.stack_size = frame

        if self.stack_size:
            self.emit(f"sub rsp, {self.stack_size}")

        for instr in body:
            self.generate_instr(instr)

        # If no explicit ret, emit fallback.
        # _start must never return: call sys_exit(0) then ud2.
        if not body or body[-1].get("op") != "ret":
            if name == "_start":
                self.emit("mov rdi, 0")
                self.emit("call sys_exit")
                self.emit("ud2")
            else:
                self.emit_epilogue_and_ret()

        self.emit("")
        self._fn_prefix = ""
        self._cur_func = ""

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

        if op == "const":
            v = instr["value"]
            if isinstance(v, str) and not self.is_imm(v):
                label = self.intern_string(v)
                self.emit(f"lea {self.tmp1}, [rel {label}]")
                self.store(instr["dest"], self.tmp1)
                return
            tmp = self.load(str(v), self.tmp1)
            self.store(instr["dest"], tmp)
            return

        if op == "mov":
            tmp = self.load(instr["src"], self.tmp1)
            self.store(instr["dest"], tmp)
            return

        if op == "alloca":
            dest = instr["dest"]
            off = self.alloca_map.get(dest)
            if off is None:
                raise Exception(f"alloca: missing frame reservation for {dest}")
            self.emit(f"lea {self.tmp1}, [rbp - {off}]")
            self.store(dest, self.tmp1)
            return

        if op == "addr_local":
            src = instr["src"]
            if not self.is_vreg(src):
                raise Exception("addr_local: src must be vreg")
            off = self.alloc_slot(src)
            self.emit(f"lea {self.tmp1}, [rbp - {off}]")
            self.store(instr["dest"], self.tmp1)
            return

        if op == "neg":
            src = self.load(instr["src"], self.tmp1)
            self.emit(f"neg {src}")
            self.store(instr["dest"], src)
            return

        # ----------------------------
        # Indexing ops
        # ----------------------------
        if op == "load_index":
            base = self.load(instr["base"], self.tmp1)
            idx  = self.load(instr["index"], self.tmp2)
            if idx != "rcx":
                self.emit(f"mov rcx, {idx}")
            self.emit(f"movzx {self.tmp1}, byte [{base} + rcx]")
            self.store(instr["dest"], self.tmp1)
            return

        if op == "load_ptr_index":
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
            if idx != "rcx":
                self.emit(f"mov rcx, {idx}")
            val = self.load(instr["value"], self.tmp2)
            v8 = self.low8_map.get(val, None)
            if v8 is None:
                if val != self.tmp2:
                    self.emit(f"mov {self.tmp2}, {val}")
                v8 = self.low8_map[self.tmp2]
            self.emit(f"mov byte [{base} + rcx], {v8}")
            return

        if op == "store_ptr_index":
            base = self.load(instr["base"], self.tmp1)
            idx  = self.load(instr["index"], self.tmp2)
            if idx != "rcx":
                self.emit(f"mov rcx, {idx}")
            val = self.load(instr["value"], self.tmp2)
            self.emit(f"mov qword [{base} + rcx*8], {val}")
            return

        # ----------------------------
        # Struct field ops
        # ----------------------------
        if op == "load_field":
            base = self.load(instr["base"], self.tmp1)
            off = instr.get("offset", 0)
            if isinstance(off, str) and self.is_imm(off):
                off = int(off)
            if not isinstance(off, int):
                raise Exception(f"load_field: offset must be int, got {off!r}")
            if off == 0:
                self.emit(f"mov {self.tmp2}, qword [{base}]")
            else:
                self.emit(f"mov {self.tmp2}, qword [{base} + {off}]")
            self.store(instr["dest"], self.tmp2)
            return

        if op == "store_field":
            base = self.load(instr["base"], self.tmp1)
            off = instr.get("offset", 0)
            if isinstance(off, str) and self.is_imm(off):
                off = int(off)
            if not isinstance(off, int):
                raise Exception(f"store_field: offset must be int, got {off!r}")
            val = self.load(instr["value"], self.tmp2)
            if off == 0:
                self.emit(f"mov qword [{base}], {val}")
            else:
                self.emit(f"mov qword [{base} + {off}], {val}")
            return

        # ----------------------------
        # ALU ops
        # ----------------------------
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

        # ----------------------------
        # CFG / calls / misc
        # ----------------------------
        if op == "label":
            self.emit(f"{self.mangle_label(instr['name'])}:")
            return

        if op == "jmp":
            self.jmp_near(instr["label"])
            return

        if op == "jeq":
            a = self.load(instr["src1"], self.tmp1)
            b = self.load(instr["src2"], self.tmp2)
            self.emit(f"cmp {a}, {b}")
            self.jcc_near("e", instr["label"])
            return

        if op == "call":
            arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
            for i, arg in enumerate(instr["args"]):
                if i >= len(arg_regs):
                    raise Exception("call: >6 args not supported yet")
                self.load(arg, arg_regs[i])
            self.emit(f"call {instr['func']}")
            self.store(instr["dest"], "rax")
            return

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

        if op == "ret":
            v = instr.get("value")

            # _start must never return.
            if self._cur_func == "_start":
                if v is not None:
                    self.load(v, "rdi")
                else:
                    self.emit("mov rdi, 0")
                self.emit("call sys_exit")
                self.emit("ud2")
                return

            if v is not None:
                self.load(v, "rax")
            self.emit_epilogue_and_ret()
            return

        if op == "cast":
            tmp = self.load(instr["src"], self.tmp1)
            self.store(instr["dest"], tmp)
            return

        if op == "gstore":
            sym = instr["name"]
            self._used_globals.add(sym)
            val = self.load(instr["value"], self.tmp1)
            self.emit(f"mov qword [rel {sym}], {val}")
            return

        raise Exception(f"Unknown opcode: {op}")
