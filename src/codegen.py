class CodeGenerator:
    def __init__(self):
        self.output = []
        self.reg_map = {
            "r0": "rax", "r1": "rbx", "r2": "rcx", "r3": "rdx",
            "arg0": "rdi", "arg1": "rsi", "arg2": "rdx",
            "ret": "rax"
        }

    def map_reg(self, reg):
        return self.reg_map.get(reg, reg)

    def emit(self, line):
        self.output.append(line)

    def generate(self, ir):
        self.emit("section .text")
        for func in ir:
            self.generate_function(func)
        return "\n".join(self.output)

    def generate_function(self, func):
        name = func["name"]
        self.emit(f"global {name}")
        self.emit(f"{name}:")
        for instr in func["body"]:
            self.generate_instr(instr)
        self.emit("")  # spacing

    def generate_instr(self, instr):
        op = instr["op"]

        if op == "const":
            self.emit(f"mov {self.map_reg(instr['dest'])}, {instr['value']}")

        elif op == "mov":
            self.emit(f"mov {self.map_reg(instr['dest'])}, {self.map_reg(instr['src'])}")

        elif op == "add":
            self.emit(f"mov {self.map_reg(instr['dest'])}, {self.map_reg(instr['src1'])}")
            self.emit(f"add {self.map_reg(instr['dest'])}, {self.map_reg(instr['src2'])}")

        elif op == "sub":
            self.emit(f"mov {self.map_reg(instr['dest'])}, {self.map_reg(instr['src1'])}")
            self.emit(f"sub {self.map_reg(instr['dest'])}, {self.map_reg(instr['src2'])}")

        elif op == "mul":
            self.emit(f"mov {self.map_reg(instr['dest'])}, {self.map_reg(instr['src1'])}")
            self.emit(f"imul {self.map_reg(instr['dest'])}, {self.map_reg(instr['src2'])}")

        elif op == "ret":
            val = self.map_reg(instr["value"])
            if val != "rax":
                self.emit(f"mov rax, {val}")
            self.emit("ret")

        elif op == "call":
            for i, arg in enumerate(instr["args"]):
                self.emit(f"mov {self.map_reg(f'arg{i}')}, {self.map_reg(arg)}")
            self.emit(f"call {instr['func']}")
            self.emit(f"mov {self.map_reg(instr['dest'])}, rax")

        elif op == "label":
            self.emit(f"{instr['name']}:")

        elif op == "jmp":
            self.emit(f"jmp {instr['label']}")

        elif op == "jeq":
            self.emit(f"cmp {self.map_reg(instr['src1'])}, {self.map_reg(instr['src2'])}")
            self.emit(f"je {instr['label']}")

        elif op == "syscall":
            self.emit(f"mov rax, {self.map_reg(instr['number'])}")
            arg_regs = ["rdi", "rsi", "rdx", "r10", "r8", "r9"]
            for i, reg in enumerate(instr["args"]):
                self.emit(f"mov {arg_regs[i]}, {self.map_reg(reg)}")
            self.emit("syscall")
            self.emit(f"mov {self.map_reg(instr['dest'])}, rax")

        elif op == "cast":
            # Generic cast pseudo-instruction:
            # emit a comment for types, then just mov for now
            from_type = instr.get("from", "unknown")
            to_type = instr.get("to", "unknown")
            self.emit(f"; cast {from_type} -> {to_type}")
            self.emit(f"mov {self.map_reg(instr['dest'])}, {self.map_reg(instr['src'])}")

        else:
            raise Exception(f"Unknown opcode: {op}")

    def _generate_cast(self, instr):
        src = self.map_reg(instr["src"])
        dest = self.map_reg(instr["dest"])
        from_type = instr.get("from")  # optional, might not be present
        to_type = instr.get("to")

        # If from_type missing, fallback to simple mov
        if from_type is None or to_type is None:
            self.emit(f"mov {dest}, {src}")
            return

        # Pointer casts: just mov
        if from_type == "ptr" or to_type == "ptr":
            self.emit(f"mov {dest}, {src}")
            return

        # Integer widening/narrowing
        int_types = {"int8", "int16", "int32", "int64"}
        float_types = {"float", "double"}

        # Helper to get size in bits
        def size_in_bits(t):
            sizes = {
                "int8": 8,
                "int16": 16,
                "int32": 32,
                "int64": 64,
                "float": 32,
                "double": 64,
            }
            return sizes.get(t, 64)

        if from_type in int_types and to_type in int_types:
            from_bits = size_in_bits(from_type)
            to_bits = size_in_bits(to_type)

            if from_bits == to_bits:
                # Same size int: just mov
                self.emit(f"mov {dest}, {src}")
            elif from_bits < to_bits:
                # Widening - use movsx (sign extend) or movzx (zero extend)
                # For now, assume signed extension
                if from_bits == 8:
                    self.emit(f"movsx {dest}, byte {src}")
                elif from_bits == 16:
                    self.emit(f"movsx {dest}, word {src}")
                elif from_bits == 32:
                    # movsx for 32 -> 64 bit
                    self.emit(f"movsxd {dest}, {src}")
                else:
                    self.emit(f"mov {dest}, {src}")  # fallback
            else:
                # Narrowing - just mov; upper bits ignored
                self.emit(f"mov {dest}, {src}")

            return

        # Int to float/double
        if from_type in int_types and to_type in float_types:
            # Use cvtsi2ss (int->float) or cvtsi2sd (int->double)
            if to_type == "float":
                # mov src to eax first if needed (cvtsi2ss uses eax)
                if src != "rax":
                    self.emit(f"mov rax, {src}")
                self.emit(f"cvtsi2ss xmm0, rax")
                self.emit(f"movaps {dest}, xmm0")  # store xmm0 to dest register (assume dest is xmm)
            else:
                if src != "rax":
                    self.emit(f"mov rax, {src}")
                self.emit(f"cvtsi2sd xmm0, rax")
                self.emit(f"movaps {dest}, xmm0")
            return

        # Float/double to int
        if from_type in float_types and to_type in int_types:
            # Use cvttss2si or cvttsd2si
            if from_type == "float":
                self.emit(f"cvttss2si {dest}, xmm0")
            else:
                self.emit(f"cvttsd2si {dest}, xmm0")
            return

        # Float to double or double to float
        if from_type == "float" and to_type == "double":
            self.emit(f"cvtss2sd xmm0, xmm0")
            self.emit(f"movaps {dest}, xmm0")
            return
        elif from_type == "double" and to_type == "float":
            self.emit(f"cvtsd2ss xmm0, xmm0")
            self.emit(f"movaps {dest}, xmm0")
            return

        # Fallback: just mov
        self.emit(f"mov {dest}, {src}")
