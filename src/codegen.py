from ir import IRBuilder

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
        else:
            raise Exception(f"Unknown opcode: {op}")
