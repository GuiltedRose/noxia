import os
from lexer import Lexer
from parser import Parser
from semantics import SemanticAnalyzer
from ir import IRBuilder
from codegen import CodeGenerator
import subprocess

class Compiler:
    def __init__(self):
        self.loaded = set()
        self.ast = []

    def resolve_imports(self, path):
        if path in self.loaded:
            return

        full_path = os.path.join("src", path + ".nx")
        if not os.path.exists(full_path):
            raise FileNotFoundError(f"Module not found: {path}")

        code = open(full_path).read()
        tokens = Lexer(code).tokenize()
        parsed = Parser(tokens).parse()

        self.loaded.add(path)

        for node in parsed:
            if node["type"] == "ImportStatement":
                self.resolve_imports(node["path"])

        self.ast.extend([n for n in parsed if n["type"] != "ImportStatement"])

    def compile(self, entry_path):
        entry = entry_path.replace(".nx", "")
        self.resolve_imports(entry)

        print(f"[*] Parsed {len(self.ast)} top-level nodes.")

        analyzer = SemanticAnalyzer(self.ast)
        errors = analyzer.analyze()
        if errors:
            for err in errors:
                print(err)
            return

        ir = IRBuilder().build(self.ast)
        asm = CodeGenerator().generate(ir)

        os.makedirs("bin", exist_ok=True)
        asm_path = "bin/program.asm"
        with open(asm_path, "w") as f:
            f.write(asm)

        subprocess.run(["nasm", "-f", "elf64", asm_path, "-o", "bin/program.o"])
        subprocess.run(["gcc", "bin/program.o", "-o", "bin/program"])

        print("[✓] Build complete — binary at bin/program")
