import os
import glob
import subprocess
from lexer import Lexer
from parser import Parser
from semantics import SemanticAnalyzer
from ir import IRBuilder
from codegen import CodeGenerator

class Compiler:
    def __init__(self):
        self.loaded = set()
        self.ast = []

    def resolve_file(self, full_path, path_key):
        if path_key in self.loaded:
            return

        if not os.path.exists(full_path):
            raise FileNotFoundError(f"Module not found: {path_key}")

        print(f"[~] Resolving {full_path} as {path_key}")

        code = open(full_path).read()
        tokens = Lexer(code).tokenize()
        parsed = Parser(tokens).parse()

        self.loaded.add(path_key)

        for node in parsed:
            if node["type"] == "ImportStatement":
                self.resolve_imports(node["path"])

        self.ast.extend([n for n in parsed if n["type"] != "ImportStatement"])

    def resolve_imports(self, path):
        if path == "<error>":
            print("[!] Skipping unresolved import path '<error>'")
            return

        if path.startswith("./stdlibnx/"):
            full_path = os.path.join("stdlibnx", path[len("./stdlibnx/"):]) + ".nx"
            path_key = full_path.replace(".nx", "").replace("/", "_")
            self.resolve_file(full_path, path_key)
        else:
            file_path = os.path.join("src", path + ".nx")
            self.resolve_file(file_path, path)

    def resolve_stdlib(self):
        for stdlib_file in sorted(glob.glob("stdlibnx/*.nx")):
            mod_name = os.path.splitext(os.path.basename(stdlib_file))[0]
            import_path = f"./stdlibnx/{mod_name}"
            self.resolve_file(stdlib_file, import_path)

    def compile(self, entry_path):
        self.resolve_stdlib()  # Preload stdlibnx

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
        subprocess.run(["gcc", "-nostdlib", "-static", "-o", "bin/program", "bin/program.o"])

        print("[✓] Build complete — binary at bin/program")

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python3 compiler.py <file.nx>")
        exit(1)

    entry_file = sys.argv[1]
    Compiler().compile(entry_file)
