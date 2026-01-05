import os
import subprocess

from lexer import Lexer
from parser import Parser
from semantics import SemanticAnalyzer
from ir import IRBuilder
from codegen import CodeGenerator


class Compiler:
    def __init__(self):
        self.loaded = set()   # canonical module keys we’ve already loaded
        self.ast = []
        self.parse_failed = set()  # modules that had parse errors (skip downstream)

    # ---------- path normalization ----------

    def _normalize_import(self, path: str) -> str:
        """
        Normalize import strings into a canonical form we can reason about.
        - strips leading "./"
        """
        if not path:
            return path
        if path.startswith("./"):
            return path[2:]
        return path

    def _is_stdlib_import(self, norm: str) -> bool:
        return norm.startswith("stdlibnx/")

    def _import_to_file(self, norm: str) -> tuple[str, str]:
        """
        Convert normalized import path into:
        - full_path: filesystem path to .nx file
        - key: canonical module key for 'loaded' tracking
        """
        if self._is_stdlib_import(norm):
            rel = norm[len("stdlibnx/"):]  # e.g. "print"
            full_path = os.path.join("stdlibnx", rel) + ".nx"
            key = f"stdlibnx/{rel}"        # canonical key
            return full_path, key

        # src module
        full_path = os.path.join("src", norm) + ".nx"
        key = norm
        return full_path, key

    # ---------- module loading ----------

    def resolve_file(self, full_path: str, key: str):
        if key in self.loaded:
            return
        if key in self.parse_failed:
            return

        if not os.path.exists(full_path):
            raise FileNotFoundError(f"Module not found: {key}")

        print(f"[~] Resolving {full_path} as {key}")

        with open(full_path, "r", encoding="utf-8") as f:
            code = f.read()

        tokens = Lexer(code).tokenize()

        # Parser currently prints errors; we want a hard gate.
        parser = Parser(tokens)
        parsed = parser.parse()

        # Fail-fast if parser reported any errors.
        # The drop-in parser I gave you doesn’t store errors by default,
        # so we detect failure heuristically by checking for Error nodes.
        has_error_nodes = any(isinstance(n, dict) and n.get("type") == "Error" for n in parsed)

        if has_error_nodes:
            print(f"[!] Parse failed for {key}; skipping module (fix parser errors first).")
            self.parse_failed.add(key)
            return

        self.loaded.add(key)

        # Resolve imports first (on-demand stdlib)
        for node in parsed:
            if isinstance(node, dict) and node.get("type") == "ImportStatement":
                self.resolve_imports(node.get("path", "<error>"))

        # Keep non-import nodes
        self.ast.extend([n for n in parsed if isinstance(n, dict) and n.get("type") != "ImportStatement"])

    def resolve_imports(self, path: str):
        if path == "<error>":
            print("[!] Skipping unresolved import path '<error>'")
            return

        norm = self._normalize_import(path)
        full_path, key = self._import_to_file(norm)
        self.resolve_file(full_path, key)

    # ---------- compile pipeline ----------

    def compile(self, entry_path: str):
        # DO NOT preload the entire stdlib yet.
        # Load stdlib modules only when imported; this avoids cascading failures
        # from unimplemented stdlib syntax.
        entry = entry_path.replace(".nx", "")
        self.resolve_imports(entry)

        if self.parse_failed:
            print("[!] Compilation aborted due to parse errors in:")
            for m in sorted(self.parse_failed):
                print(f"    - {m}")
            return

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
        with open(asm_path, "w", encoding="utf-8") as f:
            f.write(asm)

        subprocess.run(["nasm", "-f", "elf64", asm_path, "-o", "bin/program.o"], check=False)
        subprocess.run(["gcc", "-nostdlib", "-static", "-o", "bin/program", "bin/program.o"], check=False)

        print("[✓] Build complete — binary at bin/program")


if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python3 compiler.py <file.nx>")
        raise SystemExit(1)

    entry_file = sys.argv[1]
    Compiler().compile(entry_file)
