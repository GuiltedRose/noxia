# noxia
This is my systems language (there's a lot of programming languages out there, but this one is mine)

##  <<< NOTICE: IF YOU USE NIXOS FOR THIS LANGUAGE >>>
compiler.py:
`subprocess.run(["gcc", "-nostdlib", "-static", "-o", "bin/program", "bin/program.o"])` (LINE 56)

GCC Should be replaced with clang. This will make the bootstrap process easier.
### NOTE: This entire section will be removed when I have a self-hosting language. (The compiler will be packaged in this repository.)
