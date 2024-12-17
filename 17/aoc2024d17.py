#!/usr/bin/env python
import sys
import os


class VM:
    def __init__(self, registers: tuple[int, int, int], program: list[int]):
        self.ra, self.rb, self.rc = registers
        self.program = program
        self.ip = 0

    def read_byte(self) -> int:
        self.ip += 1
        return self.program[self.ip - 1]

    def read_combo(self) -> int:
        args = {
            0: 0,
            1: 1,
            2: 2,
            3: 3,
            4: self.ra,
            5: self.rb,
            6: self.rc,
        }
        return args[self.read_byte()]

    def run(self):
        output = []
        self.ip = 0
        while self.ip < len(self.program):
            op = self.read_byte()

            # adv (division)
            if op == 0:
                arg = self.read_combo()
                n = self.ra
                d = 2**arg
                self.ra = n // d
            # bxl (xor)
            elif op == 1:
                arg = self.read_byte()
                self.rb ^= arg
            # bst (mod)
            elif op == 2:
                arg = self.read_combo()
                self.rb = arg & 0b111
            # jnz (conditional jump)
            elif op == 3:
                arg = self.read_byte()
                if self.ra != 0:
                    self.ip = arg
            # bxc (xor again)
            elif op == 4:
                arg = self.read_byte()  # ignored
                self.rb = self.rb ^ self.rc
            # out (output)
            elif op == 5:
                arg = self.read_combo()
                output.append(arg & 7)
            # bdv (other division)
            elif op == 6:
                arg = self.read_combo()
                n = self.ra
                d = 2**arg
                self.rb = n // d
            # cdv (other other division)
            elif op == 7:
                arg = self.read_combo()
                n = self.ra
                d = 2**arg
                self.rc = n // d
        return output


def parse_program(src: str) -> tuple[tuple, list[int]]:
    register_src, program_src = src.split("\n\n")
    program = [int(x) for x in program_src.split(": ")[1].split(",")]
    registers = []
    for line in register_src.split("\n"):
        registers.append(int(line.split(": ")[1]))
    return tuple(registers), program


def sneaky_program(a):
    out = []
    while a != 0:
        out.append(((((a & 7) ^ 5) ^ (a >> ((a & 7) ^ 5))) ^ 6) & 7)
        a = a >> 3
    return out


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read()

    registers, program = parse_program(data)
    vm = VM(registers, program)
    out = vm.run()
    print(",".join(str(x) for x in out))

    a = 1
    while True:
        out = sneaky_program(a)
        if len(out) < len(program):
            if out == program[-len(out):]:
                a <<= 3
            else:
                a += 1
        else:
            if out == program:
                print(f"Quine at {a}")
                break
            else:
                a += 1
