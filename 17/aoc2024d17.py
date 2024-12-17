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

    def run(self, target = None):
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
                output.append(arg & 0b111)
                if target is not None and target[:len(output)] != output:
                    return None
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
        if target is None or output == target:
            return output
        return None


def parse_program(src: str) -> tuple[tuple, list[int]]:
    register_src, program_src = src.split("\n\n")
    program = [int(x) for x in program_src.split(": ")[1].split(",")]
    registers = []
    for line in register_src.split("\n"):
        registers.append(int(line.split(": ")[1]))
    return tuple(registers), program


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

    a = 0
    while True:
        if a % 1000 == 0:
            print(a)
        vm.ra = a
        out = vm.run(program)
        if out is not None:
            print(f"Quine thing at {a}")
            break
        a += 1
