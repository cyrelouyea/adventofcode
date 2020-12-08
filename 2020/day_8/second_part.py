from typing import List, NamedTuple, Set


class Instruction:

    def __init__(self, operation: str, argument: int):
        self._operation = operation
        self._argument = argument

    @property
    def operation(self) -> str:
        return self._operation

    @property
    def argument(self) -> int:
        return self._argument

    def __repr__(self) -> str:
        return f"(Instruction: operation='{self._operation}', argument={self.argument})"



class Instructions(List[Instruction]):
    pass


class ProgramState(NamedTuple):
    accumulator: int
    line: int
        

class Program:

    def __init__(self, instructions: Instructions):
        self._instructions = instructions

    @property
    def instructions(self) -> Instructions:
        return self._instructions


    def __iter__(self):
        state = ProgramState(accumulator=0, line=0)
        yield state

        while state.line < len(self._instructions):
            state = self._execute_instruction(self._instructions[state.line], state)
            yield state


    def _execute_instruction(self, instruction: Instruction, state: ProgramState) -> ProgramState:
        line = state.line
        accumulator = state.accumulator

        if instruction.operation == 'nop':
            line += 1
        elif instruction.operation == 'acc':
            line += 1
            accumulator += instruction.argument
        elif instruction.operation == 'jmp':
            line += instruction.argument

        return ProgramState(accumulator=accumulator, line=line)


def parse_instruction(entry: str) -> Instruction:
    splitted_entry = entry.split()
    return Instruction(operation=splitted_entry[0], argument=int(splitted_entry[1]))


def get_accumulator_after_end(program: Program) -> int:
    visited_lines: Set[int] = set()
    for state in program:
        if state.line in visited_lines:
            raise Exception('infinite loop')
        else:
            visited_lines.add(state.line)
    return state.accumulator


def find_and_get_accumulator_after_end(program: Program) -> int:
    swaps = {
        'nop': 'jmp',
        'jmp': 'nop'
    }

    instructions = program.instructions
    for line in range(len(instructions)):
        instruction = instructions[line]
        if instruction.operation in swaps:
            new_instructions = Instructions(instructions)
            new_instructions[line] = Instruction(operation=swaps[instruction.operation], argument=instruction.argument)
            try:
                return get_accumulator_after_end(Program(instructions=new_instructions))
            except:
                continue
    raise Exception('no program found')        
        

instructions = Instructions()
entry = input()
while entry != 'end':
    instructions.append(parse_instruction(entry))
    entry = input()

program = Program(instructions)

print("solution:", find_and_get_accumulator_after_end(program))