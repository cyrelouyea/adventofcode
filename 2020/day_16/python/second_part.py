import math
import re
from typing import Dict, List, Tuple, Set

Ticket = List[int]
Rule = List[Tuple[int, int]]
Rules = Dict[str, Rule]

class PossibleFields:
    def __init__(self, ok: bool, fields: Set[str]):
        self.ok = ok
        self.fields = fields


RULE_PATTERN = re.compile(r'(?P<field>.+):\s(?P<f_min>\d+)\-(?P<f_max>\d+)\sor\s(?P<s_min>\d+)\-(?P<s_max>\d+)')
FIELDS_TO_MULTIPLY = 'departure'


def ticket_value_is_valid(value: int, rule: Rule) -> bool:
    return any(mini <= value and value <= maxi for mini, maxi in rule)


def ticket_is_valid(ticket: Ticket, rules: Rules) -> bool:
    return all(
        any(ticket_value_is_valid(value, rule) for rule in rules.values())
        for value in ticket
    )


def is_unique_and_not_ok(possible_fields: PossibleFields) -> bool:
    return len(possible_fields.fields) == 1 and not possible_fields.ok


rules: Rules = dict()
my_ticket: Ticket = list()
nearby_tickets: List[Ticket] = list()

# parse rules
entry = input()
while entry != '':
    match = re.match(RULE_PATTERN, entry)
    if match:
        rules[match.group('field')] = [
            (int(match.group('f_min')), int(match.group('f_max'))),
            (int(match.group('s_min')), int(match.group('s_max'))),
        ]
    else:
        raise Exception("parsing rules exceptions: " + entry)
    entry = input()

# parse my ticket
input()  # 'your ticket:'
entry = input()
my_ticket = [int(v) for v in entry.split(',')]
input()

# parse nearby tickets
input()  # 'nearby tickets'
entry = input()
while entry != '-':
    nearby_tickets.append([int(v) for v in entry.split(',')])
    entry = input()

# filter invalid ticket
valid_nearby_tickets = [
    ticket for ticket in nearby_tickets if ticket_is_valid(ticket, rules)
]

# keep track of possible fields for each position
possible_fields_for_pos = [
    PossibleFields(ok=False, fields=set(rules.keys())) for i in range(len(my_ticket))
]

# removing invalid possible fields for each position
for ticket in valid_nearby_tickets:
    for pos, value in enumerate(ticket):
        valid_fields = {field for field, rule in rules.items() if ticket_value_is_valid(value, rule)}
        possible_fields_for_pos[pos].fields.intersection_update(valid_fields)

# Get positions that contains only one possible field
unique_possible_fields_pos = [
    pos for pos in range(len(my_ticket)) 
    if is_unique_and_not_ok(possible_fields_for_pos[pos])
]

# removing these 'ok' field from others position
for pos in unique_possible_fields_pos:
    fields_to_remove = [(pos, next(iter(possible_fields_for_pos[pos].fields)))]
    while len(fields_to_remove) != 0:
        pos_to_remove, to_remove = fields_to_remove.pop()
        possible_fields_for_pos[pos_to_remove].ok = True
        for i, fields in enumerate(possible_fields_for_pos):
            if i != pos_to_remove:
                possible_fields_for_pos[i].fields.discard(to_remove)
                if is_unique_and_not_ok(possible_fields_for_pos[i]):
                    fields_to_remove.append((i, next(iter(possible_fields_for_pos[i].fields))))


print(
    "solution:",
    math.prod(
        value for pos, value in enumerate(my_ticket) 
        if next(iter(possible_fields_for_pos[pos].fields)).startswith(FIELDS_TO_MULTIPLY)
    )
)
