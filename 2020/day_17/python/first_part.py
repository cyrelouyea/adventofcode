import itertools
from collections import defaultdict
from typing import Set, Dict, Tuple


Coordinate = Tuple[int, ...]

NB_DIM = 3
NEIGHBOURS_AND_ITSELF = set(itertools.product((-1, 0, 1), repeat=NB_DIM))
NEIGHBOURS = NEIGHBOURS_AND_ITSELF - {(0,) * NB_DIM}
NB_CYCLE = 6


def add_coordinate(c1: Coordinate, c2: Coordinate) -> Coordinate:
    return tuple(list(map(sum, zip(c1, c2))))


actives: Set[Coordinate] = set()

y = 0
entry = input()
while entry != '-':
    actives.update({
        (x, y,) + ((0,) * (NB_DIM - 2)) for x, c in enumerate(entry)
        if c == '#'
    })
    y += 1
    entry = input()

for _ in range(NB_CYCLE):
    to_check = {
        add_coordinate(coord, jump)
        for coord in actives
        for jump in NEIGHBOURS_AND_ITSELF
    }
    
    nb_active_neighbours: Dict[Coordinate, int] = defaultdict(lambda: 0)

    for coord in actives:
        for jump in NEIGHBOURS:
            nb_active_neighbours[add_coordinate(coord, jump)] += 1

    actives_to_remove: Set[Coordinate] = set()
    actives_to_add: Set[Coordinate] = set()
    for coord in to_check:
        if coord in actives:
            if not (2 <= nb_active_neighbours[coord] <= 3):
                actives_to_remove.add(coord)
        else:
            if nb_active_neighbours[coord] == 3:
                actives_to_add.add(coord)
    
    actives.update(actives_to_add)
    actives.difference_update(actives_to_remove)

print("solutions:", len(actives))