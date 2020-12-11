from typing import List, NamedTuple
from enum import Enum


class Seat(str, Enum):
    FLOOR = '.'
    EMPTY = 'L'
    OCCUPIED = '#'

    def __repr__(self) -> str:
        return self.value


class Location:

    def __init__(self, row: int, col: int):
        self._col = col
        self._row = row

    @property
    def col(self):
        return self._col

    @property
    def row(self):
        return self._row

    def __add__(self, location: 'Location') -> 'Location':
        return Location(row=self._row+location._row, col=self._col+location._col)

    def __mul__(self, scalar: int) -> 'Location':
        return Location(row=self._row*scalar, col=self._col*scalar)

class Ferry:

    ADJACENT_LOCATIONS = (
        Location(col=-1, row=0),
        Location(col=1, row=0),
        Location(col=0, row=-1),
        Location(col=0, row=1),
        Location(col=-1, row=1),
        Location(col=1, row=-1),
        Location(col=-1, row=-1),
        Location(col=1, row=1),
    )

    def __init__(self, layout: List[List[Seat]]):
        self._layout = [
            [Seat(el) for el in row] 
            for row in layout
        ]

    def nb_rows(self) -> int:
        return len(self._layout)

    def nb_columns(self, row: int) -> int:
        return len(self._layout[row])
    
    def get_nb_seat_type(self, seat_type: Seat) -> int:
        return sum(sum(1 for seat in row if seat == seat_type) for row in self._layout)

    def _get_seat(self, location: Location) -> Seat:
        return self._layout[location.row][location.col]

    def _get_adjacent_seats(self, location: Location) -> List[Seat]:
        adj_seats = []
        for adj_loc in self._get_adjacent_locations(location):
            i = 1
            new_location = location + adj_loc * i
            while self._is_location_within_limits(new_location) and self._get_seat(new_location) == Seat.FLOOR:
                i += 1
                new_location = location + adj_loc * i
            if self._is_location_within_limits(new_location):
                adj_seats.append(self._get_seat(new_location))
        return adj_seats

    def _get_adjacent_locations(self, location: Location) -> List[Location]:
        return [l for l in self.ADJACENT_LOCATIONS if self._is_location_within_limits(location + l)]

    def _is_location_within_limits(self, location: Location) -> bool:
        if location.row < 0 or location.row >= self.nb_rows():
            return False
        if location.col < 0 or location.col >= self.nb_columns(location.row):
            return False
        return True

    def __iter__(self):
        has_state_changed = True
        while has_state_changed:
            has_state_changed = False
            yield self._layout
            next_layout = [
                [el for el in row]
                for row in self._layout
            ]
            for row in range(self.nb_rows()):
                for col in range(self.nb_columns(row)):
                    location = Location(row=row, col=col)
                    seat = self._get_seat(location)
                    if seat == Seat.OCCUPIED:
                        continue
                    adj_seats = self._get_adjacent_seats(location)
                    if seat == Seat.EMPTY and adj_seats.count(Seat.OCCUPIED) == 0:
                        next_layout[location.row][location.col] = Seat.OCCUPIED
                        has_state_changed = True
                    elif seat == Seat.OCCUPIED and adj_seats.count(Seat.OCCUPIED) >= 5:
                        next_layout[location.row][location.col] = Seat.EMPTY
                        has_state_changed = True
            self._layout = next_layout



def parse_row(row: str) -> List[Seat]:
    return [Seat(el) for el in row]


layout: List[List[Seat]] = list()
row = input()
while row != '-':
    layout.append(parse_row(row))
    row = input()

ferry = Ferry(layout)
for _ in ferry: pass
print(ferry.get_nb_seat_type(Seat.OCCUPIED))