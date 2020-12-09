from collections import deque
from itertools import combinations
from typing import Deque

PREAMBLE_LENGTH = 25

data: Deque[int] = deque()
for i in range(PREAMBLE_LENGTH):
    data.append(int(input()))


while True:
    new_number = int(input())
    for combination in combinations(data, 2):
        if sum(combination) == new_number:
            data.popleft()
            data.append(new_number)
            break
    else:
        print("solution:", new_number)
        break