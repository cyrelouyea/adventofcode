from collections import deque
from itertools import combinations
from typing import Deque, List

PREAMBLE_LENGTH = 25

invalid_number = None
list_numbers: List[int] = list()
data: Deque[int] = deque()
for i in range(PREAMBLE_LENGTH):
    new_number = int(input())
    data.append(new_number)
    list_numbers.append(new_number)

new_number = int(input())
while new_number != -1:
    for combination in combinations(data, 2):
        if sum(combination) == new_number:
            data.popleft()
            data.append(new_number)
            list_numbers.append(new_number)
            break
    else:
        invalid_number = new_number 
        break
    new_number = int(input())

if new_number == -1:
    raise Exception("no invalid number found")

while new_number != -1:
    new_number = int(input())
    list_numbers.append(new_number)

for first_index, last_index in combinations(range(len(list_numbers)), 2):
    contiguous_set = list_numbers[first_index:last_index+1]
    if sum(contiguous_set) == invalid_number:
        print("solution:", min(contiguous_set) + max(contiguous_set))
        break
else:
    raise Exception("no set found")