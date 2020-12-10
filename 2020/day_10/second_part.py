from typing import List

adapters: List[int] = [0]
adapter = int(input())
while adapter != -1:
    adapters.append(adapter)
    adapter = int(input())
adapters.append(max(adapters) + 3)
adapters.sort()

nb_arrangements = [0] * len(adapters)
nb_arrangements[-1] = 1

for i in range(len(adapters)-1, -1, -1):
    for j in (1,2,3,):
        if i-j >= 0 and adapters[i-j] + 3 >= adapters[i]:
            nb_arrangements[i-j] += nb_arrangements[i]

print("solutions:", nb_arrangements[0])

