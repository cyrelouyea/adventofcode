SUM_TO_FIND = 2020

entries = []

entry = int(input())
while entry != -1:
    entries.append(entry)
    entry = int(input())

len_entry = len(entries)
for i in range(len_entry):
    for j in range(i+1, len_entry):
        if entries[i] + entries[j] == SUM_TO_FIND:
            print("solution:", entries[i] * entries[j])
        


