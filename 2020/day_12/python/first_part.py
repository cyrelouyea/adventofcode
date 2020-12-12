cos = {
    0: 1,
    90: 0,
    180: -1,
    270: 0
}

sin = {
    0: 0,
    90: 1,
    180: 0,
    270: -1
}

def r(deg):
    return (
        (cos[deg], -sin[deg]),
        (sin[deg], cos[deg]),
    )

def l(deg):
    return r(360 - deg)

actions = {
    'N': lambda val, p, d: ((p[0], p[1] - val), d),
    'S': lambda val, p, d: ((p[0], p[1] + val), d),
    'E': lambda val, p, d: ((p[0] + val, p[1]), d),
    'W': lambda val, p, d: ((p[0] - val, p[1]), d),
    'L': lambda val, p, d: (p, (d[0] * l(val)[0][0] + d[1] * l(val)[0][1], d[0] * l(val)[1][0] + d[1] * l(val)[1][1])),
    'R': lambda val, p, d: (p, (d[0] * r(val)[0][0] + d[1] * r(val)[0][1], d[0] * r(val)[1][0] + d[1] * r(val)[1][1])),
    'F': lambda val, p, d: ((p[0] + d[0] * val, p[1] + d[1] * val), d),
}

pos = (0, 0)
direction = (1, 0)
instruction = input()
while instruction != '-':
    action = instruction[0]
    val = int(instruction[1:])
    pos, direction = actions[action](val, pos, direction)
    print(action, val, pos, direction)
    instruction = input()

print("solution:", abs(pos[0]) + abs(pos[1]))