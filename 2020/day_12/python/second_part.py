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
    'N': lambda val, p, wp: (p, (wp[0], wp[1] - val)),
    'S': lambda val, p, wp: (p, (wp[0], wp[1] + val)),
    'E': lambda val, p, wp: (p, (wp[0] + val, wp[1])),
    'W': lambda val, p, wp: (p, (wp[0] - val, wp[1])),
    'L': lambda val, p, wp: (p, (wp[0] * l(val)[0][0] + wp[1] * l(val)[0][1], wp[0] * l(val)[1][0] + wp[1] * l(val)[1][1])),
    'R': lambda val, p, wp: (p, (wp[0] * r(val)[0][0] + wp[1] * r(val)[0][1], wp[0] * r(val)[1][0] + wp[1] * r(val)[1][1])),
    'F': lambda val, p, wp: ((p[0] + wp[0] * val, p[1] + wp[1] * val), wp),
}

pos = (0, 0)
waypoint = (10, -1)
instruction = input()
while instruction != '-':
    action = instruction[0]
    val = int(instruction[1:])
    pos, waypoint = actions[action](val, pos, waypoint)
    print(action, val, pos, waypoint)
    instruction = input()

print("solution:", pos, waypoint, abs(pos[0]) + abs(pos[1]))