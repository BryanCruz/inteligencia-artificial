n = 4

def feasible(st):
    for i in range(n):
        countV = 0
        countH = 0
        for j in range(n):
            countV += st[i][j]
            countH += st[i][j]
        if countV > 1 or countH > 1:
            return False
    return True

def isGoal(st):
    return len(st[0]) == 8 and feasible(st)

def bfs(sts):
    goal = list(filter(isGoal, sts))
    while len(goal) == 0:
        sts  = list(filter(feasible, move(sts)))
        goal = list(filter(isGoal, sts))
    return goal[0]
