import sys

class Solution:
    def __init__(self, size, column = 0):
        self.size = size
        self.board = []
        self.column = column

        for i in range(self.size):
            self.board += [[]]
            for j in range(self.size):
                self.board[i] += [0]
    
    def clone(self):
        newSolution = Solution(self.size, self.column)

        newSolution.board = list(self.board)
        for i in range(self.size):
            newSolution.board[i] = list(self.board[i])
        return newSolution
    
    def print(self):
        printBoard(self.board, self.size)

def printLine(n):
    for i in range(n):
        print("=", end="")
    print()

def printBoard(board, n):
    printLine(2*n-1)
    for i in range(n):
        for j in range(n):
            print(("%d" % board[i][j]).zfill(1), end=" ")
        print()
    printLine(2*n-1)

def permitido(sol, x, y):
    for i in range(sol.column):
        if(sol.board[x][i] == 1):
            return False

        yD  = y-i-1
        if(yD >= 0):
            xD1 = x-i-1
            xD2 = x+i+1
            
            if(xD1 >= 0 and sol.board[xD1][yD] == 1):
                return False

            if(xD2 < sol.size and sol.board[xD2][yD] == 1):
                return False

    return True

def acoes(sol):
    acoesList = []
    n = sol.size
    y = sol.column

    for x in range(n):
        if(permitido(sol, x, y)):
            acoesList += [[x, y]]
    return acoesList

def atingiuObjetivo(sol):
    return sol.column == sol.size

def resultado(sol, acao):
    x = acao[0]
    y = acao[1]
    newSol = sol.clone()

    newSol.board[x][y] = 1
    newSol.column = newSol.column+1

    return newSol

def dfs(sol):
    sol.print()
    for acao in acoes(sol):
        newSol = dfs(resultado(sol, acao))
        if atingiuObjetivo(newSol):
            return newSol
    return sol

    
def bfs(estados):
    for estado in estados:
        estado.print()

    estados2 = []
    for s in estados:
        s2 = [resultado(s,a) for a in acoes(s)]
        estados2 = estados2 + s2

    for estado in estados2:
        if(atingiuObjetivo(estado)):
            return estado

    return bfs(estados2)

if __name__ == '__main__':
    n   = int(sys.argv[2])
    sol = Solution(n)
    sol.print()

    if(sys.argv[1] == "dfs"):
        print("dfs")
        newSol = dfs(sol)
        newSol.print()
    else:
        print("bfs")
        newSol = bfs([sol])
        newSol.print()

