from RREF_Decomposition import GaussDecomposition


def printMatrix(matrix):
    for row in matrix:
        print(" ".join(map(str, row)))


def printOps(opList):
    for op in opList:
        print(op)


def identityMatrix(n):
    return [[int(i == j) for j in range(n)] for i in range(n)]


def transpose(A):
    return list(map(list, zip(*A)))


def matmul(A, B):
    return [[sum(a * b for a, b in zip(row, col)) for col in transpose(B)] for row in A]


def getRREF(matrix):
    return GaussDecomposition(matrix)[1]


def det(A):
    n = len(A)
    rref = getRREF(A)
    det = 1
    for i in range(n):
        det *= rref[i][i]
    return det
