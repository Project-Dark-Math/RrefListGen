from fractions import Fraction
from random import randint, choice
from MatrixUtils import *
from itertools import product


def poolGen(r1, r2):
    return list(
        set(Fraction(a, b) for (a, b) in product(range(-r1, r1 + 1), range(1, r2 + 1)))
    )


pools = [poolGen(r1, r2) for r1, r2 in [(5, 1), (10, 3), (20, 10), (30, 20)]]


def generateInvertibleMatrix(n, pool):
    mat = [[choice(pool) for _ in range(n)] for _ in range(n)]
    if det(mat) == 0:
        return generateInvertibleMatrix(n, pool)
    else:
        return mat


def generateMatrixWithRank(n, m, rank, pool):
    mat = [[1 if i == j and i < rank else 0 for j in range(m)] for i in range(n)]
    A = generateInvertibleMatrix(n, pool)
    B = generateInvertibleMatrix(m, pool)
    mat = matmul(A, mat)
    mat = matmul(mat, B)
    return mat


n, m = randint(3, 6), randint(3, 6)
r, pool = randint(2, min(n, m)), choice(pools)

print(n, m)
printMatrix(generateMatrixWithRank(n, m, r, pool))
