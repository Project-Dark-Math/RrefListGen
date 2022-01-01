from fractions import Fraction
from random import randint, choice
from MatrixUtils import *
from itertools import product, combinations, permutations
from RREF_Decomposition import *
from math import sqrt


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


def gen():
    n, m = randint(3, 6), randint(3, 6)
    r, pool = randint(2, min(n, m)), choice(pools[:1])

    print(n, m)
    printMatrix(generateMatrixWithRank(n, m, r, pool))


def generateMatrixWithIntegerRREF(n, m, rank, range1, range2, lb, ub):
    pivots = choice(list(combinations(range(m), rank)))

    def entry(i, j):
        if i < rank:
            if j == pivots[i]:
                return 1
            if j > pivots[i] and j not in pivots:
                return randint(-range1, range1)
            else:
                return 0
        else:
            return 0

    matrix = [[entry(i, j) for j in range(m)] for i in range(n)]

    if any([all(entry == 0 for entry in col) for col in transpose(matrix)]):
        return generateMatrixWithIntegerRREF(n, m, rank, range1, range2, lb, ub)

    def sqsum(row):
        return sum([x ** 2 for x in row])

    def l1(row):
        return sum(map(abs, row))

    while any(l1(row) < lb for row in matrix):
        newmat = list(map(list, matrix))

        if choice([True] * 7 + [False] * 3):
            i, j = choice([(i, j) for i, j in product(range(n), repeat=2) if i != j])
            c = choice([i for i in range(-range2, range2) if i != 0])
            RowAddition(i, j, c).applyTo(newmat)
            if sqsum(newmat[j]) < ub:
                matrix = newmat
        else:
            i = choice(list(range(n)))
            c = choice([i for i in range(-range2, range2) if i != 0])
            RowScalarMultiple(i, c).applyTo(newmat)
            if sqsum(newmat[i]) < ub:
                matrix = newmat

    perm = choice(list(permutations(range(n))))
    matrix = [matrix[perm[i]] for i in range(n)]

    return matrix


if __name__ == "__main__":
    # n, m, r, r1, r2, lb, ub = 8, 10, 6, 10, 3, 20, 2500
    n, m, r, r1, r2, lb, ub = 6, 8, 4, 5, 3, 16, 2500

    print(n, m)
    printMatrix(generateMatrixWithIntegerRREF(n, m, r, r1, r2, lb, ub))
