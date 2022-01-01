from RREF_Decomposition import *
from MatrixUtils import *
from fractions import Fraction
from sys import stdin

readline = stdin.readline


def main():
    n, m = map(int, readline().split())
    matrix = [[Fraction(x) for x in readline().strip().split()] for _ in range(n)]
    opList, RREF = GaussJordanDecomposition(matrix)
    printOps(opList)
    printMatrix(RREF)


if __name__ == "__main__":
    main()
