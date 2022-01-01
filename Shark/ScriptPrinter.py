from MatrixGenerator import *
from RREF_Decomposition import *
from MatrixUtils import *
from fractions import Fraction

def ScriptPrinter():
  n, m, r, r1, r2, lb, ub = 6, 8, 4, 5, 3, 16, 2500
  matrix = generateMatrixWithIntegerRREF(n, m, r, r1, r2, lb, ub)
  matrix = [list(map(Fraction,row)) for row in matrix]

  opList, rref = GaussJordanDecomposition(matrix)

  print(n,m)
  printMatrix(matrix)

  for op in opList:
    print()
    print(op)
    op.applyTo(matrix)
    printMatrix(matrix)

if __name__ == '__main__':
  ScriptPrinter()
