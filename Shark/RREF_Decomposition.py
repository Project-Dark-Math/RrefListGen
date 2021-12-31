from fractions import Fraction
from itertools import product
from copy import deepcopy

class ElementaryRowOps():
  pass

class RowExchange(ElementaryRowOps):
  def __init__(self, i, j):
    self.i = i
    self.j = j
  def __str__(self):
    return f"RowExchange {self.i} {self.j}"
  def __repr__(self):
    return str(self)
  def toMatrix(self, n):
    matrix = [[int(i==j) for j in range(n)] for i in range(n)]
    for x,y in product([self.i,self.j], repeat = 2):
      matrix[x][y] = int(x!=y)
    return matrix
  def applyTo(self, matrix):
    matrix[self.i], matrix[self.j] = matrix[self.j], matrix[self.i]

class RowScalarMultiple(ElementaryRowOps):
  def __init__(self, i, c):
    self.i = i
    self.c = c
  def __str__(self):
    return f"RowScalarMultiple {self.i + 1} {self.c}"
  def __repr__(self):
    return str(self)
  def toMatrix(self, n):
    matrix = [[int(i==j) for j in range(n)] for i in range(n)]
    matrix[self.i][self.i] = self.c
    return matrix
  def applyTo(self, matrix):
    matrix[self.i] = [entry * self.c for entry in matrix[self.i]]

class RowAddition(ElementaryRowOps):
  def __init__(self, i, j, c):
    self.i = i
    self.j = j
    self.c = c
  def __str__(self):
    return f"RowAddition {self.i + 1} {self.j + 1} {self.c}"
  def __repr__(self):
    return str(self)
  def toMatrix(self, n):
    matrix = [[int(i==j) for j in range(n)] for i in range(n)]
    matrix[self.j][self.i] = self.c
    return matrix
  def applyTo(self, matrix):
    matrix[self.j] = [x * self.c + y for x,y in zip(matrix[self.i], matrix[self.j])]

def simpleElimination(column, rank):
  if column[rank] == 1:
    if all(x == 0 for x in column[rank+1:]):
      return []
    else:
      return [RowAddition(rank, i, -x) for i,x in enumerate(column) if i > rank and x != 0]
  else:
    if column[rank] != 0:
      newcol = deepcopy(column)
      newcol[rank] = 1
      return [RowScalarMultiple(rank, 1/column[rank])] + simpleElimination(newcol,rank)
    else:
      i = [i for i,x in enumerate(column) if i > rank and x != 0][0]
      newcol = deepcopy(column)
      newcol[rank], newcol[i] = newcol[i], newcol[rank]
      return [RowExchange(rank,i)] + simpleElimination(newcol,rank)

def GaussDecomposition(matrix):
  matrix = deepcopy(matrix)
  n,m = len(matrix), len(matrix[0])
  rank = 0
  opList = []

  for j in range(m):
    col_j = [matrix[i][j] for i in range(n)]
    if all(x == 0 for x in col_j[rank:]):
      continue
    else:
      newOps = simpleElimination(col_j, rank)
      for op in newOps:
        op.applyTo(matrix)
      opList.extend(newOps)
      rank += 1

  return opList, matrix

def GaussJordanDecomposition(matrix):
  opList, matrix = GaussDecomposition(matrix)
  n,m = len(matrix), len(matrix[0])
  
  for row in matrix:
    if all(x == 0 for x in row):
      return opList, matrix
    else:
      j = row.index(1)
      col = [matrix[i][j] for i in range(n)]
      newOps = [RowAddition(j,i,-x) for i,x in enumerate(col) if i < j and x != 0]
      for op in newOps:
        op.applyTo(matrix)
      opList.extend(newOps)

  return opList, matrix
