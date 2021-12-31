module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import qualified Data.List as L
import Data.Maybe
import Data.Ratio
import Text.Printf

newtype Matrix a =
    Matrix
        { getInner :: A.Array (Int, Int) a
        }

infixl 9 |.|

(|.|) :: Matrix a -> (Int, Int) -> a
(|.|) mat a = getInner mat A.! a

makeMatrix :: Int -> Int -> [a] -> Matrix a
makeMatrix n m =
    Matrix . A.array ((1, 1), (n, m)) . zip (liftA2 (,) [1 .. n] [1 .. m])

makeRationalMatrix :: Int -> Int -> [Int] -> Matrix Rational
makeRationalMatrix n m = makeMatrix n m . map toRational

printMatrix :: (Show a) => Int -> Matrix a -> String
printMatrix n mat@(Matrix inner)
    | n > maxRow = ""
    | otherwise =
        L.intercalate "   " (show . snd <$> getNth n) ++
        "\n" ++ printMatrix (n + 1) mat
  where
    (_, (maxRow, maxCol)) = A.bounds inner
    getNth i = filter ((== i) . fst . fst) (A.assocs inner)

instance (Show a) => Show (Matrix a) where
    show = printMatrix 1

infixl 7 ~*, ~/

infixl 6 ~+, ~-

class (Eq a) =>
      Field a
    where
    (~+) :: a -> a -> a
    (~*) :: a -> a -> a
    addIdent :: a
    mulIdent :: a
    addInv :: a -> a
    mulInv :: a -> a
    (~-) :: a -> a -> a
    x ~- y = x ~+ addInv y
    (~/) :: a -> a -> a
    x ~/ y = x ~/ mulInv y

instance (Integral a) => Field (Ratio a) where
    (~+) = (+)
    (~*) = (*)
    addIdent = 0 % 1
    mulIdent = 1 % 1
    addInv a = -a
    mulInv a = (1 % 1) / a
    (~-) = (-)
    (~/) = (/)

data ElemRowOp a
    = RowExchange Int Int
    | RowScalarMultiple Int a
    | RowAddition Int Int a
    deriving (Show)

doElemRowOp :: (Field a) => ElemRowOp a -> Matrix a -> Matrix a
doElemRowOp elem mat@(Matrix inner) =
    case elem of
        RowExchange i j ->
            let tmp = map snd $ filter ((== i) . fst . fst) (A.assocs inner)
             in Matrix $
                inner A.//
                [((i, k), num) | k <- [1 .. maxCol], let num = inner A.! (j, k)] A.//
                [((j, k), num) | (k, num) <- zip [1 .. maxCol] tmp]
        RowScalarMultiple i c ->
            Matrix $
            inner A.//
            [ (idx, c ~* num)
            | (idx, num) <- filter ((== i) . fst . fst) (A.assocs inner)
            ]
        RowAddition i j c ->
            let ithRow = filter ((== i) . fst . fst) (A.assocs inner)
                jthRowRaw =
                    map snd $ filter ((== j) . fst . fst) (A.assocs inner)
             in Matrix $
                inner A.//
                [ (fst (ithRow !! k), snd (ithRow !! k) ~+ c ~* jthRowRaw !! k)
                | k <- [0 .. maxCol - 1]
                ]
  where
    (_, (_, maxCol)) = A.bounds inner

data Oracle a =
    Oracle
        { getElemOps :: [ElemRowOp a]
        , getMatrix :: Matrix a
        , getMaxEntry :: (Int, Int)
        , getPivotLoc :: (Int, Int)
        , getLocation :: (Int, Int)
        , isHalt :: Bool
        }

instance (Show a) => Show (Oracle a) where
    show (Oracle elems mat matEntity pivotLoc loc halt) =
        "Oracle {\n    getElemOps = " ++
        show elems ++
        "\n    getMatrix = \n" ++
        show mat ++
        "    getMaxEntry = " ++
        show matEntity ++
        "\n    getPivotLoc = " ++
        show pivotLoc ++
        "\n    getLocation = " ++
        show loc ++ "\n    isHalt = " ++ show halt ++ "\n}"

matrixToOracle :: Matrix a -> Oracle a
matrixToOracle mat = Oracle [] mat (maxRow, maxCol) (1, 1) (1, 1) False
  where
    (_, (maxRow, maxCol)) = A.bounds . getInner $ mat

runOracle :: (Field a) => Oracle a -> Oracle a
runOracle =
    head .
    dropWhile (not . isHalt) .
    iterate (makePivotOneCol . changeIfNecessary . findColPivot)

findColPivot :: (Field a) => Oracle a -> Oracle a
findColPivot oracle@(Oracle _ _ _ _ _ True) = oracle
findColPivot oracle@(Oracle elem mat (maxRow, maxCol) (pivRow, _) (row, col) _)
    | col > maxCol = oracle {isHalt = True}
    | row > maxRow = findColPivot $ oracle {getLocation = (pivRow, col + 1)}
    | mat |.| getLocation oracle /= addIdent =
        oracle {getPivotLoc = (row, col), getLocation = (1, col)}
    | otherwise = findColPivot $ oracle {getLocation = (row + 1, col)}

changeIfNecessary :: (Field a) => Oracle a -> Oracle a
changeIfNecessary oracle@(Oracle _ _ _ _ _ True) = oracle
changeIfNecessary oracle@(Oracle elems mat _ (pivRow, pivCol) _ _) =
    if pivRow > pivCol
        then oracle
                 { getElemOps = elem : elems
                 , getMatrix = doElemRowOp elem mat
                 , getPivotLoc = (pivCol, pivCol)
                 , getLocation = (1, pivCol)
                 }
        else oracle
  where
    elem = RowExchange pivRow pivCol

makePivotOneCol :: (Field a) => Oracle a -> Oracle a
makePivotOneCol oracle@(Oracle _ _ _ _ _ True) = oracle
makePivotOneCol oracle@(Oracle elems mat (maxRow, maxCol) pivotIdx (row, col) _)
    | col > maxCol = oracle {isHalt = True}
    | row > maxRow = oracle
    | otherwise = makePivotOneColHelper 1 oracle

makePivotOneColHelper :: (Field a) => Int -> Oracle a -> Oracle a
makePivotOneColHelper _ oracle@(Oracle _ _ _ _ _ True) = oracle
makePivotOneColHelper n oracle@(Oracle elems mat (maxRow, _) pivotIdx@(pivRow, _) (_, col) _)
    | col /= snd pivotIdx = error "Internal error"
    | n > maxRow = nextCol oracle
    | n == pivRow =
        let elem = RowScalarMultiple n (mulInv $ mat |.| pivotIdx)
         in makePivotOneColHelper (n + 1) $
            oracle
                { getElemOps = elem : elems
                , getMatrix = doElemRowOp elem mat
                , getLocation = (n + 1, col)
                }
    | otherwise =
        let elem = RowAddition n pivRow (addInv (mat |.| (n, col)) ~* pivot)
         in makePivotOneColHelper (n + 1) $
            oracle
                { getElemOps = elem : elems
                , getMatrix = doElemRowOp elem mat
                , getLocation = (n + 1, col)
                }
  where
    pivot = mulInv $ mat |.| pivotIdx

nextCol :: Oracle a -> Oracle a
nextCol oracle
    | (snd . getMaxEntry $ oracle) < (snd . getPivotLoc $ oracle) =
        error "Index is out of scope"
    | otherwise =
        oracle
            { getPivotLoc = (pivRow + 1, pivCol + 1)
            , getLocation = (pivRow + 1, pivCol + 1)
            }
  where
    (pivRow, pivCol) = getPivotLoc oracle

mat :: Matrix Rational
mat =
    makeMatrix
        3
        4
        (concat
             [ [1 % 1, 1 % 2, -1 % 2, 3 % 2]
             , [0 % 1, 1 % 1, 7 % 5, -3 % 5]
             , [0 % 1, 0 % 1, 1 % 1, 1 % 1]
             ])

oracle :: Oracle Rational
oracle = matrixToOracle mat

foo :: (Field a) => Oracle a -> Oracle a
foo = makePivotOneCol . changeIfNecessary . findColPivot

-- TODO: implement IO
main :: IO ()
main = undefined
