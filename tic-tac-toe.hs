import Control.Monad

data Square = EmptySquare Int
            | X
            | O
            deriving (Eq, Read)

instance Show Square where
  show (EmptySquare int) = show int
  show X                 = "X"
  show O                 = "O"

data Row = Row {
            firstSquare  :: Square,
            secondSquare :: Square,
            thirdSquare  :: Square
         }

instance Show Row where
  show row = show (firstSquare row) ++ " | " ++ show (secondSquare row) ++ " | " ++ show (thirdSquare row)

data Board = Board {
             firstRow  :: Row,
             secondRow :: Row,
             thirdRow  :: Row
           }

instance Show Board where
  show board = show (firstRow board) ++ "\n-   -   -\n" ++ show (secondRow board) ++ "\n-   -   -\n" ++ show (thirdRow board)

blankBoard :: Board
blankBoard = Board (Row (EmptySquare 1) (EmptySquare 2) (EmptySquare 3)) 
                   (Row (EmptySquare 4) (EmptySquare 5) (EmptySquare 6)) 
                   (Row (EmptySquare 7) (EmptySquare 8) (EmptySquare 9))

squareByIndex :: Int -> Row -> Square
squareByIndex index row
              | index == 0 = firstSquare row
              | index == 1 = secondSquare row
              | index == 2 = thirdSquare row
squareByIndex _ _        = error "Rows only have three squares"

rowByIndex :: Board -> Int -> Row
rowByIndex board index
           | index == 0 = firstRow board
           | index == 1 = secondRow board
           | index == 2 = thirdRow board
rowByIndex _ _        = error "Boards only have three rows"

squareByCoords :: Board -> (Int, Int) -> Square
squareByCoords board (rowI, colI) = let row = rowByIndex board rowI
                                    in squareByIndex colI row

updateSquareInRow :: Row -> Int -> Square -> Row
updateSquareInRow (Row f s t) i v
                  | i == 0 = Row v s t
                  | i == 1 = Row f v t
                  | i == 2 = Row f s v
updateSquareInRow _ _ _ = error "Rows only have three squares"

updateRowInBoard :: Board -> Int -> Row -> Board
updateRowInBoard (Board f s t) i v
                 | i == 0 = Board v s t
                 | i == 1 = Board f v t
                 | i == 2 = Board f s v
updateRowInBoard _ _ _ = error "Boards only have three rows"

updateBoard :: Board -> Int -> Int -> Square -> Board
updateBoard board rowI colI square = updateRowInBoard board rowI row'
                                   where row' = updateSquareInRow (rowByIndex board rowI) colI square

indexToCoords :: Int -> (Int, Int)
indexToCoords int = ((int - (int `mod` 3)) `quot` 3, int `mod` 3)

swapPlayer :: Square -> Square
swapPlayer X = O
swapPlayer O = X
swapPlayer _ = error "Only Xs or Os"

checkRow :: Row -> Bool
checkRow (Row f s t) = (f == s) && (s == t)

checkColumn :: Board -> Int -> Bool
checkColumn board col = let (f:s:t:[]) = map (squareByIndex col . rowByIndex board) [0..2]
                        in  (f == s) && (s == t)

checkDiagonals :: Board -> Bool
checkDiagonals board = let lookUp = squareByCoords board
                           (f:s:t:[]) = map lookUp [(0, 0), (1, 1), (2, 2)]
                           (f':_:t':[]) = map lookUp [(0, 2), (1, 1), (2, 0)]
                       in ((f == s) && (s == t)) || ((f' == s) && (s == t'))

checkRows :: Board -> Bool
checkRows board = any checkRow $ map (rowByIndex board) [0..2]

checkColumns :: Board -> Bool
checkColumns board = any (checkColumn board) [0..2]

checkBoard :: Board -> Bool
checkBoard board = checkColumns board || checkRows board || checkDiagonals board

loop :: Board -> Int -> Square -> IO ()
loop board turnNo player = do
  print board
  str <- getLine
  Control.Monad.unless (turnNo >= 8) $
    let number = read str :: Int
        (row, col) = indexToCoords (number - 1)
        board' = updateBoard board row col player
        victory = checkBoard board'
        player' = swapPlayer player
        turn' = turnNo + 1
      in
        if victory then
        putStrLn $ show board' ++ "\n" ++ show player ++ " Wins!" else
        loop board' turn' player'

main :: IO ()
main = loop blankBoard 0 X