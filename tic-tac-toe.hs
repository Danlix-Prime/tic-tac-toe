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
  show row = (show (firstSquare row)) ++ " | " ++ (show (secondSquare row)) ++ " | " ++ (show (thirdSquare row))

data Board = Board {
             firstRow  :: Row,
             secondRow :: Row,
             thirdRow  :: Row
           }

instance Show Board where
  show board = (show (firstRow board)) ++ "\n-   -   -\n" ++ (show (secondRow board)) ++ "\n-   -   -\n" ++ (show (thirdRow board))

blankBoard = Board (Row (EmptySquare 1) (EmptySquare 2) (EmptySquare 3)) 
                   (Row (EmptySquare 4) (EmptySquare 5) (EmptySquare 6)) 
                   (Row (EmptySquare 7) (EmptySquare 8) (EmptySquare 9))

squareByIndex :: Int -> Row -> Square
squareByIndex index row
              | index == 0 = firstSquare row
              | index == 1 = secondSquare row
              | index == 2 = thirdSquare row

rowByIndex :: Int -> Board -> Row
rowByIndex index board
           | index == 0 = firstRow board
           | index == 1 = secondRow board
           | index == 2 = thirdRow board

updateSquareInRow :: Row -> Int -> Square -> Row
updateSquareInRow (Row f s t) i v
                  | i == 0 = Row v s t
                  | i == 1 = Row f v t
                  | i == 2 = Row f s v

updateRowInBoard :: Board -> Int -> Row -> Board
updateRowInBoard (Board f s t) i v
                 | i == 0 = Board v s t
                 | i == 1 = Board f v t
                 | i == 2 = Board f s v

updateBoard :: Board -> Int -> Int -> Square -> Board
updateBoard board rowI colI square = updateRowInBoard board rowI row'
                                   where row' = updateSquareInRow (rowByIndex rowI board) colI square

indexToCoords :: Int -> (Int, Int)
indexToCoords int = ((int - (int `mod` 3)) `quot` 3, int `mod` 3)

swapPlayer :: Square -> Square
swapPlayer X = O
swapPlayer O = X
swapPlayer _ = error "Only Xs or Os"

loop :: Board -> Int -> Square -> IO ()
loop board turnNo player = do
  putStrLn $ (show board)
  str <- getLine
  if turnNo > 9
  then
    return ()
  else
    let
      number     = read str :: Int
      (row, col) = indexToCoords (number - 1)
      board'     = updateBoard board row col player
      player'    = swapPlayer player
      turn'      = turnNo + 1
    in
      loop board' turn' player'

main :: IO ()
main = loop blankBoard 0 X