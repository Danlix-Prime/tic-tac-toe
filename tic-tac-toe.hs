data Square = EmptySquare Int
            | X
            | O
            deriving (Show, Eq)

data Row = Row {
            firstSquare  :: Square,
            secondSquare :: Square,
            thirdSquare  :: Square
         } deriving (Show)

data Board = Board {
             firstRow  :: Row,
             secondRow :: Row,
             thirdRow  :: Row
           } deriving (Show)

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
updateBoard board rowI colI square = let row' = updateSquareInRow (rowByIndex rowI board) colI square
                                     in case rowI of
                                       0 -> Board row'             (secondRow board) (thirdRow board)
                                       1 -> Board (firstRow board) row'              (thirdRow board)
                                       2 -> Board (firstRow board) (secondRow board) row'

--parseInput :: String -> (Int, Int, Maybe Square)
--parseInput str = (row', col', square')
--               where [row:col:square:[]] = words str
--                     row'                = read row :: Int
--                     col'                = read col :: Int
--                     square'             = case square of
--                                             "x"       -> Just X
--                                             "o"       -> Just O
--                                             otherwise -> Nothing
