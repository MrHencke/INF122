import Data.Char

--Oppgave A
gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep _ _ [] = []
gRep pr y [x] = if pr x then [y] else [x]
gRep pr y (x : xs) = if pr x then y : gRep pr y xs else x : gRep pr y xs

gRepMap :: (t -> Bool) -> t -> [t] -> [t]
gRepMap _ _ [] = []
gRepMap pr y xs = map (\x -> if pr x then y else x) xs

--Oppgave B
putBoard' :: Board -> IO ()
putBoard' board = putBoardHelper board 1

putBoardHelper :: Board -> Int -> IO ()
putBoardHelper [] _ = putStrLn "Your board is empty"
putBoardHelper [b] c = putRow c b
putBoardHelper (b : bs) c = do
  putRow c b
  putBoardHelper bs (c + 1)

--Oppgave D
--All changes are done inline in the play method.
play :: Board -> [Board] -> Int -> IO ()
play board state player = do
  let newState = state ++ [board]
  putChar '\n'
  putBoard' board
  if finished board
    then do
      putChar '\n'
      putStr "Player "
      putStr (show (next player))
      putStrLn " wins!!"
    else do
      putChar '\n'
      putStr "Player "
      print player
      row <- getDigit "Enter a row number, or 0 to undo: "
      if row == 0
        then do
          putStrLn "Undid last play"
          play (last state) (init state) (next player)
        else do
          num <- getDigit "Stars to remove: "
          if valid board row num
            then do
              play (move board row num) newState (next player)
            else do
              putChar '\n'
              putStrLn "ERROR: Invalid move"
              play board state player

-- Auxiliary functions for nim game.

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [6, 5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row -1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

nim :: IO ()
nim = play initial [] 1

playNim :: Board -> IO ()
playNim board = play board [] 1