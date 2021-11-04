module Main where

import Data.Char (isDigit)

type Board = [[Int]]

type State = [Board]

type MSG = (String, String) -- Color code + message

main :: IO ()
main = gameLoop [] [] 0 ("", "")

gameLoop :: Board -> State -> Int -> MSG -> IO ()
gameLoop [] state nm msg = do
  -- Initial game loop, a player can either start a game or quit from here
  clr
  putStrLn "Welcome to Towers of Hanoi!"
  putStrLn "List all available commands with: \"help\""
  drawMessage msg
  putStrLn "Start a new game with: b <Number Of Rings> or quit with: q"
  cmd <- promptLine ""
  case words cmd of
    ["b"] -> gameLoop [] state nm ("R", "You have to provide a number for this command")
    ["b", n] -> newGame [] n state nm
    ["help"] -> do
      clr
      help
      gameLoop [] state nm ("", "")
    ["q"] -> return ()
    ["q", _] -> gameLoop [] state nm ("R", "This command only contains a single letter, \"q\", try again.")
    _ -> gameLoop [] state nm ("R", "You have to initialize the game to input other commands than b and q")
gameLoop [[], [], xs] _ nm _ = do
  -- Game loop when a player has won, they may return to the initial loop from here
  clr
  winMessage xs nm
  gameLoop [] [] 0 ("G", "You've won, but can you do it again?")
gameLoop board state nm msg = do
  -- Main game loop, where the game is played
  clr
  drawTowers board
  drawMoves nm
  drawMessage msg
  cmd <- promptLine ""
  case words cmd of
    ["b"] -> gameLoop board state nm ("R", "You have to provide a number for this command")
    ["b", n] -> newGame board n state nm
    ["z"] -> gameLoop board state nm ("R", "You have to provide a number for this command")
    ["z", n] ->
      if isNumber n
        then do
          let num = read n :: Int
          if num > nm
            then gameLoop (state !! nm) [state !! nm] 0 ("Y", "You can't go back further than your moves \nTaking you back to the starting position")
            else do
              gameLoop (state !! num) (drop num state) (nm - num) ("G", "Regretting: " ++ n ++ " moves")
        else gameLoop board state nm ("R", "Please input a valid number")
    ["help"] -> do
      clr
      help
      gameLoop board state nm ("", "")
    ["q"] -> return ()
    ["q", _] -> gameLoop board state nm ("R", "This command only contains a single letter, \"q\", try again.")
    ["h"] -> gameLoop board state nm ("R", "This command has not been implemented.")
    [f, t] ->
      if isNumber f && isNumber t
        then do
          if legalMove board (read f :: Int) (read t :: Int)
            then do
              let newBoard = move board (read f :: Int) (read t :: Int)
              gameLoop newBoard (newBoard : state) (nm + 1) ("", "")
            else gameLoop board state nm ("R", "That move is illegal")
        else gameLoop board state nm ("R", "Please input a valid number")
    xs -> gameLoop board state nm ("R", "\"" ++ unwords xs ++ "\"" ++ " is not a valid command, check available commands with \"help\"")

--------------- Helper functions ---------------

move :: Board -> Int -> Int -> Board -- Cant really think of a dynamic way to do this without making spaghetti code
move [a : as, b, c] 1 2 = [as, a : b, c]
move [a : as, b, c] 1 3 = [as, b, a : c]
move [a, b : bs, c] 2 3 = [a, bs, b : c]
move [a, b : bs, c] 2 1 = [b : a, bs, c]
move [a, b, c : cs] 3 1 = [c : a, b, cs]
move [a, b, c : cs] 3 2 = [a, c : b, cs]
move board _ _ = board

initializeBoard :: Int -> Board
initializeBoard x = [[1 .. x], [], []]

drawTowers :: Board -> IO ()
drawTowers [t1, t2, t3] = do
  let mh = maximum (t1 ++ t2 ++ t3) + 1
  writeRows (2 + mh) (reverse t1) mh
  writeRows (3 * (2 + mh)) (reverse t2) mh
  writeRows (5 * (2 + mh)) (reverse t3) mh
  goto 0 (mh + 2) -- Return line under towers

writeRows :: Int -> [Int] -> Int -> IO ()
writeRows _ _ 0 = return ()
writeRows pivot [] mh = do
  writeAt "|" 1 pivot mh
  writeRows pivot [] (mh - 1)
writeRows pivot n mh = do
  let el = head n
  writeAt " #" el (pivot - el) mh
  writeRows pivot (tail n) (mh - 1)

writeAt :: String -> Int -> Int -> Int -> IO ()
writeAt str n pivot height = do
  goto pivot height
  putStrLn (concat (replicate n str))

legalMove :: Board -> Int -> Int -> Bool
legalMove board x y =
  legalTowers x y
    && (board !! (x - 1) /= [])
    && (null (board !! (y - 1)) || (head (board !! (x - 1)) < head (board !! (y - 1))))

legalTowers :: Int -> Int -> Bool
legalTowers x y = x /= y && legalTower x && legalTower y

legalTower :: Int -> Bool
legalTower x = 0 < x && x < 4

colorToCode :: String -> String
colorToCode "R" = "31"
colorToCode "G" = "32"
colorToCode "Y" = "33"
colorToCode _ = "0"

--------------- Game loop IO Actions ---------------
help :: IO ()
help = do
  putStrLn "There are 4 commands:"
  putStrLn "b <n>: Starts a new game with n rings"
  putStrLn "<f> <t>: Moves a ring from pole f to pole t, if the move is legal"
  putStrLn "z <n>: Regrets n moves"
  putStrLn "q: Quits the game, losing all state"
  putStrLn "Remember that all commands are written in lowercase.\n"
  _ <- promptLine "Press enter to return to the game"
  return ()

drawMoves :: Int -> IO ()
drawMoves nm = putStrLn ("Number of moves: " ++ show nm)

drawMessage :: MSG -> IO ()
drawMessage (color, msg) = if msg == "" then putStr "\n" else putStr ("\ESC[" ++ colorToCode color ++ "m" ++ msg ++ "\ESC[0m\n")

newGame :: Board -> String -> State -> Int -> IO ()
newGame board n state nm =
  if isNumber n
    then do
      let num = read n :: Int
      if 0 < num && num <= 12
        then do
          let newBoard = initializeBoard num
          gameLoop newBoard [newBoard] 0 ("G", "A game with " ++ n ++ " rings will at the very least take " ++ show (2 ^ num - 1 :: Int) ++ " moves. Good luck.")
        else gameLoop board state nm ("R", "Right now, 12 is the maximum amount of rings playable.")
    else gameLoop board state nm ("R", "Please input a valid number")

winMessage :: [Int] -> Int -> IO ()
winMessage xs nm = do
  putStrLn "You won, congratulations!"
  let rings = last xs
  let optimalSol = 2 ^ rings - 1 :: Int
  putStrLn ("This game contained " ++ show rings ++ " rings")
  putStrLn
    ( "You did it in " ++ show nm ++ " moves, "
        ++ if optimalSol == nm
          then "just like the optimal solution. Good job!"
          else "while the optimal solution uses " ++ show optimalSol ++ ". Better luck next time!"
    )
  _ <- promptLine "Press enter to continue to the main menu"
  return ()

------------- Utility functions -------------
clr :: IO ()
clr = putStr "\ESC[2J" >> goto 0 0

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

promptLine :: String -> IO String
promptLine [] = putStr "> " >> getLine
promptLine prompt = putStrLn prompt >> putStr "> " >> getLine

isNumber :: String -> Bool
isNumber = all isDigit