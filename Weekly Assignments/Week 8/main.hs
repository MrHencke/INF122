-- I'm sorry for the spaghetti code you're about to read.

--Oppgave A
trekant :: Int -> IO ()
trekant x =
  if x < 1
    then putStrLn "Input a valid number"
    else do
      clr
      writeRows 0 x
      goto 0 (x + 2) --return to lines under pyramids

--Oppgave B
rettTrekant :: Int -> IO ()
rettTrekant x =
  if x < 1
    then putStrLn "Input a valid number"
    else do
      clr
      writeRows2 1 x
      goto 0 (x + 2) --return to lines under pyramid

--Oppgave C
trekanter :: Int -> Int -> Int -> IO ()
trekanter t1 t2 t3 =
  if min t1 (min t2 t3) < 0
    then putStrLn "Input valid numbers"
    else do
      clr
      helper 0 t1
      helper (2 * t1) t2
      helper (2 * (t1 + t2)) t3
      goto 0 (max t1 (max t2 t3) + 2) --return to lines under pyramids
  where
    helper i = writeRows2 (i + 1)

--Oppgave D
-- Jeg løste denne oppgaven, men sannsynligvis ikke slik det var "ment" å gjøres
data FileOrFolder = File Int | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint (File x) = putStrLn ("-File " ++ show x)
prettyPrint (Folder x) = do
  clr
  putStrLn ("-Folder " ++ show (length x))
  mapM_ (`prettyPrintHelper` 1) x

prettyPrintHelper :: FileOrFolder -> Int -> IO ()
prettyPrintHelper (File x) y = do
  putStrLn (indent y ++ "-File " ++ show x)
prettyPrintHelper (Folder x) y = do
  putStrLn (indent y ++ "-Folder " ++ show (length x))
  mapM_ (`prettyPrintHelper` (y + 1)) x

--Hjelpemetoder

indent :: Int -> String
indent y = concat (replicate y "    ")

writeRows :: Int -> Int -> IO ()
writeRows i n =
  if n < 0
    then return ()
    else do
      writeRow i n
      writeRows i (n -1)

writeRows2 :: Int -> Int -> IO ()
writeRows2 i n =
  if n < 0
    then return ()
    else do
      writeRow i n
      writeRows2 (i + 1) (n - 1)

writeRow :: Int -> Int -> IO ()
writeRow i n = do
  goto i n
  putStrLn (concat (replicate n " *"))

clr :: IO ()
clr = do
  putStr "\ESC[2J"
  goto 0 0

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
