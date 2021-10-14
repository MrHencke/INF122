-- I'm sorry for the spaghetti code you're about to read.
-- Trekanter works as expected from assignment

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
      writeRows2 1 x x
      goto 0 (x + 2) --return to lines under pyramid

--Oppgave C
trekanter :: Int -> Int -> Int -> IO ()
trekanter t1 t2 t3 =
  if min t1 (min t2 t3) < 0
    then putStrLn "Input valid numbers"
    else do
      clr
      let mh = max t1 (max t2 t3)
      helper 0 t1 mh
      helper (2 + 2 * t1) t2 mh
      helper (4 + 2 * (t1 + t2)) t3 mh
      goto 0 (mh + 2) --return to lines under pyramids
  where
    helper i = writeRows2 (i + 1)

--Oppgave D
-- Jeg løste denne oppgaven, men sannsynligvis ikke slik det var "ment" å gjøres
data FileOrFolder = File Int | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint fof = do
  clr
  prettyPrintHelper fof 0

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
      writeRow i n n
      writeRows i (n -1)

writeRows2 :: Int -> Int -> Int -> IO ()
writeRows2 i n mh =
  if n < 0
    then return ()
    else do
      writeRow i n mh
      writeRows2 (i + 1) (n - 1) (mh - 1)

writeRow :: Int -> Int -> Int -> IO ()
writeRow i n mh = do
  goto i mh
  putStrLn (concat (replicate n " *"))

clr :: IO ()
clr = do
  putStr "\ESC[2J"
  goto 0 0

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
