-- Henrik Hammer / Data HVL, Kull 19
module Main where

dictionary =
  [ ("bb", ["Big Brother"]),
    ("dep", ["department"]),
    ("sec", ["Sector"]),
    ("doubleplusgood", ["excellent", "fabulous", "fantastic", "best"]),
    ("doubleplusungood", ["terrible", "horrible", "worst"]),
    ("Ingsoc", ["English Socialism"]),
    ("joycamp", ["labour camp"]),
    ("Oldspeak", ["Standard English", "English"]),
    ("oldthink", ["objectivity", "rationalism", "democracy"]),
    ("thinkpol", ["The Thought Police"]),
    ("prolefeed", ["Popular culture", "pop-culture"]),
    ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
    ("fullwise", ["fully", "completely", "totally"]),
    ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
    ("goodwise", ["well"]),
    ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
    ("plusgood", ["very good", "great"]),
    ("plusungood", ["very bad"]),
    ("misprint", ["error", "misprediction"]),
    ("Miniluv", ["The Ministry of Love"]),
    ("Minipax", ["The Ministry of Peace"]),
    ("Minitrue", ["The Ministry of Truth"]),
    ("Miniplenty", ["The Ministry of Plenty"]),
    ("bellyfeel", ["blind, enthusiastic acceptance"]),
    ("doublethink", ["believing two contradictory ideas"]),
    ("duckspeak", ["vocal support of political orthodoxies"]),
    ("un", ["not"]),
    ("peace", ["war"]),
    ("strength", ["ignorance"]),
    -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
    ("", ["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
  ]

-- Oppgave 1 ----------------------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] ys = True
isPrefix xs [] = False
isPrefix (x : xs) (y : ys) = (x == y) && isPrefix xs ys

-- Oppgave 2 ----------------------------------------------------
locate :: String -> String -> [(Int, Int)]
locate [] ys = []
locate xs [] = []
locate xs ys = locateHelper xs ys [] 0
  where
    locateHelper xs ys akk n
      | null ys = akk
      | isPrefix xs ys = akk ++ (n, n + length xs) : locateHelper xs (tail ys) akk (n + 1)
      | otherwise = locateHelper xs (tail ys) akk (n + 1)

-- Oppgave 3 ----------------------------------------------------
translate :: String -> String
translate [] = ""
translate xs = translateHelper xs dictionary
  where
    translateHelper xs (d : ds)
      | null dict = ""
      | any (\x -> length x == length xs && isPrefix xs x) (snd (head dict)) = fst (head dict)
      | otherwise = translateHelper xs (tail dict)

-- Oppgave 4 ----------------------------------------------------
replace :: [(Int, Int)] -> String -> String
replace [] [] = []
replace [] txt = txt
replace xs [] = []
replace xs txt = replaceHelper (qsort xs) txt
  where
    replaceHelper ((x, y) : xs) txt
      | translate (sub x y txt) == "" = replace xs (sub 0 x txt ++ replicate (y - x) '*' ++ sub y (length txt) txt)
      | otherwise = replace xs (sub 0 x txt ++ translate (sub x y txt) ++ sub y (length txt) txt)

sub :: Int -> Int -> String -> String
sub s e txt = take (e - s) (drop s txt)

qsort :: [(Int, Int)] -> [(Int, Int)]
qsort [] = []
qsort (x : xs) = qsort gt ++ [x] ++ qsort lt
  where
    lt = [a | a <- xs, a <= x]
    gt = [b | b <- xs, b > x]

-- Oppgave 5 ----------------------------------------------------
toNewspeak :: String -> String
toNewspeak [] = []
toNewspeak txt = replace getIndexes txt
  where
    getIndexes = concatMap (`locate` txt) (concatMap snd dictionary)

-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int
analytics os ts = round (100 * (fromIntegral (diff os ts) / fromIntegral (length os)))

diff os ts = sum (filter (0 <) (map (\x -> count x os - count x ts) (' ' : '*' : ['A' .. 'z'])))
  where
    count x txt = length (filter (x ==) txt)

-- Oppgave 7 ----------------------------------------------------
main :: String -> (String, Int)
main xs = (newspeak, analytics xs newspeak)
  where
    newspeak = toNewspeak xs

-- Bonus ---------------------------------------------------------
{-
-- Jeg skrev noen tester for å sjekke svarene mine mot eksemplene som var gitt i oppgaveteksten.
-- Jeg er usikker på hvor relevant det er å inkludere i besvarelsen, så jeg lar det ligge kommentert ut.

test1 = map (\(x,y,z) -> isPrefix x y == z) testSet1
test2 = map (\(x,y,z) -> locate x y == z) testSet2
test3 = map (\(x,y) -> translate x == y) testSet3
test4 = map (\(x,y,z) -> replace x y == z) testSet4
test5 = map (\(x,y) -> toNewspeak x == y) testSet5
test6 = map (\(x,y,z) -> analytics x y == z) testSet6
test7 = map (\(x,y) -> main x == y) testSet7

testSet1 = [("foo","foobar", True), ("bar", "foobar", False), ("","foobar", True)]
testSet2 = [("oo", "foobarbarfoo", [(1,3),(10,12)]), ("oo","barbarbar",[]),("","foobar",[])]
testSet3 = [("excellent", "doubleplusgood"), ("anti-social tendency", "ownlife"),("haskell","")]
testSet4 = [([(11,20)],"Haskell is excellent", "Haskell is doubleplusgood"), ([(0,7)],"Haskell is excellent", "******* is excellent"), ([(0,7),(11,20)],"Haskell is excellent", "******* is doubleplusgood"), ([], "Haskell is excellent", "Haskell is excellent")]
testSet5 = [("The Thought Police put people in labour camps for inciting revolution and not having blind, enthusiastic acceptance of the department", "thinkpol put people in joycamps for inciting ********** and un having bellyfeel of the dep")]
testSet6 = [("Julia and Winston Smith think Big Brother is horrible","***** and ************* think bb is doubleplusungood", 49), ("excellent work", "doubleplusgood work", 43),("great job", "great job", 0),("well done", "", 100)]
testSet7 = [("Julia and Winston Smith think Big Brother is horrible", ("***** and ************* think bb is doubleplusungood", 49))]

runTests | all and [test1,test2,test3,test4,test5,test6, test7] = "All tests passed"
         | otherwise = "A test failed"

verboseRunTests = map show [test1,test2,test3,test4,test5,test6,test7]

-}