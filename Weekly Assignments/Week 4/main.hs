-- Oppgave C
s :: (a -> b -> c) -> (a -> b) -> a -> c
s = \ f g x -> f x (g x) 

k :: a -> b -> a
k = \ x y -> x 

-- (s k k) er en partially applied funksjon av s.
-- I dette sammenheng returneres x av funksjonen uendret, pga k x (k x) returnerer x.
-- (k x) er bare en partially applied versjon av k, og brukes ikke.
skk :: x -> x -- evt c -> c for å matche type def av s
skk = s k k


-- Oppgave F
-- Med otherwise syntax
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) y   | x == y = xs 
                | otherwise = x : rem1 xs y

-- Med if/else syntax
rem1Alt :: Eq a => [a] -> a -> [a]
rem1Alt [] _ = []
rem1Alt (x:xs) y = if x == y then xs else x : rem1Alt xs y
