data Ast = V Int | P Ast Ast | M Ast Ast

--Oppgave C
ev :: Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a
ev (V a) v _ _ = v a
ev (P ast1 ast2) v p m = ev ast1 v p m `p` ev ast2 v p m
ev (M ast1 ast2) v p m = ev ast1 v p m `m` ev ast2 v p m

va :: Num a => a -> a
va a = a

pa :: Num a => a -> a -> a
pa a b = a + b

ma :: Num a => a -> a -> a
ma a b = a * b

vb :: Int -> Bool
vb a = a `mod` 2 == 1

pb :: Bool -> Bool -> Bool
pb a b = a || b

mb :: Bool -> Bool -> Bool
mb a b = a && b

vStr :: Int -> String
vStr = show

pStr :: String -> String -> String
pStr a b = "(" ++ a ++ " + " ++ b ++ ")"

mStr :: String -> String -> String
mStr a b = "(" ++ a ++ " * " ++ b ++ ")"

--Oppgave D
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add a b) = g (folde f g a) (folde f g b)
