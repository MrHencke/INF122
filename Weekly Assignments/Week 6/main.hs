-- Oppgave A
remg :: [a] -> (a -> Bool) -> [a]
remg [] y = []
remg (x:xs) y = if y x then xs else x : remg xs y


-- Oppgave 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap ea oa [] = []
altMap ea oa [x] = [ea x]
altMap ea oa (x:y:xs) = ea x : oa y : altMap ea oa xs