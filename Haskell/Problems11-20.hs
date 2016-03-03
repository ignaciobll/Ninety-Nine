-- Problema 11 -- Pendiente. Probable creación de tipo nuevo.
encode' :: (Eq a) => [a] -> [(Int,a)]
encode' [] = []
encode' (x:xs)
    | (length (same (x:xs) x)) == 1 = [(1,x)] ++ (encode' xs)
    | (length (same (x:xs) x)) /= 1 = (encode (drop (length (same (x:xs) x)) (x:xs)))

-- Problema 12 -- Dependiente de 11

-- Problema 13 -- Dependiente de 12

-- Problema 14

dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x,x]
dupli (x:xs) = (dupli [x]) ++ (dupli xs)

-- Problem 15 --

repli :: [a] -> Int -> [a]
repli [] _ = []
--repli [x] n = 
               
-- HELP --

same :: (Eq a) => [a] -> a -> [a]
same [] _ = []
same [x] a = if x == a then [x] else []
same (x:xs) y
    | x == y = [x] ++ same xs y
    | x /= y = []

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = [(length (same (x:xs) x), x)] ++ (encode (drop (length (same (x:xs) x)) (x:xs)))
