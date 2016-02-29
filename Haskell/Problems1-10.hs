myLast :: [a] -> a
myLast [] = error "No last item of an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "No last but one element"
myButLast [x] = error "No last but one element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

myTake :: Int -> [a] -> [a]
myTake z [] = []
myTake z [x] = x:[]
myTake z (x:xs) = if z > 1 then x:(myTake (z-1) xs) else x:[] 
                   
elementAt :: Int -> [a] -> a
elementAt z (xs) = myLast (myTake z (xs))
                   
myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (_:xs) = 1 + myLength (xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = True
isPalindrome [x,y] = x == y
isPalindrome (x:xs) = x == (last xs) && isPalindrome (init xs)

-- Problema 7 --
