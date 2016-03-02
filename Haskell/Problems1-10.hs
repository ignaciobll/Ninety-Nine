import Data.List
import Test.QuickCheck

-- Problem 1 --
myLast :: [a] -> a
myLast [] = error "No last item of an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2 --
myButLast :: [a] -> a
myButLast [] = error "No last but one element"
myButLast [x] = error "No last but one element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Problem 3 --
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake z [] = []
myTake z [x] = x:[]
myTake z (x:xs) = if z > 1 then x:(myTake (z-1) xs) else x:[] 
                   
elementAt :: Int -> [a] -> a
elementAt z (xs) = myLast (myTake z (xs))

-- Problem 4 --                   
myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (_:xs) = 1 + myLength (xs)

-- Problem 5 --
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6 --
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = True
isPalindrome [x,y] = x == y
isPalindrome (x:xs) = x == (last xs) && isPalindrome (init xs)

-- Problema 7 --
data NestedList a = Elem a | List [NestedList a]
my_Flatten :: NestedList a -> [a]
my_Flatten (Elem a) = [a]
my_Flatten (List []) = []
my_Flatten (List (x:xs)) = (my_Flatten x) ++ (my_Flatten (List xs))

-- Problem 8 --
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | x /= y = [x] ++ compress (y:xs)

-- Problem 9 --

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = [(same (x:xs) x)] ++ pack (drop (length (same (x:xs) x)) (x:xs))
              
same :: (Eq a) => [a] -> a -> [a]
same [] _ = []
same [x] a = if x == a then [x] else []
same (x:xs) y
    | x == y = [x] ++ same xs y
    | x /= y = []
                   
-- Problem 10 --

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = [(length (same (x:xs) x), x)] ++ (encode (drop (length (same (x:xs) x)) (x:xs)))

-------------  Test --------------


prop_myTake :: Int -> [a] -> Bool
prop_myTake z xs = (length (myTake z xs)) == (length (take z xs)) --Not working
