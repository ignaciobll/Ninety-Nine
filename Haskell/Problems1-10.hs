myLast :: [a] -> a
myLast [] = error "No last item of an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "No last but one element"
myButLast [x] = error "No last but one element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
