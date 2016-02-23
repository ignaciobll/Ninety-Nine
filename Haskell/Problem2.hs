myButLast :: [a] -> a
myButLast [] = error "No last but one element"
myButLast [x] = error "No last but one element"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs

