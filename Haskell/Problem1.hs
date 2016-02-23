myLast :: [a] -> a
myLast [] = error "No last item of an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs
