multDois:: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs
