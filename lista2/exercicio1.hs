-- recebe um inteiro e uma lista e multiplica todos
-- os elementos dessa lista pelo inteiro

multLista:: Int -> [Int] -> [Int]
multLista _[] = []
multLista n (x:xs) = n * x: multLista n xs