-- implemente a função 
-- elemento :: Int => [Int] -> Bool
-- que recebe um inteiro e uma lista, e devolve um booleano dizendo se o inteiro se encontra
-- na lista

elemento:: Int -> [Int] -> Bool
elemento _ [] = False
elemento a (x:xs)
  | x == a = True
  | otherwise = elemento a xs




