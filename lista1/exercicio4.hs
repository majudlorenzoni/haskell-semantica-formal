-- Implemente a função:
-- menorTres :: Int -> Int -> Int -> Int
-- que recebe três inteiros e devolve o menor entre os três

menorTres :: Int -> Int -> Int -> Int
menorTres x y z
  |(x < y) || (x < z) = x
  |(y < x) || (y < z) = y
  |(z < x) || (z < y) = z

main::IO()
main = do
  print (menorTres x y z)

