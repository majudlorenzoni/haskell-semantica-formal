-- Defina, usando guardas, a função sinal
-- sinal :: Int -> Int
-- que recebe um inteiro como entrada e devolve: -1 se a entrada for um número negativo,
-- 1, caso seja positivo ou 0 caso a entrada seja o número zero

sinal :: Int -> Int
sinal x = -(x)

main::IO()
main = do
  print (sinal x)
