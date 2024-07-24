-- Implementar uma função recursiva que
-- recebe a base e o expoente e calcula a potência:
-- > potencia 2 3
-- 8

potencia:: Int -> Int -> Int 
potencia base 0 = 1
potencia base expoente = base * potencia base (expoente - 1)

main ::IO()
main = do
  print (potencia base expoente)