-- A soma do comprimento de qualquer dois lados de um triângulo é sempre maior do que
-- o comprimento do terceiro lado. Fazer uma função que recebe o comprimento dos três
-- lados de um triângulo e verifica essa condição

somaTriangulo :: Int -> Int -> Int -> Bool
somaTriangulo a b c = (a + b > c) && (a + c > b) && (b + c > a)

main ::IO()
main = do
    print (somaTriangulo a b c) -- True