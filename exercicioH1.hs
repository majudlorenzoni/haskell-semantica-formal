-- exercicioH1.hs

-- Função que verifica se uma string contém um palíndromo
palindromo :: String -> Bool
palindromo str = any isPalindrome (filter (\s -> length s > 1) (substrings str))
  where
    -- Função que verifica se uma substring é um palíndromo
    isPalindrome :: String -> Bool
    isPalindrome s = s == reverse s

    -- Função que gera todas as substrings de uma string
    substrings :: String -> [String]
    substrings [] = []
    substrings str@(x:xs) = [ take n str | n <- [1..length str] ] ++ substrings xs

main :: IO ()
main = do
  print $ palindromo "racecar"  -- True, "racecar" é um palíndromo
  print $ palindromo "hello"    -- False, não há palíndromos em "hello"
  print $ palindromo "abccba"  -- True, "abccba" é um palíndromo
  print $ palindromo "ab"      -- False, não há palíndromos com mais de um caractere
