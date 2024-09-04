conta:: Int -> [Int] -> Int
conta _ [] = 0
conta a(x:xs)
  | x == a = 1 + conta a xs
  | otherwise = conta a xs
