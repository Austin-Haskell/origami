import Data.Char (ord, chr, isAlpha)

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) n
  | isAlpha x = encode x n (+) : caesar xs n
  | otherwise = x : caesar xs n

encode :: Char -> Int -> (Int -> Int -> Int) -> Char
encode x n f = chr $ f (ord x - base) n `mod` r + base
  where base = ord 'a'
        r = 26

-- goes the other way
unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar (x:xs) n
  | isAlpha x = encode x n (-) : unCaesar xs n
  | otherwise = x: unCaesar xs n


-- "test"
t = "abc xyz"
shift = 5

cipherOk = (unCaesar (caesar t shift) shift) == t
