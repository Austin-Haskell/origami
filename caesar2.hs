-- render unto caesar
module Caesar where
-- refactor this for better naming, main works this way, altho read is bad and will throw an exception if you ignore its instruction to give it a number. could undoubtedly refactor with maybe etc.
import Data.Char

render :: String -> [Char]
render xs = map toLower (concat (words xs))

unto' :: Int -> Char -> Char
unto' x y
    | isAlpha y = caesar x y
    | otherwise = y

unto  :: Int -> String -> String
unto x ys = map (unto' x) (render ys)
-- these work the way i want them to work, but
-- i'd like to combine them into one function
-- which seems like it should be totally possible


caesar :: Int -> Char -> Char
-- caesar k c  chr . (+ k) . ord $ c
-- that was my original, but it can't wrap
caesar k c = chr $ ord 'a' + (ord c - ord 'a' + k) `mod` 26

-- caesar k c (=) chr $ (ord c + k) `mod` 26
-- oh yikes this last version is NOT GOOD


-- unto x ys (=) map (caesar x) (render ys)
-- doing it this way but w/o `render` means it translates all the characters,
-- including spaces


mainCaesar x = do
	ys <- getLine
	return $ unto x ys

main = do
  putStrLn "Give me a number"
  shift <- getLine
  putStrLn "What should I encode?"
  word <- getLine
  print $ unto (read shift) word
