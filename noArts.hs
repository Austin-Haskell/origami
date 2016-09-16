noArts :: String -> [String]
noArts xs = filter (\x -> not (elem x ["the", "a", "an"])) $ words xs
