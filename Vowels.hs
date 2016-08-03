isVowel = (`elem` "aeiouAEIOU")
isConsonant = not . isVowel
intervowel wd = [vls] ++ intersperse vls [Literal [a]|a<-filter isConsonant wd] ++ [vls]
	where vls = Star (Charset "aeiou")
