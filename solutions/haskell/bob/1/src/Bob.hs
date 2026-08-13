module Bob (responseFor) where

import Data.Char (isAlpha, isSpace)

responseFor :: String -> String
responseFor xs 
  | isSilence = "Fine. Be that way!"
  | isYelling && isQ = "Calm down, I know what I'm doing!"
  | isYelling = "Whoa, chill out!"
  | isQ = "Sure."
  | otherwise = "Whatever."
  where 
    trimmed = dropWhile isSpace (reverse (dropWhile isSpace (reverse xs)))
    isSilence = null trimmed
    isQ = last trimmed == '?'
    isYelling = any isAlpha xs && all (\c -> not (isAlpha c) || (c >= 'A' && c <= 'Z')) xs