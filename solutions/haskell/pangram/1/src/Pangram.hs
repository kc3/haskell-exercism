module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (elem)

isPangram :: String -> Bool
isPangram text = all (`elem` lowerText) ['a'..'z']
  where lowerText = map toLower text
  
