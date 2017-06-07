module Search where

import Match

-- Used to filter matches based on @key, which is a Result, Class, or Archetype.
-- @contents is contents of containing file, @n is 1, 3, 5, depending on a search
-- is being computed based on Result, or Class/Archetype for player(3)/opponent(5).
search :: [String] -> String -> Int -> [String]
search contents key n = filter (\x -> key `elem` take' n (words x)) contents 

take' n xs = if n > 3
                then take 2 (reverse xs)
                else take n xs

-- Counts wins, losses, or draws as a @key from @contents.
countResults :: [String] -> Result -> Int
countResults contents key = length $ search contents (show key) 1

-- Gets # wins / # losses in results, which will usually search-filtered result.
getRate :: [String] -> Double
getRate results = (resultsToDouble results Win) / (resultsToDouble results Loss)

-- Convenience method
resultsToDouble :: [String] -> Result -> Double
resultsToDouble results kind = fromIntegral $ countResults results kind 

-- Pure method for removing @class entries from @contents.
resetClass'           :: [String] -> Class -> [String] 
resetClass' contents c = filter (\x -> (show c) `notElem` take 3 (words x)) contents 
