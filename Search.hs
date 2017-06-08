module Search where

import Match

-- Used to filter matches based on @key, which is a Result, Class, or Archetype.
-- @contents is contents of containing file, @n is 1, 3, 5, depending on a search
-- is being computed based on Result, or Class/Archetype for player(3)/opponent(5).
search               :: [String] -> String -> Int -> [String]
search contents key n = filter (\x -> key `elem` take' n (words x)) contents 

take' n xs = if n > 3
                then take 2 (reverse xs)
                else take n xs

-- Counts wins, losses, or draws as a @key from @contents.
countResults :: [String] -> Result -> Int
countResults contents key = length $ search contents (show key) 1

-- Gets # wins / # losses in results, which will usually be search-filtered result.
getRate        :: [String] -> Double
getRate results = filterMath ((resultsToDouble results Win)
                              / (resultsToDouble results Win + resultsToDouble results Loss))
                  where filterMath x | isNaN x = 0
                                     | isInfinite x = 1
                                     | otherwise = x

allRates'         :: [String] -> String
allRates' contents = unlines $ zipWith (combine)
                        allClasses
                        (map (show . getRate) (map (\x -> search contents x 3) allClasses)) 
                     where combine x y = (pad x) ++ "| " ++ y
{-
allRates' contents = zipWith (++) allClasses (search
allRates' contents = [y ++ " " ++ show x | y <- allClasses, x <- getRate (search contents y 3)]
allRates' contents  = zipWith (getRate) (replicate (length allClasses) contents) allClasses 
-}

pad  :: String -> String
pad s | length s < 13 = pad (s ++ " " )
      | otherwise     = s

-- Convenience method
resultsToDouble             :: [String] -> Result -> Double
resultsToDouble results kind = fromIntegral $ countResults results kind 

-- Pure method for removing @class entries from @contents.
resetClass'           :: [String] -> Class -> [String] 
resetClass' contents c = filter (\x -> (show c) `notElem` take 3 (words x)) contents 
