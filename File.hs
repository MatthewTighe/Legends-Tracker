module File where

import Data.List
import System.Directory

import Match

(</>)  :: FilePath -> FilePath -> FilePath
p </> q = p ++ "/" ++ q

doesMatchesExist = do
    dir <- getUserDocumentsDirectory
    exists <- doesFileExist (dir </> "Legends-Tracker/Matches")
    return exists

writeMatch :: Match -> IO ()
writeMatch m = do
    file <- getFileHandle
    appendFile file (intercalate " " (matchString m ++ ["\n"])) 

rateByClass:: Class -> IO Double 
rateByClass c = do
    exists <- doesMatchesExist
    file <- getFileHandle
    content <- readFile file
    let fileLines = lines content
    if exists
        then return $ getRate (search fileLines (show c) 3) 
        else return 0

getFileHandle = do
    dir <- getUserDocumentsDirectory
    createDirectoryIfMissing False (dir </> "Legends-Tracker")
    return (dir </> "Legends-Tracker/Matches") 

-- Used to filter matches based on @key, which is a Result, Class, or Archetype.
-- @contents is contents of containing file, @n is 1, 3, 5, depending on a search
-- is being computed based on Result, or Class/Archetype for player(3)/opponent(5).
search :: [String] -> String -> Int -> [String]
search contents key n = filter (\x -> key `elem` take n (words x)) contents 

-- Counts wins, losses, or draws as a @key from @contents.
countResults :: [String] -> Result -> Int
countResults contents key = length $ search contents (show key) 1

getRate :: [String] -> Double
getRate results = (resultsToDouble results Win) / (resultsToDouble results Loss)

resultsToDouble :: [String] -> Result -> Double
resultsToDouble results kind = fromIntegral $ countResults results kind 
