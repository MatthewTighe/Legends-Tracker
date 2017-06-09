module File where

import System.IO
import Data.List
import System.Directory

import Match
import Search

-- Convenience method borrowed from Mark Jones, PSU professor, that came from
-- one of his homework assignments.
(</>)  :: FilePath -> FilePath -> FilePath
p </> q = p ++ "/" ++ q

-- Check existence of the containing file.
doesMatchesExist :: IO Bool
doesMatchesExist  = do
    dir <- getUserDocumentsDirectory
    exists <- doesFileExist (dir </> "Legends-Tracker/Matches")
    return exists

-- Get the file handle of the tracking file.
getMatchFileHandle :: IO FilePath
getMatchFileHandle  = do
    dir <- getUserDocumentsDirectory
    createDirectoryIfMissing False (dir </> "Legends-Tracker")
    return (dir </> "Legends-Tracker/Matches") 

getDeckFileHandle :: IO FilePath
getDeckFileHandle  = do
    dir <- getUserDocumentsDirectory
    createDirectoryIfMissing False (dir </> "Legends-Tracker")
    return (dir </> "Legends-Tracker/Decks")
    
-- Write a match to the tracking file.
writeMatch  :: Match -> IO ()
writeMatch m = do
    file <- getMatchFileHandle
    appendFile file (matchString m)

writeDeck  :: Deck -> IO ()
writeDeck d = do
    file <- getDeckFileHandle
    appendFile file (deckString d)

retrieveDeck  :: String -> [String]
retrieveDeck s = do
    file <- getDeckFileHandle
    contents <- readFile file
    let fileLines = lines contents
    search fileLines s 1
        
allRates :: IO ()
allRates  = do
    exists <- doesMatchesExist
    file <- getMatchFileHandle
    content <- readFile file
    let fileLines = lines content
    if exists
        then putStrLn (allRates' fileLines)
        else print "No matches found."
    

-- Get the rate of wins/losses by class.
rateByClass  :: Class -> IO ()
rateByClass c = do
    exists <- doesMatchesExist
    file <- getMatchFileHandle
    content <- readFile file
    let fileLines = lines content
    if exists
        then print $ getRate (search fileLines (show c) 3) 
        else print "No matches found."

-- Get the rate of one class compared to a second.
rateByClassVClass        :: Class -> Class -> IO ()
rateByClassVClass me them = do
    exists <- doesMatchesExist
    file <- getMatchFileHandle
    content <- readFile file
    let fileLines = lines content
    if exists
        then print $ getRate (search (search fileLines (show me) 3) (show them) 5)
        else print "No matches found."

-- Delete the containing file
resetTracking :: IO ()
resetTracking  = do
    exists <- doesMatchesExist  
    file <- getMatchFileHandle
    if exists
        then removeFile file
        else return ()
    
-- Remove all entries of a specific class from the containing file.
resetClass  :: Class -> IO ()
resetClass c = do
    exists <- doesMatchesExist
    file <- getMatchFileHandle
    contents <- readFile file
    let fileLines = lines contents
    if length contents >=0 && exists
        then writeFile file $ unlines (resetClass' fileLines c)
        else return ()
