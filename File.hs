module File where

import System.Exit
import System.IO
import Data.List
import System.Directory

import Match
import Search

-- Convenience method borrowed from Mark Jones, PSU professor, that came from
-- one of his homework assignments.
(</>)  :: FilePath -> FilePath -> FilePath
p </> q = p ++ "/" ++ q

-- Check existence of the tracking file.
doesMatchesExist :: IO Bool
doesMatchesExist  = do
    dir <- getUserDocumentsDirectory
    exists <- doesFileExist (dir </> "Legends-Tracker/Matches")
    return exists

-- Get the file handle of the tracking file.
getFileHandle :: IO FilePath
getFileHandle  = do
    dir <- getUserDocumentsDirectory
    createDirectoryIfMissing False (dir </> "Legends-Tracker")
    return (dir </> "Legends-Tracker/Matches") 

-- Write a match to the tracking file.
writeMatch  :: Match -> IO ()
writeMatch m = do
    file <- getFileHandle
    appendFile file (matchString m) 

-- Get the win ratio for every class.
allRates :: IO ()
allRates  = do
    exists <- doesMatchesExist
    if exists then do 
            file <- getFileHandle
            content <- readFile file
            let fileLines = lines content
            putStrLn (allRates' fileLines)
        else putStrLn "No matches found."
    

-- Get the rate of wins/losses by class.
rateByClass  :: Class -> IO ()
rateByClass c = do
    exists <- doesMatchesExist
    if exists then do
            file <- getFileHandle
            content <- readFile file
            let fileLines = lines content
            print $ getRate (search fileLines (show c) 3) 
        else putStrLn "No matches found."

-- Get the rate of one class compared to a second.
rateByClassVClass        :: Class -> Class -> IO ()
rateByClassVClass me them = do
    exists <- doesMatchesExist
    if exists then do
            file <- getFileHandle
            content <- readFile file
            let fileLines = lines content
            print $ getRate (search (search fileLines (show me) 3) (show them) 5)
        else putStrLn "No matches found."

-- Delete the tracking file
resetTracking :: IO ()
resetTracking  = do
    exists <- doesMatchesExist  
    if exists then do
            file <- getFileHandle
            removeFile file
        else putStrLn "No matches found." 
    
-- Remove all entries of a specific class from the tracking file.
-- Creates a temp file, writes the filtered information to it, and then
-- replaces the tracking file.
resetClass  :: Class -> IO ()
resetClass c = do
    exists <- doesMatchesExist
    if exists then do
            file <- getFileHandle
            let tmp = "tmp"
            contents <- readFile file
            let fileLines = lines contents
            writeFile tmp $ unlines (resetClass' fileLines c)
            renameFile tmp file
        else putStrLn "No matches found." 
