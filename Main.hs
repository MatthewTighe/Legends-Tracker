
-- Required modules for argument parsing
import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

-- Local modules
import Match
import File

{-
addMatch = do
    m <- getLine
    writeMatch $ parseMatchInput m
-}
addMatch     :: [String] -> IO ()
addMatch args = do
    writeMatch $ buildMatch args

{-
classRate = do
    c <- getLine
    rateByClass $ parseClass c
-}
classRate arg = do
    rateByClass $ parseClass arg

main = do
    args <- getArgs
    parse args

parse           :: [String] -> IO ()
parse []         = showHelp
parse (cmd:args) | cmd == "add" = addMatch args 
                 | cmd == "classAll" = classRate (args !! 0) 
                 | cmd == "help" = showHelp

helpMessage = [ "Usage: legends-tracker [commands ...]"
              , "Commands:"
              , "add <result> <archetype> <class> <archetype> <class>:  Adds match."
              , "classAll <class>:  Looks up winrate with <class>."
              , "help"
              , "<result>: one of 'win', 'loss', 'draw'"
              , "<archetype>: one of 'aggro', 'midrange', 'control'"
              , "<class>: one of 'archer', 'assassin', 'battlemage', 'crusader',"
              , "                'mage', 'monk', 'scout', sorcerer', 'spellsword',"
              , "                'warrior', 'strength', 'intelligence', 'willpower',"
              , "                'agility', 'endurance', 'neutral'"
              , "         color can be substituted for base class e.g. red == strength.'"
              , "NOTE: add format should be <result> <your stuff> <opponent stuff>"
              ] 
               
{-
helpMessage = "Usage: legends-tracker [commands ...]\n
               Commands:\n
               add <result> <archetype> <class> <archetype> <class>:  Adds match.\n
               classAll <class>:  Looks up winrate with <class>.\n
               help\n
               <result>: one of 'win', 'loss', 'draw'\n
               <archetype>: one of 'aggro', 'midrange', 'control'\n
               <class>: one of 'archer', 'assassin', 'battlemage', 'crusader',\n
                               'mage', 'monk', 'scout', sorcerer', 'spellsword',\n
                               'warrior', 'strength', 'intelligence', 'willpower',\n
                               'agility', 'endurance', 'neutral'\n
                        color can be substituted for base class e.g. red == strength.'\n
               NOTE: add format should be <result> <your stuff> <opponent stuff>\n"
-}

showHelp = do
    mapM_ putStrLn helpMessage
    
    
    
{-
-- Argument parsing modified from:
-- https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling
data Flags
    = Add           -- -a
    | ClassRate     -- -c
    | Help          -- -h

flags =
    [ Option ['a'] ["add"]          (NoArg Add)
    , Option ['c'] ["rate-class"]   (NoArg ClassRate)
    , Option ['h'] ["help"]         (NoArg Help)
    ]

parse argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        let commands = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else return (nub (concatMap set args), commands)
        
    (_ , _ , errs)  -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: legends-tracker [-ac] [command ...]"

main = do
    (as, fs) <- getArgs >>= parse
    putStrLn $ "Flags: " ++ show as
    putStrLn $ "Files: " ++ show fs
-}
