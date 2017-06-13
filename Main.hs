
-- Required modules for argument parsing
import System.Environment
import Data.Char

-- Local modules
import Match
import File

doClassRate arg = do rateByClass $ parseClass (map toLower arg) 

-- Get win rates for a specific class against a second specific class.
doClassVClass args = do rateByClassVClass 
                            (parseClass $ args !! 0) 
                            (parseClass $ args !! 1)

doResetClass arg = do resetClass $ parseClass (map toLower arg)

-- The entry point of the program.
main = do
    args <- getArgs
    parse args

-- Parse command line arguments.
parse           :: [String] -> IO ()
parse []         = showHelp
parse (cmd:args) | cmd == "add" = writeMatch $ buildMatch (toLower' args)
                 | cmd == "class" = doClassRate (args !! 0) 
                 | cmd == "vs" = doClassVClass (toLower' args)
                 | cmd == "all" = allRates
                 | cmd == "reset" = resetTracking
                 | cmd == "resetClass" = doResetClass (args !! 0)
                 | cmd == "help" = showHelp
                 | otherwise = showHelp

toLower'   :: [String] -> [String]
toLower' ss = map (map toLower) ss

-- A verbose help message to assist users.
helpMessage = [ "Usage: legends-tracker [commands ...]"
              , ""
              , "Commands:"
              , "   add <result> <archetype> <class> <archetype> <class>:  Adds match."
              , "   class <class>:  Looks up winrate with <class>."
              , "   vs <class> <class>: Looks up winrate with first <class> against second."
              , "   all: Looks up winrate for every class."
              , "   reset: Resets all tracking data."
              , "   resetClass <class>: Resets all tracking data for a specific class."
              , "   help"
              , ""
              , "<result>: one of 'win', 'loss', 'draw'"
              , "<archetype>: one of 'aggro', 'midrange', 'control'"
              , "<class>: one of 'archer', 'assassin', 'battlemage', 'crusader',"
              , "                'mage', 'monk', 'scout', sorcerer', 'spellsword',"
              , "                'warrior', 'strength', 'intelligence', 'willpower',"
              , "                'agility', 'endurance', 'neutral'"
              , "         color can be substituted for base class e.g. red == strength."
              , ""
              , "NOTE: add format should be <result> <your stuff> <opponent stuff>"
              ] 

-- Print the help message.
showHelp = do
    mapM_ putStrLn helpMessage

