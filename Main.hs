
-- Required modules for argument parsing
import System.Environment

-- Local modules
import Match
import File

addMatch     :: [String] -> IO ()
addMatch args = do
    writeMatch $ buildMatch args

classRate arg = do
    rateByClass $ parseClass arg

classVClass args = do
   rateByClassVClass (parseClass $ args !! 0) (parseClass $ args !! 1)

allClassRates = do
   allRates 

main = do
    args <- getArgs
    parse args

parse           :: [String] -> IO ()
parse []         = showHelp
parse (cmd:args) | cmd == "add" = addMatch args 
                 | cmd == "class" = classRate (args !! 0) 
                 | cmd == "vs" = classVClass args
                 | cmd == "all" = allClassRates
                 | cmd == "reset" = resetTracking
                 | cmd == "resetClass" = resetClass $ parseClass (args !! 0)
                 | cmd == "help" = showHelp
                 | otherwise = showHelp

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

showHelp = do
    mapM_ putStrLn helpMessage

test = do
    args <- getLine
    parse (words args)
