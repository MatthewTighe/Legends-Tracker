
-- Required modules for argument parsing
import System.Environment

-- Local modules
import Match
import File

addMatch     :: [String] -> IO ()
addMatch args = if length args == 5 
    then do writeMatch $ buildMatch args
    else do writeMatch $ buildMatchWithDeck (args ++ (retrieveDeck $ args !! 1))

addDeck     :: [String] -> IO()
addDeck args = do writeDeck $ buildDeck args

classRate arg = do
    rateByClass $ parseClass arg

classVClass args = do
   rateByClassVClass (parseClass $ args !! 0) (parseClass $ args !! 1)

allClassRates = do
   allRates 

startInteractive = do putStrLn "quit will exit this session. Enter a command: "
                      line <- getLine
                      if line == "quit"
                          then putStrLn "Thanks for using Legends Tracker!" 
                          else parse $ words line

main = do
    args <- getArgs
    parse args

parse           :: [String] -> IO ()
parse []         = showHelp
parse (cmd:args) | cmd == "add" = addMatch args 
                 | cmd == "addDeck" = addDeck args
                 | cmd == "class" = classRate (args !! 0) 
                 | cmd == "vs" = classVClass args
                 | cmd == "all" = allClassRates
                 | cmd == "reset" = resetTracking
                 | cmd == "resetClass" = resetClass $ parseClass (args !! 0)
                 | cmd == "interactive" = startInteractive
                 | cmd == "help" = showHelp

helpMessage = [ "Usage: legends-tracker [commands ...]"
              , ""
              , "Commands:"
              , "   add <result> <archetype> <class> <archetype> <class>:  Adds match."
              , "   add <result> <deck> <archetype> <class>: Adds match using saved deck."
              , "   addDeck <name> <archetype> <class>: Save one of your deck types."
              , "   class <class>:  Looks up winrate with <class>."
              , "   vs <class> <class>: Looks up winrate with first <class> against second."
              , "   all: Looks up winrate for every class."
              , "   reset: Resets all tracking data."
              , "   resetClass <class>: Resets all tracking data for a specific class."
              , "   interactive: Start an interactive session."
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
