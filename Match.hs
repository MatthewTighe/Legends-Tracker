module Match where

import qualified Data.Text as T

data Match = Match {
                   result :: Result,
                   myArch :: Archetype,
                   myClass :: Class,
                   theirArch :: Archetype,
                   theirClass :: Class
                   } deriving Show

data Deck = Deck {
                 name :: String,
                 archetype ::  Archetype,
                 clss :: Class
                 }
    deriving Show

-- The possible results of a Match.
data Result = Win
            | Loss
            | Draw
    deriving (Show, Eq)

-- TODO
-- not in use yet
resultNum  :: Match -> Int
resultNum m | result m == Win  = 1
            | result m == Loss = -1
            | result m == Draw = 0

-- The playable classes.
data Class = Archer 
           | Assassin
           | Battlemage
           | Crusader
           | Mage
           | Monk
           | Scout
           | Sorcerer
           | Spellsword
           | Warrior
           | Strength
           | Intelligence
           | Willpower
           | Agility
           | Endurance
           | Neutral
    deriving (Show, Eq, Enum)

-- https://stackoverflow.com/questions/44420523/can-i-build-a-list-of-strings-using-each-constructor-of-a-class#44420810
allClasses :: [String]
allClasses  = fmap show $ enumFrom Archer

-- The typical archetypes. This could easily be extended.
data Archetype = Aggro
               | Midrange
               | Control
    deriving Show

-- TODO
-- not in use yet
matchData  :: Match -> (Int, [String]) 
matchData m = (resultNum m,
               [show (myArch m), show (myClass m),
                show (theirArch m), show (theirClass m)]) 

-- Convert a Match to a String.
matchString  :: Match -> String
matchString m = (show $ result m) ++ " " ++ 
                (show $ myArch m) ++ " " ++ (show $ myClass m) ++ " " ++
                (show $ theirArch m) ++ " " ++ (show $ theirClass m) ++ "\n"

-- Convert a Deck to a String
deckString  :: Deck -> String 
deckString d = name d ++ " " ++ (show $ archetype d) ++ " " ++ (show $ clss d) ++ "\n"

-- Build a Match from its component Strings.
buildMatch   :: [String] -> Match
buildMatch ss = Match (parseResult $ ss !! 0)
                      (parseArchetype $ ss !! 1)
                      (parseClass $ ss !! 2)
                      (parseArchetype $ ss !! 3)
                      (parseClass $ ss !! 4)
-- Build a Match from its component Strings, using a Deck.
buildMatchWithDeck   :: [String] -> Match
buildMatchWithDeck ss = Match (parseResult $ ss !! 0)
                        (parseArchetype $ (reverse ss) !! 1)
                        (parseClass $ (reverse ss) !! 0) 
                        (parseArchetype $ ss !! 2)
                        (parseClass $ ss !! 3)

-- Build a Deck from its component Strings.
buildDeck   :: [String] -> Deck 
buildDeck ss = Deck (ss !! 0)
                    (parseArchetype $ ss !! 1)
                    (parseClass $ ss !! 2)

-- Parse a String to the appropriate Class.
parseClass  :: String -> Class
parseClass s | s == "archer" = Archer
             | s == "assassin" = Assassin
             | s == "battlemage" = Battlemage
             | s == "crusader" = Crusader
             | s == "mage" = Mage
             | s == "monk" = Monk
             | s == "scout" = Scout
             | s == "sorcerer" = Sorcerer
             | s == "spellsword" = Spellsword
             | s == "warrior" = Warrior
             | s == "strength" || s == "red" = Strength
             | s == "intelligence" || s == "blue" = Intelligence
             | s == "willpower" || s == "yellow" = Willpower
             | s == "agility" || s == "green" = Agility
             | s == "endurance" || s == "purple" = Endurance
             | s == "neutral" = Neutral

-- TODO unused. import would be unnecessary.
-- Parse a String to a Match.
parseMatchInput  :: String -> Match
parseMatchInput s = buildMatch $ map T.unpack ((map T.toLower (map T.pack (words s))))

-- Parse a String to the appropriate Archetype.
parseArchetype  :: String -> Archetype
parseArchetype s | s == "aggro" = Aggro
                 | s == "midrange" = Midrange
                 | s == "control" = Control

-- Parse a String to the appropriate Result.
parseResult  :: String -> Result
parseResult s | s == "win" = Win
              | s == "loss" = Loss
              | s == "draw" = Draw
