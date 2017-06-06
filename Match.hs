module Match where

import qualified Data.Text as T

data Match = Match {
                   result :: Result,
                   myArch :: Archetype,
                   myClass :: Class,
                   theirArch :: Archetype,
                   theirClass :: Class
                   } deriving Show

-- The possible results of a Match.
data Result = Win
            | Loss
            | Draw
    deriving (Show, Eq)

-- TODO
-- not in use yet
resultNum :: Match -> Int
resultNum m | result m == Win  = 1
            | result m == Loss = -1
            | result m == Draw = 0

-- The playable classes.
data Class =  Archer 
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
            deriving (Show, Eq)

-- The typical archetypes. This could easily be extended.
data Archetype =  Aggro
                | Midrange
                | Control
    deriving Show

-- TODO
-- not in use yet
matchData :: Match -> (Int, [String]) 
matchData m = (resultNum m,
               [show (myArch m), show (myClass m),
                show (theirArch m), show (theirClass m)]) 

-- Convert a Match to a String.
matchString :: Match -> [String]
matchString m = [show (result m), show (myArch m), show (myClass m), 
                      show (theirArch m), show (theirClass m)]

-- Parse a String to a Match.
parseInput :: String -> Match
parseInput s = buildMatch $ map T.unpack ((map T.toLower (map T.pack (words s))))

-- Build a Match from its component Strings.
buildMatch :: [String] -> Match
buildMatch ss = Match (parseResult $ ss !! 0)
                      (parseArchetype $ ss !! 1)
                      (parseClass $ ss !! 2)
                      (parseArchetype $ ss !! 3)
                      (parseClass $ ss !! 4)

-- Parse a String to the appropriate Class.
parseClass :: String -> Class
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

-- Parse a String to the appropriate Archetype.
parseArchetype :: String -> Archetype
parseArchetype s | s == "aggro" = Aggro
                 | s == "midrange" = Midrange
                 | s == "control" = Control

-- Parse a String to the appropriate Result.
parseResult :: String -> Result
parseResult s | s == "win" = Win
              | s == "loss" = Loss
              | s == "draw" = Draw
