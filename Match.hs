module Match where

import qualified Data.Text as T

data Match = Match {
                   result :: Result,
                   myArch :: Archetype,
                   myClass :: Class,
                   theirArch :: Archetype,
                   theirClass :: Class
                   } deriving Show

data Result = Win
            | Loss
            | Draw
    deriving (Show, Eq)

resultNum :: Match -> Int
resultNum m | result m == Win  = 1
            | result m == Loss = -1
            | result m == Draw = 0

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

data Archetype =  Aggro
                | Midrange
                | Control
    deriving Show

matchData :: Match -> (Int, [String]) 
matchData m = (resultNum m,
               [show (myArch m), show (myClass m),
                show (theirArch m), show (theirClass m)]) 

matchString :: Match -> [String]
matchString m = [show (result m), show (myArch m), show (myClass m), 
                      show (theirArch m), show (theirClass m)]

parseInput :: String -> Match
parseInput s = buildMatch $ map T.unpack ((map T.toLower (map T.pack (words s))))

buildMatch :: [String] -> Match
buildMatch ss = Match (parseResult $ ss !! 0)
                      (parseArchetype $ ss !! 1)
                      (parseClass $ ss !! 2)
                      (parseArchetype $ ss !! 3)
                      (parseClass $ ss !! 4)

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

parseArchetype :: String -> Archetype
parseArchetype s | s == "aggro" = Aggro
                 | s == "midrange" = Midrange
                 | s == "control" = Control

parseResult :: String -> Result
parseResult s | s == "win" = Win
              | s == "loss" = Loss
              | s == "draw" = Draw
