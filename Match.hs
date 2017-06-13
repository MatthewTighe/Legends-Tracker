module Match where

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
            deriving (Show, Eq, Enum)

-- https://stackoverflow.com/questions/44420523/can-i-build-a-list-of-strings-using-each-constructor-of-a-class
allClasses :: [String]
allClasses  = fmap show $ enumFrom Archer

-- The typical archetypes. This could easily be extended.
data Archetype =  Aggro
                | Midrange
                | Control
    deriving Show

-- Convert a Match to a String.
matchString :: Match -> String
matchString m = (show $ result m) ++ " " ++ 
                (show $ myArch m) ++ " " ++ (show $ myClass m) ++ " " ++
                (show $ theirArch m) ++ " " ++ (show $ theirClass m) ++ "\n"

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
