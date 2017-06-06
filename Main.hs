import Match
import File

main = do
    a <- getLine
    writeMatch $ parseInput a
    
