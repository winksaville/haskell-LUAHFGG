import Control.Monad
import Data.Char

main = do
    contents <- getContents
    --putStr (map toUpper contents)
    putStr $ map toUpper contents
