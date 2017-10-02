import System.Environment
import System.Directory
import System.IO
import Data.List
import Dbg (db)

lvl :: Int
lvl = 4

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("view", view)
           , ("add", add)
           , ("remove", remove)
           ]

view :: [String] -> IO ()
view args = putStrLn $ "view args:=" ++ show args

add :: [String] -> IO ()
add args = putStrLn $ "add args:=" ++ show args

remove :: [String] -> IO ()
remove args = putStrLn $ "remove args:=" ++ show args

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args