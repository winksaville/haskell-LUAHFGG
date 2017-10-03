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
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n") `db` lvl $ "add fileName:=" ++ show fileName ++ " todoItem:=" ++ todoItem

remove :: [String] -> IO ()
remove args = putStrLn $ "remove args:=" ++ show args

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
