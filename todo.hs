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
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith(\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n") `db` lvl $ "add fileName:=" ++ show fileName ++ " todoItem:=" ++ todoItem

remove :: [String] -> IO ()
remove args = putStrLn $ "remove args:=" ++ show args

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
