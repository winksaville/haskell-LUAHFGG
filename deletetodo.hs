import Dbg (db)
import System.IO
import System.Directory
import Data.List

lvl :: Int
lvl = 4

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle `db` lvl $ "tempName:=" ++ show tempName ++ " tempHandle:=" ++ show tempHandle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks `db` lvl $ "todoTasks:=" ++ show todoTasks
    putStrLn "These are your TO-DO itemp:"
    putStr $ unlines numberedTasks `db` lvl $ "numberedTasks:=" ++ show numberedTasks
    hClose handle
    hClose tempHandle
