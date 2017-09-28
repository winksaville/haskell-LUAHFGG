import Data.List
import Geometry.Sphere as Sphere
import Geometry.Cube as Cube
import Dbg

addPeriods :: String -> String
addPeriods lst = intersperse '.' lst

spaceSep :: [String] -> String
spaceSep strgs = intercalate " " strgs

lvl :: Int
lvl = 4

testSphere :: Float -> ()
testSphere radius =
        let vol = Sphere.volume radius
            area = Sphere.area radius
        in
            -- Valid forms of applying db

            -- The "infix" notation with the return value first is the "best" form to use
            () `db` lvl $ "radius:=" ++ show radius ++ " vol:=" ++ show vol

            -- As you can easilly comment out the `db` to "remove" the comment
            --() --`db` lvl $ "radius:=" ++ show radius ++ " vol:=" ++ show vol

            -- If you don't use the "$" and use infix then you need to use parens as below
            -- or use the db in the prefix form, see **
            --(() `db` lvl) ("radius:=" ++ show radius ++ " vol:=" ++ show vol)

            -- ** This shows the prefix form of db with or without "$"
            --db () lvl $ "radius:=" ++ show radius ++ " vol:=" ++ show vol
            --db () lvl ("radius:=" ++ show radius ++ " vol:=" ++ show vol)

testSphere' :: Float -> IO ()
testSphere' radius = do
        let vol = Sphere.volume radius
            area = Sphere.area radius
        putStrLn $ "radius:=" ++ show radius ++ " vol:=" ++ show vol ++ " area:=" ++ show area

