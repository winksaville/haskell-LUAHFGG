import Data.List
import Geometry.Sphere as Sphere
import Geometry.Cube as Cube
import Dbg

addPeriods :: String -> String
addPeriods lst = intersperse '.' lst

spaceSep :: [String] -> String
spaceSep strgs = intercalate " " strgs


testSphere :: Float -> ()
testSphere radius =
        let vol = Sphere.volume radius
            area = Sphere.area radius
        in () `db` ("radius:=" ++ show radius ++ " vol:=" ++ show vol)

testSphere' :: Float -> IO ()
testSphere' radius = do
        let vol = Sphere.volume radius
            area = Sphere.area radius
        putStrLn ("radius:=" ++ show radius ++ " vol:=" ++ show vol ++ " area:=" ++ show area)
