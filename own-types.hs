import Dbg (db)

lvl :: Int
lvl = 4

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ radius) =
        let area = pi * radius ^ 2
        in area `db` lvl $ "radius:=" ++ show radius ++ " area:=" ++ show area
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
        let width = (abs $ x2 - x1)
            height = (abs $ y2 - y1)
            area = width * height
        in  area `db` lvl $ "width:=" ++ show width ++ " height:=" ++ show height ++ " area:=" ++ show area
