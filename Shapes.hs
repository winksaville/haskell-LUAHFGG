module Shapes
( Point(..)
, Shape(Circle, Rectangle) -- same as Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

import Dbg (db)

lvl :: Int
lvl = 4

data Point = Point { x :: Float , y :: Float } deriving (Show)
data Shape = Circle { loc :: Point, radius :: Float } | Rectangle { ll :: Point, ur :: Point } deriving (Show)

-- How would you combine Circle and Rectangle into a Shape, this doesn't work
-- we get "Multipel declaractions of 'Circle' and 'Rectangle'
--data Circle = Circle { loc :: Point, radius :: Float } deriving (show)
--data Rectangle = Rectangle { ll :: Point, ur :: Point } deriving (Show)
--data Shape = Circle | Rectangle

surface :: Shape -> Float
surface (Circle _ radius) =
        let area = pi * radius ^ 2
        in area `db` lvl $ "radius:=" ++ show radius ++ " area:=" ++ show area
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
        let width = (abs $ x2 - x1)
            height = (abs $ y2 - y1)
            area = width * height
        in  area `db` lvl $ "width:=" ++ show width ++ " height:=" ++ show height ++ " area:=" ++ show area

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) radius) dx dy = Circle (Point (x+dx) (y+dy)) radius
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

baseCircle :: Float -> Shape
baseCircle radius = Circle (Point 0 0) radius

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
