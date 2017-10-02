data TrafficLight = Red | Yellow | Green

-- Define an instanceof Eq for TrafficLight
-- Only need to define '==' as '/=' in Eq is
-- defined in terms of '=='.
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- Great our own Show instance for TrafficLight
instance Show TrafficLight where
        show Red = "Red Light"
        show Green = "Green Light"
        show Yellow = "Yellow Light"
