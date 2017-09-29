{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}

import Dbg (db)
--import GHC.OverloadedLabels

data P = MkP { personId :: Int, name :: String } deriving (Show)
data L = MkL { personId :: Int, name :: String } deriving (Show)

resetId :: P -> P
resetId p = p { personId = 0 }

updateId :: P -> Int -> P
updateId p newId = p { personId = newId }

-- An attempt to test overloaded labels, but it doesn't work :(
-- Ghc complains that "Record syntax is illegal here: {personId :: Int}
--getId :: r { personId :: Int } => r -> Int
--getId x = #personId x
