import System.Random
import Dbg (db)

lvl :: Int
lvl = 4

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen `db` lvl $ "gen:=" ++ show gen
        (secondCoin, newGen') = random newGen `db` lvl $ "firstCoin:=" ++ show firstCoin ++ " newGen:=" ++ show newGen
        (thirdCoin, newGen'') = random newGen' `db` lvl $ "secondCoin:=" ++ show secondCoin ++ " newGen':=" ++ show newGen'
    in (firstCoin, secondCoin,thirdCoin) `db` lvl $ "thirdCoin:=" ++ show thirdCoin ++ " newGen'':=" ++ show newGen''


randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
    let (value, newGen) = random gen
    in value:randoms' newGen

finiteRandoms :: (RandomGen g, Show g, Random a, Num n, Eq n, Show n, Show a) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen) `db` lvl $ "gen:=" ++ show gen ++ " value:=0"
finiteRandoms n gen =
    let (value, newGen) = random gen `db` lvl $ "gen:=" ++ show gen ++ " n:=" ++ show n
        (restOfList, finalGen) = finiteRandoms (n-1) newGen `db` lvl $ "value:=" ++ show value ++ " newGen:=" ++ show newGen
    in (value:restOfList, finalGen) `db` lvl $ "value:=" ++ show value ++ " resetOfList:=" ++ show restOfList ++ " finalGen:=" ++ show finalGen
