module Haskellium where

import Shape
import Point


data Haskellium = Haskellium  {name :: String
                            , lastName1 :: String
                            , lastName2 :: String
                            , location :: Point Float
                            , houseShape :: Shape
                            } deriving Eq

son :: Haskellium -> Haskellium -> String -> Haskellium
son (Haskellium _ apeP _ locP casaP) (Haskellium _ apeM _ locM casaM) (na) = (Haskellium na apeP apeM locP casaP )

houseCost :: Haskellium -> Float
houseCost (Haskellium _ _ _ _ hs) = (area(hs) * 2) + (perimeter(hs) * 2.5)

timeToWork :: Haskellium -> Float
timeToWork (Haskellium _ _ _ l _)= 
    if fromO(l) <= 300 
    then fromO(l) / 30
    else fromO(l) / 70

