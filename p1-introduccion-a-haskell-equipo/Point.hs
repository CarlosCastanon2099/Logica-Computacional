module Point where

type Point a = (a,a)

distance :: (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) = sqrt(((x2-x1)^2)+((y2-y1)^2))

fromO :: (Float,Float)-> (Float)
fromO (x1,y1) = sqrt(((x1)^2)+((y1)^2))