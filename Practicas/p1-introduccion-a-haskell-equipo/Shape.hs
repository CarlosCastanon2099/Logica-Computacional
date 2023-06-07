module Shape where

data Shape = Circle Float | Square Float | Rectangle Float Float | Triangle Float Float | Trapeze Float Float Float

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Square byh ) = byh * byh
area (Rectangle b h) = b * h
area (Triangle b h)= (b * h)/2 
area (Trapeze bma bme h)=((bma+bme)/2)*h

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 4 * l
perimeter (Rectangle b h) = (2 * b) + (2 * h)
perimeter (Triangle b h) = 3 * b --Considerando que es equilatero
perimeter (Trapeze bma bme h) = ((sqrt((((bma-bme)/2)^2)+ h^ 2))*2)+bma+bme

instance Eq Shape where
    (==)(Circle r1)(Circle r2)=area(Circle r1)==area(Circle r2)
    (==)(Square byh1)(Square byh2)=area(Square byh1)==area(Square byh2)
    (==)(Rectangle b1 h1)(Rectangle b2 h2)= area(Rectangle b1 h1)==area(Rectangle b2 h2)
    (==)(Triangle b1 h1)(Triangle b2 h2)= area(Triangle b1 h1)==area(Triangle b2 h2)
    (==)(Trapeze bma1 bme1 h1)(Trapeze bma2 bme2 h2)=area(Trapeze bma1 bme1 h1)==area(Trapeze bma2 bme2 h2)
    (==)_ _ = False
    
instance Ord Shape where 
 compare s1 s2 = compare(area s1)(area s2)



