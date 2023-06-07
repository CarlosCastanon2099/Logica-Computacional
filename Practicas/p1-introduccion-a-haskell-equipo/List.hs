module List where

    -- 1
    isPal :: String -> Bool
    isPal [] = True
    isPal [x] = True
    isPal (x:xs) = reversaFr (x:xs) == x:xs 

    -- 2
    concat' :: [[a]] -> [a]
    concat' [] = []
    concat' (a:as) = a ++ concat' as

    -- 3
    pascal :: Int -> [Int]
    pascal 1 = [1]
    pascal n = [1] ++ [x+y | (x,y) <- auxPascal (pascal (n-1))] ++ [1]

    auxPascal :: [a] -> [(a,a)]
    auxPascal (x:y:xs) = (x,y) : auxPascal (y:xs)
    auxPascal _ = []

    --pascal :: Int -> [Int]
    --pascal n = [comb n k | k <- [0..n]]

    -- Funcion auxiliar que calcula la combinatoria para el ejercicio de Pascal
    -- Usando la Formula de (comb n k) = n! / (k!(n-k)!)
    comb :: Int -> Int -> Int
    comb n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))

    -- Funcion auxiliar de combinatoria para poder sacar el factorial que necesitamos :p
    factorial :: Int -> Int
    factorial 0 = 1 -- Caso de termino
    factorial n = n * factorial (n-1) -- Caso recursivo

    -- 4
    reversaFr :: [a] ->[a]
    reversaFr [] = [] -- caso base
    reversaFr (a : as) = reversaFr as ++ [a] -- caso recursivo
