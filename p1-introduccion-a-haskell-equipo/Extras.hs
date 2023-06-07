module Extras where

{-
Definir la función divisores utilizando LISTAS POR COMPRENSION tal que "divisores x" 
es la lista de los divisores de x. Por ejemplo, divisores 12 => [1,2,3,4,6,12]

Hint: Crear una función divisible :: Int -> Int -> Bool que verifica i un número
es divisible entre otro
-}

-- Funcion que calcula los divisores de un numero
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], divisible x y]

--Funcion auxiliar de divisores, verifica si un numero es divisible entre otro
divisible :: Int -> Int -> Bool
divisible x y = x `mod` y == 0

{-
Definir la función mezcla tal que "mezcla l1 l2" es la lista ordenada obtenida
al mezclar las listas ordenadas l1 y l2. Por ejemplo,
mezcla [1,3,5] [2,9] => [1,2,3,5,9]
-}

-- Función que mezcla dos listas no ordenadas y devuelve una lista ordenada con los elementos de ambas listas
mezcla :: Ord a => [a] -> [a] -> [a] 
mezcla x y = ordenaElementos z 
    where z = (concatenar x y)
    

-- Funcion auxiliar de mezcla, sirve para ordenar los elementos de una lista
ordenaElementos :: Ord a => [a] -> [a]
ordenaElementos [] = []
ordenaElementos (x:xs) = inserta x (ordenaElementos xs)

-- Funcion auxiliar de ordenaElementos, inserta el elemento e en la lista delante del primer elemento que sea mayor o igual al recibido
inserta :: Ord a => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs)
    | a <= x = a:x:xs
    | otherwise = x : inserta a xs

-- Funcion Auxiliar de mezcla, sirve para concatenar dos listas y de esa forma devolvernos una sola lista con los elementos de ambas listas pero sin ordenar
concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : (concatenar xs ys)





