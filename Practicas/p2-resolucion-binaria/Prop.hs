module Prop where


data Prop = Var String | Cons Bool| Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Eq)

-- 1
-- Dada una fórmula, devuelve su forma normal negativa.
-- Siguiendo el algoritmo de la clase de primero quitar equivalencias, luego implicaciones 
-- y por último meter negaciones.
fnn :: Prop -> Prop
fnn p = meteNegacion (quitaImpl (quitaSyss p))

-- Funcion auxiliar que elimina las equivalencias de una formula
quitaSyss :: Prop -> Prop
quitaSyss (Var p) = (Var p)
quitaSyss (Cons b) = (Cons b)
quitaSyss (Not p) = Not (quitaSyss p) 
quitaSyss (And p q) = And (quitaSyss p) (quitaSyss q) 
quitaSyss (Or p q) = Or (quitaSyss p) (quitaSyss q) 
quitaSyss (Impl p q) = Impl (quitaSyss p) (quitaSyss q) 
quitaSyss (Syss p q) = And (Impl (quitaSyss p) (quitaSyss q)) (Impl (quitaSyss q) (quitaSyss p))

-- Funcion auxiliar que elimina las implicaciones de una formula
quitaImpl :: Prop -> Prop
quitaImpl (Var p) = (Var p)
quitaImpl (Cons b) = (Cons b)
quitaImpl (Not p) = Not (quitaImpl p) 
quitaImpl (And p q) = And (quitaImpl p) (quitaImpl q) 
quitaImpl (Or p q) = Or (quitaImpl p) (quitaImpl q) 
quitaImpl (Impl p q) = Or (Not (quitaImpl p)) (quitaImpl q)
quitaImpl (Syss p q) = And (Impl (quitaImpl p) (quitaImpl q)) (Impl (quitaImpl q) (quitaImpl p))

-- Funciones auxiliares que aplican la doble negacion a una formula y la distribucion de conjunciones/disyunciones
-- Para de esta forma si nos encontamos con por ejemplo un (¬¬p) lo convertimos en p y este no se quede como (¬¬p)
meteNegacion :: Prop -> Prop
meteNegacion (Var p) = (Var p)
meteNegacion (Cons b) = (Cons b)
meteNegacion (Not p) = meteNegacion' p
meteNegacion (And p q) = And (meteNegacion p) (meteNegacion q) 
meteNegacion (Or p q) = Or (meteNegacion p) (meteNegacion q) 
 
meteNegacion' :: Prop -> Prop
meteNegacion' (Var p) = Not (Var p)
meteNegacion' (Cons b) = (Cons b)
meteNegacion' (Not p) = meteNegacion p 
meteNegacion' (And p q) = Or (meteNegacion' p) (meteNegacion' q) 
meteNegacion' (Or p q) = And (meteNegacion' p) (meteNegacion' q) 

-- 2
-- Dada una fórmula, devuelve su forma normal conjuntiva.
-- Siguiendo el algoritmo de primero obtener su forma normal negativa
-- Para despues distribuir las disyunciones
fnc :: Prop -> Prop
fnc p = distribuyeDisyunciones (fnn p)

-- Funcion auxiliar que distribuye las disyunciones de una formula que ya esta en fnn 
distribuyeDisyunciones :: Prop -> Prop
distribuyeDisyunciones (Or (And x y) q) = distribuyeDisyunciones(And (Or (distribuyeDisyunciones x) (distribuyeDisyunciones q))(Or (distribuyeDisyunciones y) (distribuyeDisyunciones q)))
distribuyeDisyunciones (Or p (And g1 g2)) = distribuyeDisyunciones(And (Or (distribuyeDisyunciones p) (distribuyeDisyunciones g1))(Or (distribuyeDisyunciones p) (distribuyeDisyunciones g2)))
distribuyeDisyunciones (And p q) = And (distribuyeDisyunciones p) (distribuyeDisyunciones q)
distribuyeDisyunciones (Or p q)
   | contieneDisyuncion (Or p q) = distribuyeDisyunciones(Or (distribuyeDisyunciones p)(distribuyeDisyunciones q))
   | otherwise = Or (distribuyeDisyunciones p)(distribuyeDisyunciones q)
distribuyeDisyunciones p = p

-- Funcion auxiliar que nos dice si una proposicion contiene una disyuncion
contieneDisyuncion:: Prop -> Bool
contieneDisyuncion (And p q) = True
contieneDisyuncion (Or p q) = (contieneDisyuncion p) || (contieneDisyuncion q)
contieneDisyuncion _ = False
 