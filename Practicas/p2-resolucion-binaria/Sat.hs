module Sat where

import Prop
import Data.List

type Literal = Prop
type Clausula = [Literal]

-- 1
-- Dada una fórmula en FNC, devuelve una lista con las cláusulas que la forman.
clausulas :: Prop -> [Clausula]
clausulas p = map getLits(listaLiterals(fnc p))
getLits(Or p q) = getLits p ++ getLits q
getLits p = [p]


-- Funcion auxiliar que devuelve una lista con los literales de una formula
listaLiterals :: Prop -> [Literal]
listaLiterals (And p q) = listaLiterals p ++ listaLiterals q
listaLiterals p = [propALiteral p]


-- Funcion auxiliar que devuelve el literal de una formula apartir de una prop
propALiteral :: Prop -> Literal
propALiteral (Not p) = Not p
propALiteral p = p


-- 2
-- Dadas dos cláusulas, devuelve el resolvente obtenido después de aplicar la regla de resolución binaria. 
-- Se puede asumir que se puede obtener un resolvente a partir de los argumentos
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 = 
  let
    compLit = encuentraComplementario c1 c2
    in case compLit of
        Nothing -> eliminaDuplicados(union c1 c2)
        Just(l1,l2) -> eliminaDuplicados(delete l2 (delete l1 (union c1 c2)))


-- Funcion auxiliar que elimina los elementos duplicados de una lista
eliminaDuplicados :: Clausula -> Clausula
eliminaDuplicados [] = [] 
eliminaDuplicados (x:xs) = x:eliminaDuplicados(filter(/= x)xs)


-- 3
-- Determina si es posible obtener un resolvente a partir de dos cláusulas.
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = if encuentraComplementario c1 c2 == Nothing then False else True


-- Funcion auxiliar que devuelve el literal complementario de una clausula si es que se encuentra
encuentraComplementario :: Clausula -> Clausula -> Maybe (Literal,Literal)
encuentraComplementario _[] = Nothing
encuentraComplementario []_ = Nothing
encuentraComplementario (l:ls) c2 =
    if elem (complementario l) c2
        then Just (l, complementario l)
    else encuentraComplementario ls c2


-- Funcion auxiliar que devuelve el literal complementario/contrario de una formula
complementario :: Literal -> Literal
complementario (Not l) = l
complementario l = Not l


-- 4
-- Dada una fórmula proposicional, determina si esta es satisfacible o no usando el algoritmo de saturación.
saturacion :: Prop -> Bool
saturacion p 
    | clausulaEvaluador (resolucionMultiple (clausulas p) ) == Cons False = False
    | otherwise = True 


-- Funcion auxiliar que toma una clausula y quita los complementos de los literales
--[Not (Var "suda"),Not (Var "cansarse")],[Var "suda",Var "cansarse"] -> False operador
clausulaEvaluador :: Clausula -> Literal
clausulaEvaluador c = 
  case eliminaDuplicados(filter (\x -> not (elem (complementario x ) c)) c ) of
    [Cons False, Cons True] -> Cons False
    [Cons True, Cons False] -> Cons False
    [] -> Cons False
    (l:_) -> Cons True


-- Funcion auxiliar que aplica las respectivas resoluciones a una lista de clausulas
resolucionMultiple :: [Clausula] -> Clausula
resolucionMultiple [] = []
resolucionMultiple [c] = c
resolucionMultiple (c1:c2:cs) =
  let nuevaClausula = resolucion c1 c2
  in resolucionMultiple (nuevaClausula:cs)

