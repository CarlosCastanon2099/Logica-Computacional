module DPLL where

import Prop
import Data.List 
            
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [ Clausula ])

-- Esta practica consiste en la implementacion del algoritmo DPLL para la satisfaccion de formulas proposicionales.


-- 1
-- Funcion que verifica si estamos ante el escenario de conflicto en el algoritmo DPLL
-- Por lo tanto, la funcion determina si la claúsula vacía, representada como una lista vacía, forma parte del conjunto de claúsulas. 
-- Puesto que no existe modelo que satisfaga a la claúsula vacía, la búsqueda del modelo falla.
-- Ejemplo de uso: conflict ([] , [[p,q] , [r,s] , [] , [p,r,t]]) = True 
-- conflict ([] , [[V "p", V "q"] , [V "r", V "s"] , [] , [V "p", V "r", V "t"]])
conflict :: Estado -> Bool
conflict (i, c) = if null' c then True else False

-- Funcion Auxiliar que busca una lista vacia en una lista de listas
-- Ejemplo de uso: null' [[1,2,3],[4,5,6],[]] = True
null' :: [[a]] -> Bool
null' [] = False
null' (x:xs) = null x || null' xs


-- 2
-- Funcion que verifica si estamos ante el escenario de exito en el algoritmo DPLL
-- Determina si la búsqueda del modelo ha sido exitosa, esto sucede cuando el conjunto de cláusulas es vacío
-- Ejemplo de uso: success ([(p,True),(q,False)],[]) = True
-- success ([("p",True),("q",False)],[])
success :: Estado -> Bool
success (_, []) = True 

-- 3
-- Funcion que aplica la regla unitaria al conjunto de clausulas
unit :: Estado -> Estado
unit (interpretacion, clausulas) =
    case find (\c -> length c == 1) clausulas of
        Nothing -> (interpretacion, clausulas)
        Just unitaria -> let lit = head unitaria
                             nuevaInterpretacion = case lit of
                                                      (V s) -> [(s, True)]
                                                      (NotV s) -> [(s, False)]
                             clausulaNueva = filter (/= unitaria) clausulas
                         in (nuevaInterpretacion ++ interpretacion, clausulaNueva)

-- 4
-- Funcion que elimina las clausulas que contienen el literal que se le pasa como parametro
-- Ejemplo de uso: elim ([( p , True ) ] , [[p,q] , [r,s] , [p,r,t]]) = ([(p , True)] , [[r,s]])
-- Notese que esto es por que el literal p es True, por lo tanto se eliminan las clausulas que contengan a p y se mantienen las demas
-- elim ([( "p" , True ) ] , [[V "p",V "q"] , [V "r", V "s"] , [V "p", V "r", V "t"] , [V "c", V "m"]])
elim :: Estado -> Estado
elim (i, c) = (i, filter (not . containsAssignedLiteral i) c)

containsAssignedLiteral :: Interpretacion -> Clausula -> Bool
containsAssignedLiteral i c = any (\l -> isAssigned i l) c

isAssigned :: Interpretacion -> Literal -> Bool
isAssigned i (V s) = maybe False id (lookup s i)
isAssigned i (NotV s) = maybe False (not . id) (lookup s i)


-- Funcion Auxiliar que busca las clausulas que contienen el literal que se le pasa como parametro
buscaLiteral :: Literal -> [Clausula] -> [Clausula]
buscaLiteral l c = filter (\x -> not (elem l x)) c

-- Funcion auxiliar que obtiene la literal de una interpretacion
literalInterp :: Interpretacion -> Literal
literalInterp i = V (fst' (head i))


-- 5
-- Funcion que a partir de la literal de la interpretacion del estado
-- devuelve un nuevo estado pero con sus clausulas modificadas en el aspecto de que 
-- se eliminan de las clausulas involucradas a la literal complementaria de la literal de la interpretacion
-- cabe resaltar que si una clausula contiene esto, no se debe eliminar la clausula, sino que se debe eliminar solamente la literal complementaria
-- red ([( p , True ) ] , [[not p,q] , [r,s] , [p,r,t]]) = ([(p , True)] , [[q] , [r,s] , [r,t]])
-- red ([( "p" , True ) ] , [[NotV "p",V "q"] , [V "r",V "s"] , [V "p", V "r", V "t"]])
red :: Estado -> Estado
red (i, c) = (i, map (filterClauses (oppositeLiterals i)) c)

oppositeLiterals :: Interpretacion -> [Literal]
oppositeLiterals i = map (\(s, b) -> if b then NotV s else V s) i

filterClauses :: [Literal] -> Clausula -> Clausula
filterClauses _ [] = []
filterClauses ol (x:xs)
  | x `elem` ol = filterClauses ol xs
  | otherwise = x : filterClauses ol xs



-- Funcion auxiliar que devuelve el complemento de una literal
complemento :: Literal -> Literal
complemento (V x) = NotV x
complemento (NotV x) = V x


-- Funcion que dada una literal pertenecinete a la clausula en un estado inicial
-- devuelve dos nuevos estados en los que el primero sera el estado con la interpretacion de verdadero para la literal
-- y el segundo sera el estado con la interpretacion de falso para la literal
-- sep p ([] , [[ p , q ]]) = (([(p , True)] , [[p,q]]) , ([(p , False)] , [[p,q]]))
-- sep (V "p") ([] , [[ V "p" , V "q" ]])
sep :: Literal -> Estado -> (Estado, Estado)
sep (V x) e = (agregaInterp [(x, True)] e, agregaInterp [(x, False)] e)
sep (NotV x) e = (agregaInterp [(x, False)] e, agregaInterp [(x, True)] e)



-- Funcion Auxiliar sep' la cual a partir de solamente un estado devuelve dos nuevos estados y en donde
-- nuestra literal la obtendremos de la funcion buscaLiteralParaSep
-- sep' ([] , [[p,q] , [r,s] , [p,r,t]]) = (([(p , True)] , [[p,q] , [r,s] , [p,r,t]]) , ([(p , False)] , [[p,q] , [r,s] , [p,r,t]]))
-- sep' ([] , [[V "p",V "q"] , [V "r",V "s"] , [V "p", V "r", V "t"]])
sep' :: Estado -> (Estado, Estado)
sep' e = sep (buscaLiteralParaSep e) e


-- Funcion Auxiliar que busca una literal entre las clausulas de un estado para seleccionarla como literal a separar
-- Priorizando que su clausula no tenga mas de dos o tres literales y que no sea una clausula unitaria
-- Ejemplo de uso: buscaLiteral ([] , [[p,q] , [r,s,m] , [p,r,t]]) = p
-- buscaLiteral ([] , [[V "p",V "q"] , [V "r",V "s",V "m"] , [V "p", V "r", V "t"]]) 
buscaLiteralParaSep :: Estado -> Literal
buscaLiteralParaSep (i, c) = head $ head $ filter (\x -> length x == 1) $ group $ sort $ concat c



-- Funcion Auxiliar que convierte literales a su forma de string
-- Ejemplo de uso: literalToString (V "p") = "p"
-- literalToString (NotV "p") = "-p"
literalToString :: Literal -> String
literalToString (V x) = x
literalToString (NotV x) = "¬" ++ x


-- Funcion auxiliar que agrega una interpretacion a un estado
agregaInterp :: Interpretacion -> Estado -> Estado
agregaInterp i (i', c) = (i ++ i', c)

-- Extra
-- Dada una formula proposicional, devuelve la literal que se repite mas veces en toda la clausula
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral f = head $ maximumBy cmp $ group $ sort $ concat f
    where cmp x y = compare (length x) (length y)

-- Funcion Auxiliar que devuelve el primer elemento de una tupla.
-- Ejemplo de uso: fst' (1,2) = 1
fst' :: (a, b) -> a
fst' (x, _) = x

-- Funcion Auxiliar que devuelve el segundo elemento de una tupla.
-- Ejemplo de uso: snd' (12,53) = 53
snd' :: (a, b) -> b
snd' (_, y) = y

data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Eq
-- data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving (Eq, Show) 


-- {- 
-- Definición de show para imprimir el árbol en la forma deseada
instance Show ArbolDPLL where
  show arbol = cadenaArbolDPLL arbol 0


-- Función auxiliar para generar la cadena de un árbol en la forma deseada
cadenaArbolDPLL :: ArbolDPLL -> Int -> String
cadenaArbolDPLL Void _ = ""
cadenaArbolDPLL (Node estado hijo) nivel = replicate (nivel * 4) ' ' ++ "Node " ++ show estado ++ "\n" ++
                                           cadenaArbolDPLL hijo (nivel + 1)
cadenaArbolDPLL (Branch estado hijoIzq hijoDer) nivel = cadenaArbolDPLL hijoDer (nivel + 1) ++
                                                         replicate (nivel * 4) ' ' ++ "Branch " ++ show estado ++ "\n" ++
                                                         cadenaArbolDPLL hijoIzq (nivel + 1)


-- -}


-- Algoritmo DPLL, usa ArbolDPLL para saber si para una clausula hay una interpretacion que la satisfaga
-- En caso de que la fórmula no sea satisfacible, la función deberá devolver una lista vacía.
-- En caso de que la fórmula sea satisfacible, la función deberá devolver una lista de literales que satisfacen la fórmula.
-- Usando las funciones existentes en este documento y aplicando elim, red, unit y sep, se deberá generar un árbol de búsqueda
dpll :: [Clausula] -> Interpretacion
dpll f = dpll' (deClausulaAArbol f) 


-- Funcion que atravez de una clausula genera un estado de esta
transformaClausulaEnEstado :: [Clausula] -> Estado
transformaClausulaEnEstado clausulas = ([], clausulas)

-- Funcion que genera un arbol a partir de una clausula
deClausulaAArbol :: [Clausula] -> ArbolDPLL
deClausulaAArbol clausulas = estadoToInitialNode (transformaClausulaEnEstado clausulas)

aplicaSepBranch :: Estado -> ArbolDPLL
aplicaSepBranch estado = if estado == getLastEstado (generateTree estado) then sepBranch (estadoToInitialNode estado) else generateTree estado


dpll' :: ArbolDPLL -> Interpretacion
dpll' (Node estado _) = 
    let arbol = aplicaSepBranch estado
    in if esSatisfacible (getLastEstado arbol) then
        listaVacia
    else
        listaNovacia
    where listaVacia = []
          listaNovacia = [("No vacia :D", False)]

-- 
esSatisfacible :: Estado -> Bool
esSatisfacible (i, c) = if c == [[]] then True else False


estadoToInitialNode :: Estado -> ArbolDPLL
estadoToInitialNode estado = Node estado Void


-- Funcion que realiza unit, elim y red al mismo tiempo
lafuncionUER :: Estado -> Estado
lafuncionUER estado = elim(red(unit estado))


applyOperations :: ArbolDPLL -> ArbolDPLL
applyOperations (Node estado _) =                                                      
    let unitEstado = lafuncionUER estado
    in Node unitEstado Void


generateTree :: Estado -> ArbolDPLL
generateTree estado =
    let rootNode = estadoToInitialNode estado
        applyNode = applyOperations rootNode
    in case applyNode of
        Node childState _ ->
            let childNode = estadoToInitialNode childState
            in if childState == estado then
                childNode
            else
                Branch estado childNode (generateTree childState)
        Void -> rootNode

getLastEstado :: ArbolDPLL -> Estado
getLastEstado (Node estado Void) = estado
getLastEstado (Node estado child) = getLastEstado child
getLastEstado (Branch _ _ child) = getLastEstado child


getLastNode :: ArbolDPLL -> ArbolDPLL
getLastNode (Node estado Void) = Node estado Void
getLastNode (Node estado child) = getLastNode child
getLastNode (Branch _ _ child) = getLastNode child

-- Funcion que aplica la regla de separacion a un arbol
sepBranch :: ArbolDPLL -> ArbolDPLL
sepBranch Void = Void
sepBranch (Node estado Void) =
    let (leftEstado, rightEstado) = sep (heuristicsLiteral (snd estado)) estado
        leftNode = Node leftEstado Void
        rightNode = Node rightEstado Void
        leftNode' = applyOperationsOnTree leftNode
        rightNode' = applyOperationsOnTree rightNode
    in case (leftNode', rightNode') of
        (Node _ _, Node _ _) -> 
            let Branch _ leftChild' rightChild' = Branch (fst leftEstado, snd estado) leftNode' rightNode'
            in Branch (fst leftEstado, snd estado) (applyOperationsOnTree leftChild') (applyOperationsOnTree rightChild')
        (Void, Node _ _) -> rightNode'
        (Node _ _, Void) -> leftNode'
        (Void, Void) -> Void
sepBranch (Node estado child) =
    let child' = sepBranch child
    in case child' of
        Void -> Void
        _ -> Node estado child'
sepBranch (Branch estado left right) =
    let left' = sepBranch left
        right' = sepBranch right
    in case (left', right') of
        (Void, _) -> right'
        (_, Void) -> left'
        (_, _) -> Branch estado left' right'

applyOperationsOnTree :: ArbolDPLL -> ArbolDPLL
applyOperationsOnTree (Node estado child) =
    let unitEstado = lafuncionUER estado
        child' = applyOperationsOnTree child
    in case child' of
        Node childState _ ->
            let childNode = estadoToInitialNode childState
            in if childState == estado then
                childNode
            else
                Branch estado childNode (generateTree childState)
        Void -> Node unitEstado Void
applyOperationsOnTree Void = Void

applyOperationsOnTree (Branch estado child1 child2) =
    let child1' = applyOperationsOnTree child1
        child2' = applyOperationsOnTree child2
    in Branch estado child1' child2'


