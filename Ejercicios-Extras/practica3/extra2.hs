import Data.List -- Extra

data Prop = Var String | Cons Bool| Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Eq)

instance Show Prop where
    show p = case p of
        (Var s) -> s
        (Cons b) -> show b
        (Not p) -> "¬" ++ (show p)
        (And p q) -> "(" ++ (show p) ++ " ∧ " ++ (show q) ++ ")"
        (Or p q) -> "(" ++ (show p) ++ " ∨ " ++ (show q) ++ ")"
        (Impl p q) -> "(" ++ (show p) ++ " → " ++ (show q) ++ ")"
        (Syss p q) -> "(" ++ (show p) ++ " ↔ " ++ (show q) ++ ")"

data Literal = V String | NotV String deriving (Eq, Ord) -- Ord es el hack para el Extra 
type Clausula = [Literal]

instance Show Literal where
    show p = case p of
                (V s) -> s
                (NotV s) -> "¬" ++ s



type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [ Clausula ])




-- Extra
-- Dada una formula proposicional, devuelve la literal que se repite mas veces en toda la clausula
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral f = head $ maximumBy cmp $ group $ sort $ concat f
    where cmp x y = compare (length x) (length y)






{-
heuristicsLiteral :: [Clausula] -> Literal 
heuristicsLiteral [] = error "No hay clausulas"
heuristicsLiteral (x:xs) = heuristicsLiteral' x (heuristicsLiteral xs)

heuristicsLiteral' :: Clausula -> Literal -> Literal
heuristicsLiteral' [] l = l
heuristicsLiteral' (x:xs) l = if (length (filter (==x) (x:xs))) > (length (filter (==l) (x:xs))) then heuristicsLiteral' xs x else heuristicsLiteral' xs l
-}
