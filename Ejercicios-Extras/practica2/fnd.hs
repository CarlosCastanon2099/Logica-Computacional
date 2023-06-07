data Prop = Var String | Cons Bool| Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Show, Eq)

fnd :: Prop -> Prop
fnd p = distribuyeConjunciones (fnn p)

distribuyeConjunciones :: Prop -> Prop
distribuyeConjunciones (And (Or x y) q) = distribuyeConjunciones(Or (And (distribuyeConjunciones x) (distribuyeConjunciones q))(And (distribuyeConjunciones y) (distribuyeConjunciones q)))
distribuyeConjunciones (And p (Or x1 x2)) = distribuyeConjunciones(Or (And (distribuyeConjunciones p) (distribuyeConjunciones x1))(And (distribuyeConjunciones p) (distribuyeConjunciones x2)))
distribuyeConjunciones (Or p q) = Or (distribuyeConjunciones p) (distribuyeConjunciones q)
distribuyeConjunciones p = p




fnn :: Prop -> Prop
fnn p = meteNegacion (quitaImpl (quitaSyss p))

quitaSyss :: Prop -> Prop
quitaSyss (Var p) = (Var p)
quitaSyss (Not p) = Not (quitaSyss p) 
quitaSyss (And p q) = And (quitaSyss p) (quitaSyss q) 
quitaSyss (Or p q) = Or (quitaSyss p) (quitaSyss q) 
quitaSyss (Impl p q) = Impl (quitaSyss p) (quitaSyss q) 
quitaSyss (Syss p q) = And (Impl (quitaSyss p) (quitaSyss q))(Impl (quitaSyss q) (quitaSyss p))

quitaImpl :: Prop -> Prop
quitaImpl (Var p) = (Var p)
quitaImpl (Not p) = Not (quitaImpl p) 
quitaImpl (And p q) = And (quitaImpl p) (quitaImpl q) 
quitaImpl (Or p q) = Or (quitaImpl p) (quitaImpl q) 
quitaImpl (Impl p q) = Or (Not (quitaImpl p)) (quitaImpl q) 

meteNegacion :: Prop -> Prop
meteNegacion (Var p) = (Var p)
meteNegacion (Not p) = meteNegacion' p
meteNegacion (And p q) = And (meteNegacion p) (meteNegacion q) 
meteNegacion (Or p q) = Or (meteNegacion p) (meteNegacion q) 
 
meteNegacion' :: Prop -> Prop
meteNegacion' (Var p) = Not (Var p)
meteNegacion' (Not p) = meteNegacion p 
meteNegacion' (And p q) = Or (meteNegacion' p) (meteNegacion' q) 
meteNegacion' (Or p q) = And (meteNegacion' p) (meteNegacion' q) 


 