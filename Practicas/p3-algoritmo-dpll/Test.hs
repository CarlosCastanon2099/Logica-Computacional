module Test where

import Parser
import Prop
import DPLL

mugiCheck :: Bool -> IO ()
mugiCheck a
  | a = putStrLn "Correcto! Kaizoku ou ni ore wa naru"
  | otherwise = putStrLn "Mal! Donificaste a Ace :C"

ej1 = parser $ lexer "(duerme ∧ corre) ∨ (¬duerme ∧ salta)"
ej2 = parser $ lexer "duerme ∧ ¬duerme"
ej3 = parser $ lexer "(ronca ↔ duerme) ∧ (¬ronca ↔ ¬duerme)"
ej4 = parser $ lexer "(p → q) ∧ p ∧ ¬q"
ej5 = parser $ lexer "((corre → suda) ∨ (corre → cansarse)) ↔ (corre ∧ ¬suda ∧ ¬cansarse)"
ej6 = parser $ lexer "T → ((p ↔ q) ∧ ((p ∧ ¬q) ∨ (¬p ∧ q) ∨ ⊥))"


t1 =  (dpll $ clausulas $ fnc ej1) /= [] -- Map.empty
t2 =  (dpll $ clausulas $ fnc ej2) == []
t3 =  (dpll $ clausulas $ fnc ej3) /= []
t4 =  (dpll $ clausulas $ fnc ej4) == []
t5 =  (dpll $ clausulas $ fnc ej5) == []
t6 =  (dpll $ clausulas $ fnc ej6) == []

-- generateTree (transformaClausulaEnEstado $ clausulas $ fnc ej6)
-- getLastEstado (aplicaSepBranch (transformaClausulaEnEstado $ clausulas $ fnc ej3))
-- esSatisfacible $ getLastEstado (aplicaSepBranch (transformaClausulaEnEstado $ clausulas $ fnc ej3))

main = do
    putStrLn "Tests"
    mugiCheck t1
    mugiCheck t2
    mugiCheck t3
    mugiCheck t4
    mugiCheck t5
    mugiCheck t6
    