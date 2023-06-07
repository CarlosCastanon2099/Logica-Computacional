(*
Extra, definir la función factorial 
Hint: Recuerda que Coq ya tiene una libreria para los nat,
y la función para multiplicar es 'mult', puedes basarte en 
la función even vista en clase
*)

Fixpoint factorial (n:nat) : nat :=
  match n with
  | O => 1
  | S n' => mult n (factorial n')
  end.



Example test_factorial1: (factorial 3) = 6.
Proof. simpl. reflexivity. Qed.

Example test_factorial2: (factorial 5) = (mult 10 12).
Proof. simpl. reflexivity. Qed.
