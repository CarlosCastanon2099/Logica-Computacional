/* 
Ejercicio Extra
Implementar la regla de fibonacci (N,X) tal que X será el N-ésimo número de la serie de Fibonacci.
1, 1, 2, 3, 5, 8, 13, 21, 34 ...
*/  

% fibonacci(X,Y).
% Ejemplo de uso: fibonacci(5,X). 
% X=5 
% Ejemplo de uso2: fibonacci(7,X). 
% X=13
fibonacci(0,0).
fibonacci(1,1).
fibonacci(X,Y) :- X > 1, X1 is X-1, X2 is X-2, fibonacci(X1,Y1), fibonacci(X2,Y2), Y is Y1+Y2.


