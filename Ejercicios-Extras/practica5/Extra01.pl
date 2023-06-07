/* 
Ejercicio Extra
Implementar una regla que nos diga si el predicado max(LS,X) es verdadero
donde X es un numero y LS una lista de numeros.
Este sera verdadero si X es el elemento mas grande de la lista LS.
*/  

% Ejemplo 
% max([1,2,3,4,5,99,1,3,4],X).
% X = 99;
% false.

% max(LS,X) :- X es el elemento mas grande de la lista LS.
max([X],X).
max([X|XS],X) :- max(XS,Y), X >= Y.
max([X|XS],Y) :- max(XS,Y), X < Y.








