/* 
Realiza un programa que contenga la base del conocimiento de los signos del zodiaco
tal que su estructura sea la siguiente: signo(Dia, Mes, Signo).
A partir de esto escribir las reglas que permitan calcular el signo del zodiaco para un dia y mes concretos
*/  

% signo(Dia, Mes, Signo).
% Ejemplo de uso: signo(20,2,X).
% X = Piscis
% False
% Ejemplo de uso2: signo(4,1,X).
% X = Capricornio 
% False 

% Base de conocimiento de los signos del zodiaco
horoscopo(acuario, 21, 1, 19, 2).
horoscopo(piscis, 20, 2, 20, 3).
horoscopo(aries, 21, 3, 20, 4).
horoscopo(tauro, 21, 4, 21, 5).
horoscopo(geminis, 22, 5, 21, 6).
horoscopo(cancer, 22, 6, 22, 7).
horoscopo(leo, 23, 7, 22, 8).
horoscopo(virgo, 23, 8, 22, 9).
horoscopo(libra, 23, 9, 22, 10).
horoscopo(escorpio, 23, 10, 21, 11).
horoscopo(sagitario, 22, 11, 21, 12).
horoscopo(capricornio, 22, 12, 19, 1).

% Regla para calcular el signo del zodiaco
signo(Dia, Mes, Signo) :-
    horoscopo(Signo, DiaInicio, MesInicio, DiaFin, MesFin),
    ((Mes = MesInicio, Dia >= DiaInicio, Dia =< 31) ; (Mes = MesFin, Dia =< DiaFin, Dia >= 1)).


%Longitud de una lista, determina el numero de elementos de una lista
/* 
Ejemplo de uso:
longitud([1,2,3,4],X).
X = 4.

longitud([a1,a2,a3,a4,a5],X).
X = 5.
*/
longitud([],0).
longitud([_|Tail],N):- longitud(Tail,N1),N is N1+1.



/* 
Ejemplo de uso:
?- abuelo("Bart Simpson", X).
X = "Abraham J. Simpson".
X = "Clancy Bouvier".

abuela("Bart Simpson", X).
X = "Mona Simpson".

hermano ("Maggie Simpson", X).
X = "Bart Simpson".

hermanas ("Maggie Simpson", X).
X = "Lisa Simpson".

hermanos ("Maggie Simpson", X).
X = "Bart Simpson";
X = "Lisa Simpson".

*/

% Hechos:

% Genero
hombre("Abraham J. Simpson").
hombre("Clancy Bouvier").
hombre("Herbert Powel").
hombre("Homer Simpson").
hombre("Bart Simpson").

mujer("Edwina Simpson").
mujer("Mona Simpson").
mujer("Abbie Simpson").
mujer("Lisa Simpson").
mujer("Maggie Simpson").
mujer("Marge Simpson").
mujer("Jacqueline Bouvier").
mujer("Patty Bouvier").
mujer("Selma Bouvier").
mujer("Ling Bouvier").

% Progenitor
progenitor("Abraham J. Simpson", "Homer Simpson").
progenitor("Abraham J. Simpson", "Herbert Powel").
progenitor("Abraham J. Simpson", "Abbie Simpson").

progenitor("Mona Simpson", "Homer Simpson").
progenitor("Edwina Simpson", "Abbie Simpson").
progenitor("Desconocida", "Herbert Powel").

progenitor("Clancy Bouvier", "Patty Bouvier").
progenitor("Clancy Bouvier", "Selma Bouvier").
progenitor("Clancy Bouvier", "Marge Simpson").

progenitor("Jacqueline Bouvier", "Patty Bouvier").
progenitor("Jacqueline Bouvier", "Selma Bouvier").
progenitor("Jacqueline Bouvier", "Marge Simpson").

progenitor("Selma Bouvier", "Ling Bouvier").

progenitor("Homer Simpson", "Bart Simpson").
progenitor("Homer Simpson", "Lisa Simpson").
progenitor("Homer Simpson", "Maggie Simpson").

progenitor("Marge Simpson", "Bart Simpson").
progenitor("Marge Simpson", "Lisa Simpson").
progenitor("Marge Simpson", "Maggie Simpson").

% Pareja
pareja("Abraham J. Simpson", "Mona Simpson").
pareja("Mona Simpson", "Abraham J. Simpson").
pareja("Clancy Bouvier", "Jacqueline Bouvier").
pareja("Jacqueline Bouvier", "Clancy Bouvier").
pareja("Homer Simpson", "Marge Simpson").
pareja("Marge Simpson", "Homer Simpson").


%Reglas
% Funciona
padre(Hijo,Padre) :-
    progenitor(Padre,Hijo),hombre(Padre).

% Funciona
madre(Hijo,Madre) :-
    progenitor(Madre,Hijo),mujer(Madre).

% Funciona
hermanos(Hermano1,Hermano2) :-
    padre(Hermano1,Y),padre(Hermano2,X),X==Y,Hermano1\=Hermano2.

% Funciona
hermano(Hermano1,Hermano2) :-
    padre(Hermano1,Y),padre(Hermano2,X),hombre(Hermano2),X==Y,Hermano1\=Hermano2.

% Funciona
hermana(Hermano1,Hermano2) :-
    padre(Hermano1,Y),padre(Hermano2,X),mujer(Hermano2),X==Y,Hermano1\=Hermano2.

% Funciona
esposo(Esposa,Esposo) :-
    pareja(Esposo,Esposa),hombre(Esposo).

% Funciona
esposa(Esposo,Esposa) :-
    pareja(Esposo,Esposa),mujer(Esposa).

% Funciona
abuelo(Nieto,Abuelo) :-
    padre(X,Abuelo), (madre(Nieto,X) ; padre(Nieto,X)).

% Funciona
abuela(Nieto,Abuela) :-
    madre(X,Abuela), (madre(Nieto,X) ; padre(Nieto,X)).

% Funciona
suegro(Persona,Suegro) :-
    pareja(Persona,X),padre(X,Suegro).

% Funciona
suegra(Persona,Suegra) :-
    pareja(Persona,X),madre(X,Suegra).

% Funciona 
cuñados(Persona1,Cuñados) :-
    ((pareja(Persona1,X),hermanos(X,Cuñados));(pareja(Cuñados,X),hermanos(X,Persona1))).

% Funciona
cuñado(Persona1,Cuñado) :-
    cuñados(Persona1,Cuñado),mujer(Persona1).

% Funciona
cuñada(Persona1,Cuñada) :-
    cuñados(Persona1,Cuñada),hombre(Persona1).
 
% Funciona 
nieto(Abuelo,Nieto) :-
    (madre(Nieto,X) ; padre(Nieto,X)), padre(X,Abuelo), hombre(Nieto).

% Funciona
nieta(Abuelo,Nieta) :-
    (madre(Nieta,X) ; padre(Nieta,X)), padre(X,Abuelo), mujer(Nieta).

% Funciona
tia(Persona,Tia) :-
    (madre(Persona,X) ; padre(Persona,X)), hermana(X,Tia).

% Funciona
tio(Persona,Tio) :-
    (madre(Persona,X) ; padre(Persona,X)), hermano(X,Tio).

% Funciona
primo(Primo1,Primo2) :-
    (progenitor(X,Primo1),progenitor(Y,Primo2),hermanos(X,Y),hombre(Primo2)).

% Funciona 
prima(Primo1,Primo2) :-
    (progenitor(X,Primo1),progenitor(Y,Primo2),hermanos(X,Y),mujer(Primo2)).
