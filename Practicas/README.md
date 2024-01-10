<div align="center">

# Prácticas 📜🗝️

##   Curso de Lógica Computacional 2023-2
 
###  <em> Prácticas realizadas durante el curso: </em>
</div>

> -  Práctica 01: <em> Introduccion a Haskell. </em>
> -  Práctica 02: <em> Resolucion Binaria. </em>
> -  Práctica 03: <em> Algoritmo DPLL. </em>
> -  Práctica 04: <em> Introduccion a Prolog. </em>
> -  Práctica 05: <em> Algoritmo Hao Wang. </em>
> -  Práctica 06: <em> Introduccion a Coq. </em>




Las prácticas fueron enfocadas a programacion de codigo, para la programacion de codigo se usaron tres lenguajes a lo largo del curso, [Haskell](https://www.haskell.org), [Prolog](https://www.swi-prolog.org) y
[Coq](https://coq.inria.fr) los software´s antes mencionados son de uso libre y se pueden descargar desde 
sus respectivas paginas oficiales.

En el caso de Haskell, la estructura de compilación y ejecución de un programa es:

Haskell :

```Ruby
ghci NombreDelArchivo.hs
```

Habiendo ejecutado el anterior en terminal dentro de la carpeta que alberga el archivo, tendremos una pantalla como la siguiente:

```Ruby
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling NombreDelArchivo ( NombreDelArchivo.hs, interpreted )
Ok, one module loaded.
*NombreDelArchivo>
```

Con esto ya podremos hacer uso de las funciones que esten presentes en el archivo.



En el caso de Prolog, la estructura de compilación y ejecución de un programa es:

Prolog :

```Haskell
swipl NombreDelArchivo.pl 
```

Habiendo ejecutado en terminal el anterior dentro de la carpeta que alberga el archivo, tendremos una pantalla como la siguiente:

```Haskell
Welcome to SWI-Prolog (threaded, 64 bits, version 9.0.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?-
```

Con esto ya podremos hacer uso de las funciones que esten presentes en el archivo.



En el caso de Coq, la estructura de compilación y ejecución de un programa no será como en las anteriores, como Coq 
es un asistente de pruebas formales hecho para escribir definiciones matemáticas, algoritmos ejecutables y teoremas
para el desarrollo semi-interactivo de pruebas comprobadas por máquina.

Tendremos que todo lo que hagamos en Coq deberemos hacerlo desde adentro del IDE de Coq, esto incluye escritura de codigo y su ejecución.

Un ejemplo de compilacion-ejecucion en Coq es el de este [ejercicio extra](https://github.com/CarlosCastanon2099/Logica-Computacional/blob/main/Ejercicios-Extras/practica6/Extra1.v).

![](https://github.com/CarlosCastanon2099/Logica-Computacional/blob/main/GIFS/01.gif)
