# Problemas de Concurrencia en Haskell
### Pregunta 2.b

## Pregunta 2.b.i 

#### Dados dos vectores del mismo tamaño (representados como un arreglo), realizar el producto punto. El cálculo debe hacerse de forma concurrente, aprovechando los mecanismos provistos en el lenguaje para ello.

Este programa se encuentra en el archivo productoPunto.hs

## Pregunta 2.b.ii 

#### Dado un path que representa un directorio en el sistema operativo, cuenta la cantidad de archivos que están localizados en el subarbol que tiene como raíz el directorio propuesto. El proceso debe crear un thread por cada subdirectorio encontrado.

Este programa se encuentran en el archivo path.hs

## Corrida
La corrida se hace de manera habitual:

    ghc -o nombre nombreDeArchivo.hs
    ./nombre
