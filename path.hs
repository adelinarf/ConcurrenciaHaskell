{-Dado un path que representa un directorio en el sistema operativo, cuenta la
cantidad de archivos que están localizados en el subarbol que tiene como raíz el
directorio propuesto.
El proceso debe crear un thread por cada subdirectorio encontrado.-}

import Control.Concurrent
import Control.Monad (filterM, replicateM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (joinPath, (</>))

path = "./"              --Modificar esta variable path para que busque en el directorio deseado
main = directorio path   --Se llama a la funcion directorio que se encarga de buscar el numero de archivos en el directorio path

cantidadArchivos :: Int -> Int -> Int
cantidadArchivos x y 
                   | x < y = y - x  --Esta funcion retorna la cantidad de archivos de un directorio 
                   | y < x = x - y  --Un valor es la cantidad de elementos y otro la cantidad de subdirectorios, ya que puede ser
                   | x == y = x     --uno menor que el otro o iguales, dependiendo del directorio que se busque.

directorio :: FilePath -> IO ()
directorio path = do
  archivos <- listDirectory path  --Se buscan todos los archivos y subdirectorios que se encuentran en el path
  subdirectorios_ <- filterM (doesDirectoryExist . (path </>)) archivos  --Se filtran unicamente los subdirectorios
  let subdirectorios = map (path </>) subdirectorios_ --Se agrega el nombre del path al inicio de los subdirectorios
  mv <- newMVar 0                                     --Se crea una MVar para manejar la suma de la cantidad de archivos que tiene cada hilo
  lista <- replicateM (length subdirectorios) newEmptyMVar  --Se crea una lista de MVar para guardar si un hilo ya termino o no
  let listaF = zip subdirectorios lista                    --Se unen las listas de MVar y la lista de subdirectorios disponibles
  hilos <- traverse (\l -> forkIO $ directorios l mv) listaF   --Se crean los hilos, para cada subdirectorio
  polling lista    --Se busca para verificar si los hilos han terminado
  answer <- takeMVar mv   --Se toma el valor de la MVar
  putStrLn $ "El directorio " ++ path ++ " tiene "   --Imprime el numero de archivos que hay en el directorio
    ++ show (answer + (cantidadArchivos (length subdirectorios) (length archivos))) ++ " archivos"
  mapM_ killThread hilos  --Se terminan todos los hilos

directorios :: (FilePath, MVar Bool) -> MVar Int -> IO ()  --Esta funcion es llamada por cada uno de los hilos para verificar 
directorios (path, termine) mv = do                        --los archivos que se encuentran en un subdirectorio
  archivos <- listDirectory path                           --Es similar a la funcion superior, debido a que genera mas hilos si contiene 
  subdirectorios_ <- filterM (doesDirectoryExist . (path </>)) archivos --subdirectorios y produce otra lista de MVar para cada uno de esos
  let subdirectorios = map (path </>) subdirectorios_
  lista <- replicateM (length subdirectorios) newEmptyMVar
  let listaF = zip subdirectorios lista
  hilos <- traverse (\l -> forkIO $ directorios l mv) listaF  --Crea mas hilos en caso de que existan subdirectorios
  polling lista  --Espera para verificar que los hilos hijos de este han culminado
  mapM_ killThread hilos  --Termina los hilos hijo
  a <- takeMVar mv  --Toma la variable MVar que aloja las sumas y le suma la cantidad de archivos de este directorio unicamente
  putMVar mv (a + (cantidadArchivos (length subdirectorios) (length archivos)))  --Multiplica los valo
  putMVar termine True --indica que el hilo ya termino

polling :: [MVar Bool] -> IO ()
polling [] = pure ()
polling (t : ts) = do --Esta funcion espera a que un hilo termine verificando la lista de hilos y sus MVar
  takeMVar t
  polling ts
