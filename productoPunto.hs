-- Dadas dos vectores del mismo tamaño (representados como un arreglo), realizar
-- el producto punto
import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Foldable (traverse_)

main = do
  let 
    x = [1,2,3,4,5,5]    --x e y son los vectores que se utilizaran para calcular el producto punto, modificar estos valores 
    y = [3,4,5,7,8,8]    --para hacer el calculo con vectores diferentes
  if length x == length y then productoPunto x y    --Se verifica que los vectores sean del mismo tamano
  else print "Los vectores no son del mismo tamano"

productoPunto x y = do
  mv <- newMVar 0    --Se crea una variable para alojar las sumas de cada una de las multiplicaciones que haran los hilos
  lista <- replicateM (length x) newEmptyMVar --Se crea una lista de variables MVar que definen si el hilo o no ha terminado
  let 
    operadores = zip x y  --Se unen las posiciones del vector en una tupla
    listaF     = zip operadores lista --Se crea una lista de tuplas de tuplas con los valores a multiplicar en una tupla y el MVar del hilo
  hilos <- traverse (\l -> forkIO $ operar mv l) listaF  --Se crean los hilos, la misma cantidad como elementos tiene el vector
  polling lista  --Se espera a que cada uno de los hilos termine
  answer <- takeMVar mv  --Se toma la respuesta del mv que guarda el producto punto
  print answer
  mapM_ killThread hilos --Se terminan todos los hilos

polling :: [MVar Bool] -> IO ()
polling [] = pure ()
polling (t:ts) = do
  takeMVar t    --Esta funcion busca el MVar booleano de cada hilo y verifica si ha terminado o no
  polling ts 

operar :: MVar Int -> ((Int,Int),MVar Bool) -> IO ()
operar mv ((x,y),termine) = do
  a <- takeMVar mv
  putMVar mv (a + x*y)   --Esta funcion toma la tupla que corresponde a un hilo y calcula la multiplicacion de los valores x e y de la tupla
  putMVar termine True   --Y actualiza la MVar de la tupla a True cuando termina de operar
