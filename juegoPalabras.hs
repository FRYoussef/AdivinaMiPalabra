--Autor: Youssef El Faqir El Rhazoui

--De momento solo funciona con minusculas
--Que llegue o no a la solución final dependerá en gran medida de la función reOrdena

--Ejemplos que funcionan: [ie, nb, kk, ] bien 6
-- [pa, la, cc, br, z, ] palabra 4

--Nota: es importante poner despues de la última palabra ", " porque sino el splitOn concatena caracteres raros 


import Data.Char
import Data.List.Split

--Desplazo todos los elementos de la lista una posición a la izq, y el primero lo desplazo al final
reOrdena:: [Char] -> [Char]
reOrdena [] = []
reOrdena (x:[]) = [x]
reOrdena (x1:x2:[]) = (x2:x1:[])
reOrdena (x1:x2:x3:xs) = (x2:x3:reOrdena (x1:xs))

--Concatena dos palabras
concatena:: [Char] -> [Char] -> [Char]
concatena [] [] = []
concatena xs [] = xs
concatena xs ys = xs ++ ys

--Nos quedamos con las letras coincidentes
interseca:: [Char] -> [Char] -> [Char]
interseca [] yss = []
interseca (x:xs) yss = if elem x yss then (x:interseca xs yss) else interseca xs yss

--Nos quedamos con las letras del primer conjunto que no estan en el segundo
resta:: [Char] -> [Char] -> [Char]
resta [] yss = []
resta (x:xs) yss = if elem x yss then resta xs yss else (x:resta xs yss)

--Desplazaremos las letras una posicion en el diccionario, por ejemplo: a -> b
desplaza:: [Char] -> [Char]
desplaza [] = []
desplaza (x:xs) = let y = fromEnum x + 1 in if y == 123 then 'a':(desplaza xs) else (chr y):(desplaza xs)


solucion:: [[Char]] -> [Char] -> Int -> [[Char]]
--tomo k como la profundidad máxima

solucion [] ys k = []

--Si hemos agotado las palabras reserva, solo nos queda reordenar lo que tenemos
-- con el fin de llegar a la solución 
solucion (x:[]) ys k 
  | k == 0 = ["\nNo he podido llegar a la palabra destino\n"]
  | k > 0 = if palabra == ys then palabra:"\nExito\n":[] else palabra:(solucion [palabra] ys (k-1))
    where 
      palabra = reOrdena (interseca x ys)



-- Para k pares intersecamos y concatenamos
-- Para k impares restamos, desplazamos y concatenamos
solucion (x1:x2:xs) ys k
  | k == 0 = ["\nNo he podido llegar a la palabra destino\n"]
  | k `mod` 2 == 0 = if palabraPar == ys then palabraPar:[] else palabraPar:(solucion (palabraPar:xs) ys (k-1))
  | k `mod` 2 /= 0 = if palabraImpar == ys then palabraImpar:"\nExito\n":[] else palabraImpar:(solucion (palabraImpar:xs) ys (k-1))
     where
       palabraPar = concatena palabraIntersecada x2
       palabraIntersecada = interseca x1 ys

       palabraImpar = concatena x1 palabraDesplazada
       palabraDesplazada = desplaza palabraRestada
       palabraRestada = resta x2 ys

--Muestra el mensaje que se le pasa
muestraMsj:: String -> IO ()
muestraMsj [] = return ()
muestraMsj (x:xs) = putChar x >>= \_ -> muestraMsj xs

--Lee una cadena por consola
getLinea::IO String
getLinea = getChar >>= \c ->
      if c=='\n' then return []
                 else getLinea >>= \cs ->
                      return (c:cs)

--Lee un entero por consola
getInt::  IO Int
getInt = do line <- getLinea
            return (read line::Int)

--Le llega una lista de strings y devuelve un string de las concatenaciones
--de la lista bien formateado
formateaString:: [[Char]] -> [Char]
formateaString (x:[]) = x ++ "\n"
formateaString (x:xs) = x ++ ", " ++ formateaString xs 



--Maneja la interaccion con el usuario
juegoPalabras:: IO ()
juegoPalabras = 
  do muestraMsj "\nIntroduce la palabra destino (\"salir\" para salir): "
     palabraDestino <- getLinea
     if palabraDestino == "salir" then return ()
     else do muestraMsj "\nIntroduce la profundidad maxima para buscar: "
             profundidad <- getInt
             do muestraMsj "\nIntroduce palabras para el repertorio (separadas por comas): "
                repertorio <- getLinea
                muestraMsj (formateaString (solucion (splitOn ", " repertorio) palabraDestino profundidad))
                juegoPalabras