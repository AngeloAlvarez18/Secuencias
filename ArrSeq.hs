module ArrSeq where

import qualified Arr as A
import Seq
import Par
import Arr (length)
import Arr (tabulate)
import Arr (flatten)
import Arr (subArray)
import Arr (fromList)


-- Función auxiliar para scanS la cual aplica la operación cada dos elementos del array,
-- si el array posee un nro impar de elementos, el ultimo se retorna sin ser operado. 
contract :: (a -> a -> a) -> A.Arr a -> A.Arr a
contract f arr = tabulateS g n' 
  where 
    n = lengthS arr
    n' = if (mod n 2 == 0) then (div n 2) else (div n 2)+1
    g i = if 2*i+1 == n then (nthS arr 2*i) else f (nthS arr (2*i)) (nthS arr 2*i+1) -- chequeo si puedo operar dos elementos del array
                                                                                     -- si no puedo, agg el elemento restante al array sin operarlo

-- Funcion auxiliar para scanS la cual toma dos arrays y utiliza un algoritmo para 
-- obtener un unico array, resultado del scan.
-- El algoritmo consiste en ir formando el nuevo array elemento por elemento:
-- si la posicion a rellenar en el nuevo array es par entonces tomo el i/2 elemento del 2do array,
-- si la posición es impar entonces aplicamos la operacion al piso(i/2)-ésimo del 2do array y al
-- (i-1)-ésimo del primero.
expand :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expand f arr arr' = tabulateS g (lengthS arr) where g i | mod i 2 == 0 = nthS arr' (div i 2)
                                                        | otherwise = f (nthS arr' (div i 2)) (nthS arr (i-1))

instance Seq A.Arr where
  emptyS = A.empty
  
  singletonS x = (A.fromList [x])

  lengthS = A.length

  nthS = (A.!)

  tabulateS = A.tabulate
  
  mapS f arr 
    | (lengthS arr) == 0 = emptyS
    | otherwise = let (x, ar) = f (nthS arr 0) ||| mapS f (dropS arr 1) 
                  in (appendS (singletonS x) ar)

  takeS arr n = A.subArray 0 (n-1) arr

  dropS arr n = A.subArray n (A.length arr - n) arr

  filterS p arr 
    | (lengthS arr) == 0 = emptyS
    | otherwise = let (b, ar) = p (nthS arr 0) ||| filterS p (dropS arr 1) 
                  in if b then appendS (singletonS (nthS arr 0)) ar else ar

  appendS arr1 arr2 = A.flatten (A.fromList [arr1, arr2])

  showtS arr = case lengthS arr of
    0 -> EMPTY
    1 -> ELT (nthS arr 0)
    _ -> NODE (takeS arr (n - x)) (dropS arr (n - x)) 
      where n = lengthS arr
            x = div n 2

  showlS arr = case lengthS arr of
    0 -> NIL
    _ -> CONS (nthS arr 0) (dropS arr 1)
      where n = lengthS arr

  joinS = A.flatten

  reduceS f e arr 
    | n == 0 = arr
    | n == 1 = f e (nthS arr 1)
    | otherwise = reduceS f e (contract f arr)
      where n = lengthS arr

  scanS f e arr 
    | n == 0 = (emptyS, e)
    | n == 1 = (singletonS e, f e x)
    | otherwise = let (arr', r) = scanS f e (contract f arr) in (expand f arr arr', r)
      where n = lengthS arr
            x = nthS arr 0      

  fromList = A.fromList
