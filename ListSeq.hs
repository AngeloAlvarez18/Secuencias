module ListSeq where

import Par
import Seq

-- Función auxiliar para scanS y reduceS la cual aplica la operación cada dos elementos de la lista,
-- si la lista posee un nro impar de elementos, el ultimo se retorna sin ser operado.
contract :: (a -> a -> a) -> [a] -> [a]
contract f [] = []
contract f [x] = [x]
contract f (x:y:xs) = let (z, zs) = f x y ||| contract f xs
                      in z : zs

-- Funcion auxiliar para scanS la cual toma dos listas y utiliza un algoritmo para 
-- obtener una unica, resultado del scan.
-- El algoritmo consiste en ir formando la nueva lista elemento por elemento:
-- si la posicion a rellenar en la lista resultante es par entonces tomo el i/2 elemento de la 2da lista,
-- si la posición es impar entonces aplicamos la operacion al piso(i/2)-ésimo de la 2da lista y al
-- (i-1)-ésimo del primero.
-- La funcion tambien considera si la primera lista ingresada es de longitud par o impar.
expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand f xs [] = []
expand f l@(x:xs) [y] = if mod (lengthS l) 2 == 0 then y : (f y x) : [] else y : []
expand f (x:z:xs) (y:ys) = let (z:zs) = (f y x) ||| expand f xs ys
                           in y : z : zs

instance Seq [] where
  emptyS = []

  singletonS a = [a]

  lengthS = length

  nthS (x:xs) n = if n == 0 then x else (nthS xs (n-1))

  tabulateS f n = map f [1..n]

  mapS f [] = []
  mapS f (x:xs) = let (x', xs') = f x ||| mapS f xs
                  in x' : xs'

  filterS f [] = []
  filterS f (x:xs) = let (b, xs') = f x ||| filterS f xs
                     in if b then x:xs' else xs'

  appendS a b = a ++ b

  takeS s i = take i s

  dropS s i = drop i s

  showtS s | lengthS s == 0 = EMPTY
           | lengthS s == 1 = ELT (nthS s 0)
           | lengthS s > 1 = NODE (take (div (length s) 2) s) (drop (div (length s) 2) s)

  showlS [] = NIL
  showlS (x:xs) = CONS x xs

  joinS [] = []
  joinS (x:xs) = appendS x (joinS xs)

  reduceS f e [] = []
  reduceS f e [x] = f e x
  reduceS f e xs = reduceS f e (contract f xs)

  scanS f e [] = (emptyS, e)
  scanS f e [x] = (singletonS e, f e x)
  scanS f e xs = let (s', r) = scanS f e (contract f xs) in (expand f xs s', r)

  fromList = id
