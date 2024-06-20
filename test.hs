import Par
import Seq

emptyS = []

singletonS a = [a]

lengthS = length

nthS (x:xs) n = if n == 0 then x else nthS (n-1) 0

tabulateS f n = map f [1..n]

mapS f [] = []
mapS f (x:xs) = (f x) : mapS f xs

filterS f [] = []
filterS f (x:xs) = if (f x) then x : filterS f xs else filterS f xs

appendS a b = a ++ b

takeS s i = take i s

dropS s i = drop i s

showtS s | length s == 0 = EMPTY
        | length s == 1 = ELT (head s) 
        | length s > 1 = NODE (take (div (length s) 2) s) (drop (div (length s) 2) s)


showlS s | lengthS s == 0 = NIL
         | lengthS s > 1 = CONS (nthS s 0) (dropS s 1)


joinS [] = []
joinS (x:xs) = appendS x (joinS xs)


reduceS f e (x:xs) = case (showtS (x:xs)) of
    EMPTY -> e
    ELT a -> a
    NODE l r -> let (l', r') = reduceS f e l ||| reduceS f e r
                in f l' r'


contract f e [] = []
contract f e [x] = [f e x]
contract f e (x:y:xs) = let z = f x y
                        in z : contract f e xs

expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand f xs [] = []
expand f (x:z:xs) [y] = y : (f y x) : []
expand f (x:z:xs) (y:ys) = y : (f y x) : (expand f xs ys)


a = [1+2, 3+4, 5+6, 7+8] -- [3, 7, 11, 15]
b = [0, 0 + 1+2+3+4] -- [0, 10]

-- 0 : 0+1+2 : 0+1+2+3+4 : (0+1+2+3+4+5+6) [0, 3, 10, 21]



scanS f e [] = (emptyS, e)
scanS f e [x] = (singletonS e, f e x)
scanS f e xs = let (s', r) = scanS f e (contract f e xs) in (expand f xs s', r)
