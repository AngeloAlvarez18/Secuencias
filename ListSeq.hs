import Par
import Seq


contract f e [] = []
contract f e [x] = [f e x]
contract f e (x:y:xs) = let z = f x y
                        in z : contract f e xs

expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand f xs [] = []
expand f (x:z:xs) [y] = y : (f y x) : []
expand f (x:z:xs) (y:ys) = y : (f y x) : (expand f xs ys)

instance Seq [] where
    emptyS = []

    singletonS a = [a]

    lengthS = length

    nthS (x:xs) n = if n == 0 then x else nthS (n-1) 0

    tabulateS f n = mapS f [1..(n-1)]

    mapS f [] = []
    mapS f (x:xs) = let (r, ls) = (f x) ||| mapS f xs
                    in r:ls

    filterS f [] = []
    filterS f (x:xs) = let (r, ls) = (f x) ||| filterS f xs
                        in if r then x : ls else ls

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

    reduceS f e s = case showtS of
        EMPTY -> e
        ELT a -> a
        NODE l r -> let (l', r') = reduceS f e l ||| reduceS f e r
                    in f l' r'

    scanS f e [] = (emptyS, e)
    scanS f e [x] = (singletonS e, f e x)
    scanS f e xs = let (s', r) = scanS f e (contract f e xs) in (expand f xs s', r)

-- Wfilter(n) = W(n-1) + W(f)  ---> W(n) = Sum(W(f x), x pertenece xs)

-- Wreduce(n) = 2W(n/2) + W(take) + W(drop) + W(f) 

-- SI f pertenece O(1) entonces Wreduce(n) = O(n log n)