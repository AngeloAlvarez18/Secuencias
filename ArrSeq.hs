import qualified Arr as A

import Seq
import Par

instance Seq A.Arr where
    emptyS = A.empty

    singletonS a = A.fromList [a]

    lengthS = A.length

    nthS = (A.!)

    takeS s i = A.subArray 0 i s

    dropS s i = A.subArray i (lengthS s) s

    showtS s | lengthS s == 0 = EMPTY
             | lengthS s == 1 = ELT (nthS s 0)
             | lengthS s > 1 = NODE (takeS s (div (lengthS s) 2)) (dropS s (div (lengthS s) 2))

    showlS s | lengthS s == 0 = NIL
             | lengthS s >= 1 = CONS (nthS s 0) (dropS s 1)

    appendS a1 a2 = A.flatten (A.fromList [a1, a2])

    joinS = A.flatten

    tabulateS = A.tabulate

    mapS f arr 
    | (lengthS arr) == 0 = emptyS
    | otherwise = let (x, ar) = f (nthS arr 0) ||| mapS f (dropS arr 1) 
                  in (appendS (singletonS x) ar)

    filterS p arr 
    | (lengthS arr) == 0 = emptyS
    | otherwise = let (b, ar) = p (nthS arr 0) ||| filterS p (dropS arr 1) 
                  in if b then appendS (singletonS (nthS arr 0)) ar else ar

    reduceS 


