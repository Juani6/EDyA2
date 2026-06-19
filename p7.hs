-- module P7 where

-- import Tp2_2026.Src.Seq
-- import Tp2_2026.Src.ListSeq

-- promedios :: Seq s => s Int -> s Int
-- promedios s = let (seq,res) = scanS (+) 0 s
--                   s' = dropS (appendS seq (singletonS res)) 1
--                   in tabulateS (\i -> div (nthS s' i) (i+1)) (lengthS s')

-- mayores :: Seq s => s Int -> Int
-- mayores s = let s'  = tabulateS (\i -> (nthS s i, nthS s (i+1))) (lengthS s - 1)
--                 s'' = mapS (\i -> max (fst i) (snd i)) s' 
--             in reduceS (+) 0 (fun s s'') 
--             where fun :: Seq s => s Int -> s Int -> s Int
--                   fun s t = if even (lengthS t) 
--                             then (tabulateS (\i -> if (nthS s (i+1)) == (nthS t i) 
--                                               then 1
--                                               else 0) (lengthS t))
--                             else (tabulateS (\i -> if (nthS s (i+1)) == (nthS t i) 
--                                               then 1
--                                               else 0) (lengthS t-1))

-- fibSeq :: Seq s => Int -> s Int
-- fibSeq n = mapS (\(a,b,c,d) -> b) (appendS (fst t') (singletonS (snd t')))
--            where t = tabulateS (\i -> (1,1,1,0)) n
--                  t' = (scanS prodM (1,0,1,0) t)
--                  prodM :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
--                  prodM (a,b,c,d) (e,f,g,h) = (a*e+g*b, f*a+b*h, e*c+g*d, f*c+h*d)
-- {- 
-- aguaHist :: Seq s => s Int -> Int
-- aguaHist s = reduceS (+) 0 s'
--              where (seq1,res1) = scanS max 0 s
--                    sL  = appendS seq1 (singletonS res1)
--                    len = lengthS s
--                    (seq2,res2) = scanS max 0 (tabulateS (\i -> nthS s (len - 1 - i)) len)
--                    sR  = appendS seq2 (singletonS res2)
--                    f i =  max 0 ( (min (nthS sL i) (nthS sR (len - 1 - i))) - (nthS s i))
--                    s'  = tabulateS (\i -> f i) len :: s Int -}




-- s :: [Int]
-- s = [2,4,3,7,9,4,47,2,67,-1]
-- s2 :: [Int]
-- s2 = [1,2,5,3,5,2,7,9]
