module Tp2_2026.Src.ArrSeq where

import qualified Arr as A
import Tp2_2026.Src.Seq
import Tp2_2026.Src.Par

instance Seq A.Arr where

  emptyS = A.empty

  singletonS x = A.fromList [x]

  lengthS s = A.length s

  --nthS :: A.Arr a -> Int -> a
  nthS s k = s A.! k 

  tabulateS f n = A.tabulate f n

  mapS f s = tabulateS (\i -> f (nthS s i)) (lengthS s)

  filterS p s = A.flatten (mapS ((\t -> if p (nthS t 0) then t else emptyS) . singletonS) s)

  appendS s t = let (lenS,lenT) = lengthS s ||| lengthS t
                in tabulateS (\i -> if i < lenS then nthS s i else nthS t i) (lenS+lenT)

  takeS s k = A.subArray 0 k s

  dropS s k = A.subArray k (lengthS s - k) s
  
  showtS s | lengthS s == 0 = EMPTY
           | lengthS s == 1 = ELT (nthS s 0)
           | otherwise      = let mid   = div (lengthS s) 2
                                  (l,r) = takeS s mid ||| dropS s mid
                              in NODE l r

  showlS s | 0 == lengthS s = NIL
           | otherwise      = CONS (nthS s 0) (dropS s 1)

  joinS s = A.flatten s

  reduceS f e s | 0 == lengthS s = e
                | otherwise      = f e (red s)
                where red s | lengthS s == 1 = nthS s 0
                            | otherwise      = red (contraer f e s)
                            
  scanS f e s | lengthS s == 0   = (emptyS,e)
              | lengthS s == 1   = (singletonS e, f e (nthS s 0))
              | otherwise        =  let c = contraer f e s
                                        (s', res) = scanS f e c
                                        t = tabulateS (\i -> if even i 
                                                              then (nthS s' (div i 2)) 
                                                              else f (nthS s' (div i 2)) (nthS s (i-1))) (lengthS s)
                                    in (t , res)
                                
  fromList xs = A.fromList xs 

contraer :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a
contraer f e s  | 0 == lengthS s = emptyS
                | 1 == lengthS s = singletonS (f e (nthS s 0))
                | otherwise      = tabulateS (\i -> if 2*i+1 < lengthS s
                                                    then f (nthS s (2 * i)) (nthS s (2 * i + 1))
                                                    else nthS s (2*i)) (div (lengthS s + 1) 2)  