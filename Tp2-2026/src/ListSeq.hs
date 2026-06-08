module ListSeq where

import Par
import Seq

instance Seq [] where

  emptyS = []

  singletonS x = [x]

  lengthS s = length s

  nthS s k = s !! k

  tabulateS f n = map f [0..n-1]

  mapS f s = map f s

  filterS p s = filter p s

  appendS s t = s ++ t

  takeS s n = take n s

  dropS s n = drop n s

  showtS []  = EMPTY
  showtS [x] = ELT x
  showtS s   = let mid = div (lengthS s) 2
                 (l,r) = takeS s mid ||| dropS s mid
               in NODE l r 

  showlS []     = NIL
  showlS (x:xs) = CONS x xs

  joinS []  = []
  joinS [x] = x
  joinS s   = let mid = div (lengthS s) 2
                (l,r) = takeS s mid ||| dropS s mid
              in joinS l ++ joinS r

  reduceS f e []              = e 
  reduceS f e [x]             = f e x
  reduceS f e s = let pp      = floor (logBase 2 (fromIntegral (lengthS s))) 
                      (l,r)   = takeS s pp ||| dropS s pp
                      (l',r') = reduceS f e l ||| reduceS f e r
                  in f l' r'

  scanS f e []  = ([], e)
  scanS f e [x] = ([e], f e x)
  scanS f e s   = let c         = contraer s
                      (s', res) = scanS f e c
                      s''       = tabulateS (\i -> if even i 
                                                   then nthS s' (div i 2) 
                                                   else f (nthS s' (div i 2))  (nthS s (i-1))) (lengthS s)
                  in (s'', res)
                  where contraer []         = []
                        contraer [x]        = [x]
                        contraer (x:xs:xss) = let (l,r) = (f x xs) ||| (contraer xss)
                                              in l : r 
  fromList s = s

fview :: String -> String -> String
fview s t = "(" ++ s ++ "+" ++ t ++ ")"

-- ghci> scanS fview "E" ["x1","x2","x3"]
-- (["E","(E+x1)","(E+(x1+x2))"],"(E+((x1+x2)+x3))")