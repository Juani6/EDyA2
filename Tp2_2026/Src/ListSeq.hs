--module Tp2_2026.Src.ListSeq where
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
  showtS s   = let mid   = div (lengthS s) 2
                   (l,r) = takeS s mid ||| dropS s mid
               in NODE l r 

  showlS []     = NIL
  showlS (x:xs) = CONS x xs

  joinS []  = []
  joinS [x] = x
  joinS s   = let mid   = div (lengthS s) 2
                  (l,r) = takeS s mid ||| dropS s mid
              in joinS l ++ joinS r

  reduceS f e [] = e 
  reduceS f e s  = f e (red s)
                 where red [x] = x
                       red s   = red (contraer f e s)


  scanS op e s = case s of
    []  -> ([], e)
    [x] -> ([e], e `op` x)
    _   ->
      let
        (cont, sLen) = contract s [] op
        (s', red)    = scanS op e cont
        r            = expand s s' 0 sLen op
      in (r, red)
  
  fromList s = s

contract :: [a] -> [a] -> (a -> a -> a) -> ([a], Int)
contract []     ys _  = (ys, 0)
contract (x:xs) [] op =
  let (rest, n) = contract xs [x] op
  in (rest, n + 1)
contract (x:xs) (y:ys) op =
  let
    ((rest, n), res) = contract xs ys op ||| (y `op` x)
  in
    (res : rest, n + 1)

expand :: [a] -> [a] -> Int -> Int -> (a -> a -> a) -> [a]
expand _ [] _ _ _ = []
expand st@(x:s) st'@(y:s') i n op
    | i == n         = []
    | i `mod` 2 == 0 = y : expand st st' (i + 1) n op
    | otherwise      =
        let
          (res, rest) = (y `op` x) ||| expand (dropS s 1) s' (i + 1) n op
        in
          res : rest

fview :: String -> String -> String
fview s t = "(" ++ s ++ "+" ++ t ++ ")"

contraer :: (a -> a -> a) -> a -> [a] -> [a]
contraer f e []         = []
contraer f e [x]        = [x]
contraer f e (x:xs:xss) = let (l,r) = (f x xs) ||| (contraer f e xss)
                          in l : r

-- ghci> scanS fview "E" ["x1","x2","x3"]
-- (["E","(E+x1)","(E+(x1+x2))"],"(E+((x1+x2)+x3))")