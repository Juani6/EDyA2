module P6 where

data Btree a = Empty 
              | Node Int (Btree a) a (Btree a)
              deriving(Show)


inorder :: Btree a -> [a]
inorder Empty = []
inorder (Node _ l x r) = inorder l ++ [x] ++ inorder r

-- nth :: BTree a → Int → a, calcula el n-´esimo elemento de una secuencia.
nth :: Btree a -> Int -> a
nth (Node n l x r) i | szl == i  = x
                     | szl > i   = nth l i
                     | otherwise = nth r (i - szl - 1)
                     where szl = size l

size :: Btree a -> Int
size Empty = 0
size (Node n _ _ _) = n

--  cons :: a → BTree a → BTree a, la cual inserta un elemento al comienzo de la secuencia


--map :: (a → b) → BTree a → BTree b, la cual dada una funci´on f y una secuencia s, devuelve el resultado de
-- aplicar f sobre cada elemento de s.
{- 
mapT :: (a -> b) -> Btree a -> Btree b
mapT f Empty = Empty
mapT f (Node n l x r) = let (l', r') = (mapT f l,mapT f r)
                       in Node n l' (f x) r' -}

--tabulate :: (Int → a) → Int → BTree a, la cual dada una funci´on f y un entero n devuelve una secuencia de
-- tamano n, donde cada elemento de la secuencia es el resultado de aplicar f al ´ındice del elemento.

tabulate :: (Int -> a) -> Int -> Btree a
tabulate f 0 = Empty
tabulate f n = let m = div n 2
                   (l,r) = (tabulate f m, tabulate (\i -> f (i+m+1)) (n-m-1))
                in Node n l (f m) r

--take :: Int → BTree a → BTree a, tal que dados un entero n y una secuencia s devuelve los primeros n
-- elementos de s 
takeS :: Int -> Btree a -> Btree a
takeS _ Empty = Empty
takeS i t@(Node n l x r) | szl >= i       = takeS i l
                         | szl == i - 1   = Node i l x Empty
                         | otherwise      = Node i l x (takeS (i - szl - 1) r)
                          where szl = size l

--drop :: Int → BTree a → BTree a, tal que dados un entero n y una secuencia s devuelve la secuencia s sin los
--primeros n elementos.

dropS :: Int -> Btree a -> Btree a
dropS _ Empty = Empty
dropS i t@(Node n l x r) | szl >= i       = Node (n-i) (dropS i l) x r
                         | szl == i - 1   = r
                         | otherwise      = Node (n-i) Empty x (dropS (i - szl - 1) r)
                          where szl = size l


myTree :: Btree Char
myTree =
  Node 7
    (Node 3
      (Node 1 Empty 'H' Empty)
      'e'
      (Node 1 Empty 'l' Empty))
    'l'
    (Node 3
      (Node 1 Empty 'o' Empty)
      '!'
      (Node 1 Empty '?' Empty))
-- secuencia implícita: H e l l o ! ?  (índices 0..6)


data Tree a = E | Leaf a | Join (Tree a) (Tree a)

mapReduce :: (a->b) -> (b->b->b) -> b -> Tree a -> b
mapReduce m red e E          = e
mapReduce m red _ (Leaf x)   = m x 
mapReduce m red e (Join l r) = let (l',r') = ((mapReduce m red e l), (mapReduce m red e r))
                               in red l' r'

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = let (m,p,s,sum) = mcss' t
         in m

mcss' :: (Num a, Ord a) => Tree a -> (a,a,a,a)
mcss' t =  mapReduce (\x -> (max x 0, max x 0, max x 0, x)) 
                     (\(mL,pL,sL,sumL) (mR,pR,sR,sumR) -> (max mL (max mR (sL+pR)),
                                                           max pL (sumL + pR),
                                                           max sR (sumR + sL),
                                                           sumL + sumR)
                     ) 
                     (0,0,0,0) 
                     t

arbol :: Tree Int
arbol = 
  Join 
    (Join 
      (Leaf (-2)) 
      (Leaf 4)) 
    (Join 
      (Leaf 3) 
      (Leaf (-5)))

arbol2 :: Tree Int
arbol2 = 
  Join
    (Join
      (Join
        (Leaf (-2))
        (Leaf 1))
      (Join
        (Leaf (-3))
        (Leaf 4)))
    (Join
      (Join
        (Leaf (-1))
        (Leaf 2))
      (Join
        (Leaf 1)
        (Join
          (Leaf (-5))
          (Leaf 4))))