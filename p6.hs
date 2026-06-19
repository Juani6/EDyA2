module P6 where
{- 
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
-- secuencia implícita: H Empty l l o ! ?  (índices 0..6)


data Tree a = Empty 
            | Leaf a 
            | Join (Tree a) (Tree a)
            deriving(Show)

mapReduce :: (a->b) -> (b->b->b) -> b -> Tree a -> b
mapReduce m red e Empty          = e
mapReduce m red _ (Leaf x)       = m x 
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

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

sufijos :: Tree Int -> Tree (Tree Int)
sufijos t = suf t Empty

suf :: Tree Int -> Tree Int -> Tree (Tree Int)
suf (Leaf x) aux   = Leaf aux
suf (Join l r) Empty         = Join (suf l r) (Leaf Empty)
suf (Join l r) aux = Join (suf l (Join r aux)) (Leaf aux) 


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty          = Empty
mapTree f l@(Leaf x) = Leaf (f x) 
mapTree f (Join l r) = let (l',r') = (mapTree f l, mapTree f r)
                       in Join l' r'


masIzq :: Tree a -> a
masIzq Empty = error ("HOLI")
masIzq (Leaf x)   = x
masIzq (Join l r) = masIzq l

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Leaf x) (Leaf y)         = Leaf (x,y)
zipTree (Leaf x) t@(Join l r)     = Leaf (x, masIzq t)
zipTree t@(Join l r) (Leaf x)     = Leaf (masIzq t, x)
zipTree (Join ll rl) (Join lr rr) = Join (zipTree ll lr) (zipTree rl rr) -- Esto es paralelizable pero me dio paja

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos t = zipTree t (sufijos t) 

maxT :: Tree Int -> Int
maxT t = mapReduce (\i -> i) max (masIzq t) t

podar :: Tree (Tree a) -> Tree (Tree a)
podar (Join l (Leaf Empty)) = l
podar (Join (Leaf Empty) r) = r

maxAll :: Tree (Tree Int) -> Int
maxAll t = let t' = podar t in mapReduce maxT max (maxT (masIzq t')) t'  


mejorGanancia :: Tree Int -> Int
mejorGanancia s = let s'  = conSufijos s -- Tree (Int, Tree Int)
                      s'' = mapTree (\(x,y) -> mapTree (\z -> z - x) y) s'
                  in maxAll s''
 -}
{- 
arbol3 :: Tree Int
arbol3 = Join l r

l :: Tree Int
l = (Join (Leaf 10) (Leaf 15))

r :: Tree Int
r =  Leaf 20

t :: Tree Int
t = Join
      (Join
        (Join (Leaf 3) (Leaf 1))
        (Join (Leaf 4) (Leaf 2)))
      (Join
        (Join (Leaf 5) (Leaf 8))
        (Join (Leaf 6) (Leaf 7)))
 -}

-------------------------------- Ejercicio 4
(|||) a b = (a,b)

data T a = E | N (T a) a (T a) deriving(Show)

altura :: T a -> Int
altura E = 0
altura (N l x r) = 1 + max (altura l) (altura r)

combinar :: T a -> T a -> T a
combinar E d2  = d2
combinar d1 E  = d1
combinar d1@(N l x r) d2 = N (combinar l r) x d2

filterT :: (a -> Bool) -> T a -> T a
filterT p E         = E
filterT p (N l x r) = let (l',r') = filterT p l ||| filterT p r
                      in if p x then (N l' x r') else combinar l' r'

-- Wfilter(h) = 1 + 2 filter(h-1) + O(h-1)

quickSort :: T Int -> T Int
quickSort E = E
quickSort t@(N l x r) = let (l',r') = filterT (<x) t ||| filterT (>x) t
                        in (N (quickSort l') x (quickSort r'))

--PROFUNDIDAD
-- T(h) = 1 + max{T(h-1),T(h-1)} + O(h²) = 1 + T(h-1) + O(h²) 
--TRABAJO
-- W(n) = 1 + W(floor(n/2)) + W(ceil(n/2)) + O(n) <= 1 + 2 W(ceil(n/2)) + O(n) 

-- Árbol 1: balanceado con [3, 1, 4, 2, 5]
t1 :: T Int
t1 = N
       (N (N E 3 E) 1 (N E 4 E))
       2
       (N E 5 E)

       -- Árbol 2: más completo con [6, 2, 8, 1, 4, 7, 9]
t2 :: T Int
t2 = N
       (N (N E 6 E) 2 (N E 8 E))
       1
       (N (N E 4 E) 7 (N E 9 E))

t :: T Int
t = N
      (N
        (N (N E 7 E) 2 (N E 9 E))
        5
        (N (N E 1 E) 8 (N E 3 E)))
      6
      (N
        (N (N E 4 E) 11 (N E 2 E))
        9
        (N (N E 7 E) 3 (N E 5 E)))