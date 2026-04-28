module P4 where

import Data.List

{-
1. Si un ´arbol binario es dado como un nodo con dos sub´arboles id´enticos se puede aplicar la t´ecnica sharing para
que los sub´arboles sean representados por el mismo ´arbol. Definir las siguientes funciones de manera que se puedan
compartir la mayor cantidad posible de elementos de los ´arboles creados:
-}



{-
a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero d , crea un ´arbol binario completo
de altura d con el valor x en cada nodo.
-}

data Tree r = BE 
            | Node (Tree r) r (Tree r)
            deriving(Show)

completo :: a -> Int -> Tree a
completo x 0 = BE
completo x 1 = Node BE x BE
completo x d = let tree = completo x (d-1) in Node tree x tree

completo2 :: a -> Int -> Tree a 
completo2 x d = completo2' x d (Node BE x BE)

completo2' :: a -> Int -> Tree a -> Tree a 
completo2' x 1 r = r
completo2' x d r = let r2 = (completo2' x (d-1) r) in Node r2 x r2

{-
b) balanceado :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero n, crea un ´arbol binario balanceado
de tama˜no n, con el valor x en cada nodo.
-}

balanceado :: a -> Int -> Tree a 
balanceado x 0 = BE
balanceado x 1 = Node BE x BE
balanceado x d | mod d 2 == 1 = Node r x r
               | otherwise    = Node r x (balanceado x (d - h - 1))
               where h = div d 2
                     r = balanceado x h

{-
2. Definir las siguientes funciones sobre ´arboles binarios de b´usqueda (bst):
-}

insertarBST :: Ord a => a -> BST a -> BST a
insertarBST x Leaf                          = BNode Leaf x Leaf
insertarBST x (BNode l a r ) | x <= a    = BNode (insertarBST x l) a r 
                             | otherwise = BNode l a (insertarBST x r)

fromList :: Ord a => [a] -> BST a
fromList []      = Leaf
fromList (x:xs)  = insertarBST x (fromList xs)

--1. maximum :: Ord a ⇒ BST a → a, que calcula el m´aximo valor en un bst.

data BST a = Leaf
           | BNode (BST a) a (BST a)
           deriving(Show)

maximum2 :: Ord a => BST a -> a
maximum2 (BNode _ a Leaf) = a
maximum2 (BNode _ _ r) = maximum2 r

minimum2 :: Ord a => BST a -> a
minimum2 (BNode Leaf a _) = a
minimum2 (BNode l _ _) = maximum2 l



-- 2. checkBST :: Ord a ⇒ BST a → Bool, que chequea si un ´arbol binario es un bst.

checkBST :: Ord a => BST a -> Bool
checkBST Leaf                 = True
checkBST (BNode Leaf x Leaf)     = True
checkBST (BNode Leaf x r)     = x < minimum2 r  && checkBST r
checkBST (BNode l x Leaf)     = maximum2 l <= x && checkBST l 
checkBST (BNode l x r)     = (x < minimum2 r && maximum2 l <= x) && checkBST r && checkBST l
                                
{- 3. splitBST :: Ord a ⇒ BST a → a → (BST a, BST a), que dado un ´arbol bst t y un elemento x , devuelva una
tupla con un bst con los elementos de t menores o iguales a x y un bst con los elementos de t mayores a x . -}

splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Leaf x                      = (Leaf,Leaf)
splitBST (BNode l x r) d | x == d = (BNode l x Leaf, r)
                         | d < x  = let (l', r') = splitBST l d
                                    in (l', BNode r' x r)
                         | otherwise  = let (l', r') = splitBST r d
                                    in (BNode l x l', r')

-- 4. join :: Ord a ⇒ BST a → BST a → BST a, que una los elementos dos ´arboles bst en uno.

join :: Ord a => BST a -> BST a -> BST a
join Leaf Leaf                                        = Leaf
join Leaf r                                           = r
join r Leaf                                           = r
join root1@(BNode l1 d1 r1) root2@(BNode l2 d2 r2) = let (r1',r2') = splitBST root2 d1 
                                                     in BNode (join l1 r1') d1 (join r1 r2')


{-
3. La definici´on de member dada en teor´ıa (la cual determina si un elemento est´a en un bst) realiza en el peor
caso 2 ∗ d comparaciones, donde d es la altura del ´arbol. Dar una definici´on de member que realice a lo sumo d + 1
comparaciones. Para ello definir member en t´erminos de una funci´on auxiliar que tenga como par´ametro el elemento
candidato, el cual puede ser igual al elemento que se desea buscar (por ejemplo, el ´ultimo elemento para el cual la
comparaci´on de a 6 b retorn´o True) y que chequee que los elementos son iguales s´olo cuando llega a una hoja del
´arbol.
-}

member :: Ord a => a -> BST a -> Bool
member x r = member' x x r

member' :: Ord a => a -> a -> BST a -> Bool
member' x c Leaf                      = if x == c then True else False 
member' x c (BNode l d r) | x < d     = member' x c l
                          | otherwise = member' x d r


data Color = R | B deriving(Show)
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT x r = memberRBT' x x r

memberRBT' :: Ord a => a -> a -> RBT a -> Bool
memberRBT' x c E                       = if x == c then True else False 
memberRBT' x c (T _ l d r) | x < d     = memberRBT' x c l
                           | otherwise = memberRBT' x d r

balanceL :: Color -> RBT a -> a -> RBT a -> RBT a
balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceL c l a r                     = T c l a r

balanceR :: Color -> RBT a -> a -> RBT a -> RBT a
balanceR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceR B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceR c l a r                     = T c l a r

insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT x t = makeBlack (ins x t)

makeBlack :: RBT a -> RBT a
makeBlack E = E
makeBlack (T _ l x r) = T B l x r

ins :: Ord a => a -> RBT a -> RBT a 
ins x E = T R E x E
ins x rt@(T c l y r) | x < y = balanceL c (ins x l) y r
                     | x > y = balanceR c l y (ins x r)
                     | otherwise = rt

fromListRBT :: Ord a => [a] -> RBT a
fromListRBT []     = E
fromListRBT (x:xs) = insertRBT x (fromListRBT xs)


lista = [15,10,13,11,12]
lista2 = [0,18,17,16,2,1,3]
bst = fromList lista
bst2 = fromList lista2  

{-
5. Los ´arboles 1-2-3 son ´arboles binarios de b´usqueda donde los nodos pueden guardar m´ultiples valores y tener
entre 2 y 4 hijos.
Espec´ıficamente, en un ´arbol 1-2-3 los nodos internos son de la forma:
2-node : Contienen un valor y dos hijos.
3-node : Contienen dos valores y tres hijos.
4-node : Contienen tres valores y cuatro hijos.
-}

data T123 a = T0 
            | T1 a (T123 a) (T123 a)
            | T2 a a (T123 a) (T123 a) (T123 a)
            | T3 a a a (T123 a) (T123 a) (T123 a) (T123 a)
            deriving(Show)

-- 2. Definir una funci´on que transforme red-black trees en ´arboles 1-2-3. Paralelizar cuando sea posible

rbtTot123 :: Ord a => RBT a -> T123 a
rbtTot123 E = T0
rbtTot123 (T B (T R ll dl rl) d (T R lr dr rr)) = T3 dl d dr (rbtTot123 ll) (rbtTot123 rl) (rbtTot123 lr) (rbtTot123 rr)
rbtTot123 (T B l d (T R l' d' r'))              = T2 d d' (rbtTot123 l) (rbtTot123 l') (rbtTot123 r')
rbtTot123 (T B (T R l' d' r') d r)              = T2 d' d (rbtTot123 l') (rbtTot123 r') (rbtTot123 r) 
rbtTot123 (T B l d r)                           = T1 d (rbtTot123 l) (rbtTot123 r) 

type Rank = Int
data Heap a = EH
            | N Rank a (Heap a) (Heap a)
            deriving(Show)

rank :: Heap a -> Rank
rank EH          = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b | rank a >= rank b = N (rank b + 1) x a b
            | otherwise        = N (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 EH = h1
merge EH h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y
                                          then makeH x a1 (merge b1 h2)
                                          else makeH y a2 (merge h1 b2)

{-
6. Definir una funci´on fromList :: [a ] → Heap a, que cree un leftist heap a partir de una lista, convirtiendo cada
elemento de la lista en un heap de un solo elemento y aplicando la funcion merge hasta obtener un solo heap. Aplicar
la funcion merge n veces, donde n es la longitud de la lista que recibe como argumento la funci´on.
-}

fromListHeap :: Ord a => [a] -> Heap a
fromListHeap xs = foldr merge EH (map (\x -> N 1 x EH EH) xs)

{-
7. Un pairing heap es un ´arbol general que satisface el invariante de heap.
Para implementar pairing heap en Haskell definimos el siguiente tipo de datos:
-}
data PHeaps a = Empty | Root a [PHeaps a] deriving(Show)


--1. isPHeap :: Ord a ⇒ PHeaps a → Bool, determina si un arbol es un pairing heap, es decir cumple con el invariante de heap

minList :: Ord a => [PHeaps a] -> a
minList (x:xs) = let Root d xs' = x 
                 in minList' d xs 

minList' :: Ord a => a -> [PHeaps a] -> a 
minList' a []     = a
minList' a (x:xs) = let Root d xs' = x 
                    in if d <= a 
                       then minList' d xs 
                       else minList' a xs



isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty       = True
isPHeap (Root x []) = True
isPHeap (Root x xs) = if x <= (minList xs) 
                      then (foldr (&&) True (map isPHeap xs))
                      else False
{- 2. merge :: Ord a ⇒ PHeaps a → PHeaps a → PHeaps a, que una dos pairing heap. Para ello, comparar las raıces
de ambos arboles y elegir la menor como raız del nuevo heap, agregar el ´arbol con mayor ra´ız como hijo de este. -}

mergeP :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
mergeP h1 Empty                      = h1
mergeP Empty h2                      = h2
mergeP h1@(Root x xs) h2@(Root y ys) = if x <= y 
                                      then Root x (h2 : xs)
                                      else Root y (h1 : ys)

-- 3. insert :: Ord a ⇒ PHeaps a → a → PHeaps, que inserte un elemento en un pairing heap

insertP :: Ord a => PHeaps a -> a -> PHeaps a
insertP h1 x = mergeP h1 (Root x [])

--4. concatHeaps :: Ord a ⇒ [PHeaps a ] → PHeaps a, que dada una lista de pairing heaps construya otro con los elementos del mismo.

concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps xs = foldr mergeP Empty xs

{-
5. delMin :: Ord a ⇒ PHeaps a → Maybe (a, PHeaps a), que dado un pairing heap, devuelva si el arbol no es
vacıo un par con el menor elemento y un pairing heap sin este elemento, o Nothing en otro caso.
-}


minList2 :: Ord a => [PHeaps a] -> PHeaps a
minList2 (x:xs) = minList2' x xs 

minList2' :: Ord a => PHeaps a -> [PHeaps a] -> PHeaps a 
minList2' x []     = x
minList2' x (y:ys) =  if dato x <= dato y 
                        then minList2' x ys 
                        else minList2' y ys

dato :: PHeaps a -> a
dato (Root d _) = d

hijos :: PHeaps a -> [PHeaps a]
hijos (Root _ xs) = xs

eliminar :: Ord a => PHeaps a -> [PHeaps a] -> [PHeaps a]
eliminar x [] = []
eliminar x (y:ys) = if dato x == dato y 
                    then ys
                    else y : eliminar x ys

delMin :: Ord a => PHeaps a -> Maybe ( a ,PHeaps a )
delMin Empty = Nothing
delMin r = Just (delMin' r)

delMin' :: Ord a => PHeaps a -> (a,PHeaps a)
delMin' (Root d xs) = let r = minList2 xs 
                      in (d,Root (dato r) ((hijos r) ++ eliminar r xs))


---------------------------------------------- TEST ---------------------------------------------- 
heapList :: [PHeaps Int]
heapList = [Root 1 [],Root 2 [],Root 3 [],Root 4 [],Root 5 [],Root 6 []]

heap2 :: PHeaps Int
heap2 = Root 1
  [ Root 2
      [ Root 4 []
      , Root 5 []
      ]
  , Root 3
      [ Root 6 []
      , Root 7 []
      ]
  ]