module Lab02 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List

-- 1) Dada la siguiente definición para representar árboles binarios:

data BTree a = E | Leaf a | Node (BTree a) (BTree a)

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E           = 0
altura (Leaf _)    = 0
altura (Node a b)  = 1 + max (altura a) (altura b)

--t :: BTree Int
--t = (Node (Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 3) (Leaf 9))) (Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))))

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos
-- y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E          = True
perfecto (Leaf _)   = True
perfecto (Node E _) = False
perfecto (Node _ E) = False
perfecto (Node a b) = altura a == altura b && perfecto a && perfecto b


-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E          = []
inorder (Leaf a)   = [a]
inorder (Node a b) = inorder a ++ inorder b 


-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).



   Por ejemplo, sea n: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt n' =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

data GTree a = EG | NodeG a [GTree a]
  deriving (Show)
data BinTree a = EB | NodeB (BinTree a) a (BinTree a)
  deriving (Show)

t :: GTree Char
t = (NodeG 'A' [ (NodeG 'B' [(NodeG 'F' [(NodeG 'K' [EG]),(NodeG 'L' [EG])]),(NodeG 'G' [EG]),
    (NodeG 'H' [EG])]),(NodeG 'C' [EG]),(NodeG 'D' [EG]),(NodeG 'E' [(NodeG 'I' [(NodeG 'M' [EG])]),(NodeG 'J' [EG])])])

g2bt :: GTree a -> BinTree a
g2bt EG             = EB
g2bt (NodeG a [x])  = (NodeB (g2bt x) a EB)
g2bt (NodeG a ((NodeG b xs):xss)) = (NodeB (NodeB (g2btBrothers xs) b (g2btBrothers xss)) a EB)  

g2btBrothers :: [GTree a] -> BinTree a
g2btBrothers [EG]              = EB
g2btBrothers [(NodeG a [])]    = (NodeB EB a EB) -- Un unico hijo que es una hoja
g2btBrothers ((NodeG a []):ys) = (NodeB EB a (g2btBrothers ys)) -- Varios hijos que son hojas
g2btBrothers [(NodeG a xs)]    = (NodeB EB a (g2btBrothers xs)) -- Un unico hijo que no es una hoja
g2btBrothers ((NodeG a xs):ys) = (NodeB (g2btBrothers xs) a (g2btBrothers ys))  -- Varios hijos que no son hojas


-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
   -}

dcn :: BinTree a -> [a]
dcn = undefined

{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2   -}

maxn :: BinTree a -> Int
maxn = undefined

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar = undefined






