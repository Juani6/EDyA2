module TP1 where

import Data.List

data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
  deriving(Eq, Ord, Show)

-- 1

class Punto p where
  dimension :: p -> Int
  coord :: Int -> p -> Double
  dist :: p -> p -> Double
-- 1a  
  dist p q = dist' (dimension p - 1) p q 
  
  dist' :: Int -> p -> p -> Double
  dist' 0 p q = ((coord 0 p) - (coord 0 q)) ^ 2 
  dist' d p q = ((coord d p) - (coord d q)) ^ 2 + dist' (d-1) p q
-- 1b
  
newtype Punto2d = P2d (Double, Double) deriving(Show)
newtype Punto3d = P3d (Double, Double, Double) deriving(Show)

instance Punto Punto2d where
  dimension (P2d p) = 2
  coord 0 (P2d (x,_)) = x
  coord 1 (P2d (_,y)) = y 
  coord _ _ = error("Fuera de rango")

instance Punto Punto3d where
  dimension (P3d p) = 3
  coord 0 (P3d (x,_,_)) = x
  coord 1 (P3d (_,y,_)) = y
  coord 2 (P3d (_,_,z)) = z 
  coord _ _ = error("Fuera de rango")

-- 2
inorder :: Punto p => NdTree p -> [p]
inorder Empty = []
inorder (Node l dato r d) = inorder l ++ [dato] ++ inorder r  

fromList :: Punto p => [p] -> NdTree p
fromList (x:xs) = fromList' 0 (dimension x) (x:xs) 

fromList' :: Punto p => Int -> Int -> [p] -> NdTree p
fromList' _  _ []      = Empty
fromList' level dim xs = let xs'  = ordenarPuntos level xs -- Ordenamos la lista
                             med  = mediana xs'            -- Calculamos la mediana
                             n    = div (length xs) 2      -- Calculamos su posicion
                             xs'' = drop n xs'             -- Tomamos el lado derecho de la lista
                             n'   = medianaCorte n med xs'' - 1 -- Obtenemos el indice de la ultima ocurrencia del punto de la mediana
                             r    = drop (n' - n) xs''     -- Dropeamos los elementos faltantes para llegar a la ultima ocurrencia
                             l    = take (n' - 1) xs'      -- Tomamos la lista de elementos a la izquierda del ultimo elemento igual a la mediana
                             nl   = (mod (level+1) dim)    -- Nuevo nivel
                             in Node (fromList' nl dim l) med (fromList' nl dim r) level

ordenarPuntos :: Punto p => Int -> [p] -> [p]
ordenarPuntos i xs = sortBy (orden i) xs 

-- data Ordering = LT | EQ | GT
orden :: Punto p => Int -> p -> p -> Ordering
orden dim p q | dim == dimension p        = EQ
              | coord dim p > coord dim q = GT
              | coord dim p < coord dim q = LT
              | otherwise                 = orden (dim+1) p q


areEqual :: Punto p => p -> p -> Bool
areEqual p q = areEqual' (dimension p - 1) p q 
areEqual' :: Punto p => Int -> p -> p -> Bool
areEqual' 0 p q = coord 0 p == coord 0 q
areEqual' i p q = (coord i p == coord i q) && areEqual' (i-1) p q

-- Espera una lista ordenada
mediana :: Punto p => [p] -> p
mediana xs = xs !! div (length xs) 2 

medianaCorte :: Punto p => Int -> p -> [p] -> Int
medianaCorte j _ []     = j
medianaCorte j p (x:xs) = if areEqual p x then medianaCorte (j + 1) p xs
                                          else j



p1 :: Punto3d
p2 :: Punto3d
p3 :: Punto3d
p4 :: Punto3d

p1 = P3d (1.0,2.0,3.0)
p2 = P3d (1.0,2.0,3.0)
p3 = P3d (1.0,2.0,3.2)
p4 = P3d (2.0, 3.0, 4.0)

puntos :: [Punto2d] 
puntos = [(P2d (1.0, 2.0)), (P2d(3.5, 4.1)), (P2d(0.5, 9.2)), (P2d(7.3, 1.1)), (P2d(5.0, 5.0)),
          (P2d(2.2, 8.8)), (P2d(6.6, 3.3)), (P2d(4.4, 7.7)), (P2d(9.1, 0.2)), (P2d(8.0, 6.5))]

puntosLimite2d :: [Punto2d]
puntosLimite2d =
  [ P2d (5.0, 5.0)   -- duplicado exacto de la raíz
  , P2d (5.0, 1.0)   -- sobre el hiperplano x=5.0 (nivel 0 corta en x)
  , P2d (5.0, 9.0)   -- ídem, distinta y
  , P2d (3.0, 4.1)   -- sobre hiperplano y=4.1 (nivel 1 corta en y)
  , P2d (7.0, 4.1)   -- ídem, distinta x
  , P2d (4.4, 4.1)   -- sobre hiperplano y=4.1 del nodo (4.4,7.7)
  , P2d (5.0, 5.0)   -- segundo duplicado exacto
  , P2d (5.0, 5.0)
  ]

puntos3d :: [Punto3d]
puntos3d =
  [ P3d (5.0, 3.0, 2.0)   -- raíz (nivel 0, corta en x=5.0)
  , P3d (5.0, 1.0, 8.0)   -- sobre hiperplano x=5.0  ← límite nivel 0
  , P3d (2.0, 3.0, 6.0)   -- sobre hiperplano y=3.0  ← límite nivel 1
  , P3d (7.0, 3.0, 1.0)   -- ídem, lado derecho
  , P3d (3.0, 1.0, 2.0)   -- sobre hiperplano z=2.0  ← límite nivel 2
  , P3d (5.0, 3.0, 2.0)   -- duplicado exacto de la raíz
  , P3d (5.0, 3.0, 2.0)   -- tercer duplicado
  ]
