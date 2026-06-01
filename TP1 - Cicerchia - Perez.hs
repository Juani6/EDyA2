-- Trabajo practica 1 - Cicerchia - Perez de Urrecho

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
  
newtype Punto2d = P2d (Double, Double) deriving(Eq,Show)
newtype Punto3d = P3d (Double, Double, Double) deriving(Eq,Show)

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
fromList :: Punto p => [p] -> NdTree p
fromList []     = Empty
fromList (x:xs) = fromList' 0 (dimension x) (x:xs) 

fromList' :: Punto p => Int -> Int -> [p] -> NdTree p
fromList' _  _ []      = Empty
fromList' level dim xs = let xs'  = ordenarPuntos level xs  -- Ordenamos la lista
                             med  = mediana xs'             -- Calculamos la mediana
                             n    = div (length xs) 2       -- Calculamos su posicion
                             xs'' = drop n xs'              -- Tomamos el lado derecho de la lista
                             n'   = medianaCorte n med xs'' -- Obtenemos el indice de la ultima ocurrencia del punto de la mediana
                             r    = drop (n' - n) xs''      -- Dropeamos los elementos faltantes para llegar a la ultima ocurrencia
                             l    = take (n' - 1) xs'       -- Tomamos la lista de elementos a la izquierda del ultimo elemento igual a la mediana
                             nl   = (mod (level+1) dim)     -- Nuevo nivel
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

-- ej 3
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p raiz = insertar' 0 p raiz

insertar' :: Punto p => Int -> p -> NdTree p -> NdTree p
insertar' lvl p Empty                       = (Node Empty p Empty lvl)
insertar' _ p (Node l d r level)            = let newlvl = mod (level + 1) (dimension d) 
                                              in if coord level p <= coord level d 
                                                 then (Node (insertar' newlvl p l) d r level) 
                                                 else (Node l d (insertar' newlvl p r) level)

-- ej 4 
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty                                              = Empty
eliminar p raiz@(Node l d r lvl) | areEqual p d               = eliminar' p raiz 
                                 | coord lvl p <= coord lvl d = (Node (eliminar p l) d r lvl)
                                 | otherwise                  = (Node l d (eliminar p r) lvl) -- coord lvl p > coord lvl d

eliminar' :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar' p (Node Empty d Empty lvl) = Empty                  -- Este caso directamente elimina el nodo
eliminar' p (Node l d Empty lvl)     = let d' = findMin lvl l -- Tomamos el nodo mas chico de l, lo hacemos raiz y l pasa a ser el arbol derecho
                                       in (Node Empty d' (eliminar d' l) lvl)
eliminar' p (Node l d r lvl)         = let d' = findMin lvl r 
                                       in (Node l d' (eliminar d' r) lvl)

findMin :: (Eq p, Punto p) => Int -> NdTree p -> p
findMin k Empty                                     = error("No deberia llegar aca") -- Caso de prueba
findMin k (Node Empty d Empty lvl)                  = d                              -- Si es una hoja devuelvo
findMin k (Node l d r lvl) | lvl == k && l == Empty = d                              -- Si estoy en el eje y el de la izquierda es vacio devuelvo directamente d (el de la derecha es mayor)
                           | lvl == k || r == Empty = min2Puntos k d (findMin k l)   -- si el nivel es k o el de la derecha es vacio comparo el valor con el mas chico de la izquierda
                           | l == Empty             = min2Puntos k d (findMin k r)   -- si l es vacio chequeo con el derecho
                           | otherwise              = let dl = findMin k l
                                                          dr = findMin k r
                                                      in min3Puntos k dl dr d        -- Cualquier otro caso chequeamos cual es el minimo de los 3 posibles
min2Puntos :: Punto a => Int -> a -> a -> a
min2Puntos k x y = let x' = coord k x
                       y' = coord k y 
                   in if x' <= y' then x else y

min3Puntos :: Punto a => Int -> a -> a -> a -> a
min3Puntos k x y z = min2Puntos k x (min2Puntos k y z)

-- ej5 
type Rect = (Punto2d, Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x, y)) ( (P2d (px,py)) , (P2d (qx,qy)) ) = px <= x && qx >= x && py <= y && qy >= y 

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty rect                                                         = []
ortogonalSearch n@(Node l d r lvl) rect@(p,q) | lvl == 0 && coord 0 d  < coord 0 p = ortogonalSearch r rect -- si la x del nodo es mas chica que la recta izquierda
                                              | lvl == 1 && coord 1 d  < coord 1 p = ortogonalSearch r rect -- si la y del nodo es mas chica que la recta inferior
                                              | lvl == 0 && coord 0 d  > coord 0 q = ortogonalSearch l rect -- si la x del nodo excede el maximo
                                              | lvl == 1 && coord 1 d  > coord 1 q = ortogonalSearch l rect -- si la y del punto excede el maximo
                                              | otherwise                          = [d | inRegion d rect] 
                                                                                     ++ ortogonalSearch l rect 
                                                                                     ++ ortogonalSearch r rect