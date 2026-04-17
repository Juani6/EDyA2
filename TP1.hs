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

instance Punto Punto3d where
  dimension (P3d p) = 3
  coord 0 (P3d (x,_,_)) = x
  coord 1 (P3d (_,y,_)) = y
  coord 2 (P3d (_,_,z)) = z 


-- 2
mediana :: Punto p => Int -> [p] -> p
mediana i xs = let listaCoord = (map (coord i) xs) 
                   m = valorMediano listaCoord (dimension (head xs))
               in xs !! minList (map (abs . (\x -> x-m)) listaCoord)


valorMediano :: [Double] -> Int -> Double
valorMediano xs n = (/) (foldr (+) 0.0 xs) (fromIntegral n)

minList :: Ord x => [x] -> Int
minList (x:xs) = auxMinList (zip xs [0..]) x 0

auxMinList :: Ord x => [(x,Int)] -> x -> Int -> Int
auxMinList [] n j      = j
auxMinList (x:xs)  n j = let x' = (fst x) 
                         in if x' <= n then auxMinList xs x' (snd x) 
                                       else auxMinList xs n j

puntos :: [Punto2d] 
puntos = [(P2d (1.0, 2.0)), (P2d(3.5, 4.1)), (P2d(0.5, 9.2)), (P2d(7.3, 1.1)), (P2d(5.0, 5.0)),
          (P2d(2.2, 8.8)), (P2d(6.6, 3.3)), (P2d(4.4, 7.7)), (P2d(9.1, 0.2)), (P2d(8.0, 6.5))]