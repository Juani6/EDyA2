module Practica2 where

import Data.List

-- a) test donde test f x = f x ≡x + 2
test :: (Num a, Eq a) => (a -> a) -> a -> Bool 
test f x = (f x) == (x + 2)

-- b) esMenor donde esMenor y z = y < z

esMenor :: Int -> Int -> Bool
esMenor y z = y < z

-- c) eq donde eq a b = a ≡ b

eq :: Int -> Int -> Bool
eq a b = a == b

-- d) showVal donde showVal x = "Valor:" ++ show x

showVal :: [Char] -> Char -> [Char]
showVal str c = str ++ show c

{-
a) (+5)
(+5) :: Int -> Int
b) (0<)
(0<) :: (Num a, Ord a) => a -> Bool
c) (’a’:)
f1 :: [Char] -> [Char]
d) (++"\n")
f2 :: [Char] -> [Char]
e) filter (≡ 7)
filter7 :: [Int] -> [Int]
f) map (++[1])
map1 :: [Int] -> [Int] 
-}

--a) (Int → Int) → Int
apply0 :: (Int -> Int) -> Int
apply0 f = f 0 

--b) Int → (Int → Int)
masx :: Int -> (Int -> Int)
masx x = (+x)

--c) (Int → Int) → (Int → Int)
fCuad :: (Int -> Int) -> (Int -> Int)
fCuad f = f . f

--d) Int → Bool
isZero :: Int -> Bool
isZero n = n == 0

--e) Bool → (Bool → Bool)
and2 :: Bool -> (Bool -> Bool)
and2 b = (&&b)

--f) (Int, Char) → Bool
isOrd :: (Int, Char) -> Bool
isOrd (n,c) = if n == fromEnum c then True else False

--g) (Int, Int) → Int
sumarda :: (Int, Int) -> Int
sumarda (a,b) = a + b

--h) Int → (Int, Int)
sig :: Int -> (Int, Int)
sig n = (n,n+1)

--i) a → Bool
absoluteTrueFunction :: a -> Bool
absoluteTrueFunction x = True

--j) a → a

id :: a -> a
id x = x

{-
a) if true then false else true where false = True; true = False
f :: a -> Bool
f x = if true then false else true
    where false = True
          true  = False
-}
-- b)if if then then else else [Mal] (Sintaxis)
-- c) False == (5 >= 4) [Bien]
-- d) 1 < 2 < 3 [Mal] (Compara un Bool con un Int)
-- e) 1 + if (’a’ < ’z’) then −1 else 0 [Bien]
-- f) if fst p then fst p else snd p where p = (True,2) [Mal] (then y else tienen que tener el mismo tipo)

f2 x = if fst p then fst p else snd p 
  where p = (True,False)

-- asi anda

-- 5. Reescribir cada una de las siguientes definiciones sin usar let, where o if:

-- a) f x = let (y ,z ) = (x ,x ) in y
-- f x = ((x,x),z)

-- b) greater (x ,y ) = if x > y then True else False

greater :: Ord a => (a,a) -> Bool
greater (x,y) = x > y

-- c) f (x ,y) = let z = x + y in g (z,y) where g (a,b) = a − b
f :: Num a => (a,a) -> a
f (x,y) = x -- + y - y

{-
Pasar de notaci ́on Haskell a notaci ́on de funciones an ́onimas (llamada notaci ́on lambda),
a) smallest , definida por
smallest (x,y,z)    |x <= y ∧ x <= z = x
                    |y <= x ∧ y <= z = y
                    |z <= x ∧ z <= y = z

s = \(x,y,z) -> if x <= y && x <= z then x else if y <= z && y <= x then y else z

b) second x = \x -> x
sec = \x -> \y -> y

c) andThen , definida por
andThen True y = y
andThen False y = False

aT = \x -> \y -> if x then y else False

d) twice f x = f (f x )
t = \f -> \x -> (f . f) x

e) flip f x y = f y x
flip = \f -> \x -> \y -> f y x

f) inc = (+1)

inc = \x -> x + 1

-}

-- 7. Pasar de notacion lambda a notacion Haskell

-- a) iff = λx →λy →if x then not y else y

iff :: Bool -> Bool -> Bool
iff True  y = not y
iff False y = y

-- b) alpha = λx →x
alpha :: a -> a
alpha x = x


{-

8. Suponiendo que f y g tienen los siguientes tipos
f :: c → d
g :: a → b → c
y sea h definida como
h x y = f (g x y)
Determinar el tipo de h e indicar cuales de las siguientes definiciones de h son equivalentes a la dada:
h = f ◦ g           -- h :: (a -> b -> d)
h x = f ◦ (g x)     -- h :: (b -> d)
h x y = (f ◦ g) x y -- h :: (a -> b -> d)
Dar el tipo de la funcion (◦).

h :: a -> b -> d
(◦) :: (a -> b) -> (b -> c) -> (a -> c)

-}

{-
9. La funcion zip3 zipea 3 listas. Dar una definicion recursiva de la funcion y otra definici ́on con el mismo tipo
que utilice la funcion zip. ¿Que ventajas y desventajas tiene cada definicion?
-}

zipR :: [a] -> [b] -> [c] -> [(a,b,c)]
zipR [] _ _ = []
zipR _ [] _ = []
zipR _ _ [] = []
zipR (x:xs) (y:ys) (z:zs) = (x,y,z) : zipR xs ys zs
-- corta una vez leida la lista mas corta
-- O(min lena lenb lenz)


zipCuad :: [a] -> [b] -> [c] -> [(a,b,c)]
zipCuad x y z = [(a,b,c) | ((a,b),c) <- zip (zip x y) z]
-- Es mas costoso O(len z + min {lenx,leny} + len x + len y)

{-

10. Indicar bajo qu ́e suposiciones tienen sentido las siguientes ecuaciones. Para aquellas que tengan sentido, indicar
si son verdaderas y en caso de no serlo modificar su lado derecho para que resulten verdaderas:
a) [[]] ++ xs = xs                  -- = ([]:xs)
b) [[ ]] ++ xs = [xs ]              -- = ([]:xs)
c) [[ ]] ++ xs = [ ] : xs           -- = ([]:xs)
d) [[ ]] ++ xs = [[ ],xs ]          -- = ([]:xs) 
e) [[ ]] ++ [xs] = [[ ],xs]         -- = [[],[xs]]
f) [[ ]] ++ [ xs ] = [xs ]          -- = [[],[xs]]
g) [ ] ++ xs = [ ] : xs             -- = xs
h) [ ] ++ xs = xs                   --- ok
i) [xs ] ++ [ ] = [xs ]             --- ok
j) [xs ] ++ [ xs ] = [ xs ,xs ]     --- ok

-}

{-
11. Inferir, de ser posible, los tipos de las siguientes funciones:
(puede suponer que sqrt :: Float →Float )

a) modulus = sqrt ◦sum ◦map (↑2)
map :: (a->b) -> [a] -> [b]
sum :: (Foldable t, Num b) => [t] -> b
sqrt :: [Float] -> Float

modulus [Float] -> Float

b)
vmod [ ] = [ ]
vmod (v : vs ) = modulus v : vmod vs

vmod :: [[Float]] -> [Float]

-}


{-
12. Dado el siguiente tipo para representar n ́umeros binarios:
type NumBin = [Bool ]
donde el valor False representa el n ́umero 0 y True el 1. Definir las siguientes operaciones tomando como convenci ́on
una representaci ́on Little-Endian (i.e. el primer elemento de las lista de d ́ıgitos es el d ́ıgito menos significativo del
n ́umero representado).
a) suma binaria
b) producto binario
c) cociente y resto de la divisi ́on por dos
-}

type NumBin = [Bool]
-- False = 0
-- True  = 1

xor :: Bool -> Bool -> Bool
xor True True   = False
xor False False = False
xor _ _         = True

-- binarySum :: l1 -> l2 -> carryFlag
-- La carry flag determina si el ultimo carry alarga la lista o no
-- Si esta en False se ignora, si esta en True el carry se mantiene
binarySum :: NumBin -> NumBin -> Bool -> NumBin
binarySum xs ys carryFlag = binarySumAux xs ys False carryFlag 


binarySumAux :: NumBin -> NumBin -> Bool -> Bool -> NumBin
binarySumAux [] [] False _                 = []
binarySumAux [] [] True  False             = []
binarySumAux [] [] True  True              = [True]
binarySumAux x  [] True  cf                = binarySumAux x [True]  False cf
binarySumAux [] y  True  cf                = binarySumAux y [True]  False cf
binarySumAux x  [] False cf                = binarySumAux x [False] False cf
binarySumAux []  y False cf                = binarySumAux [False] y False cf  
binarySumAux (x:xs) (y:ys) carry cf = digito : binarySumAux xs ys carryFinal cf
                                where digito     = (x && y && carry) || (xor x (xor y carry))
                                      carryFinal = ((x && y) || (x && carry) || (y && carry))                                       
-- el carry tiene que estar prendido si hay 2 o 3 Trues
-- el digito tiene que quedar en 1 si hay 1 o 3 Trues



-- b) producto binario

esImpar :: NumBin -> Bool
esImpar (x:xs)  = x

prod2 :: NumBin -> NumBin
prod2 x = False : x

div2 :: NumBin -> NumBin 
div2 [] = []
div2 x  = tail x

bitWiseOr :: NumBin -> Bool
bitWiseOr [] = False
bitWiseOr (nb:ns) = nb || bitWiseOr ns  

prodBin :: NumBin -> NumBin -> NumBin
prodBin x y = prodBinAux x y []

prodBinAux :: NumBin -> NumBin -> NumBin -> NumBin
prodBinAux x y res | not (bitWiseOr x) = res 
                   | esImpar x   = prodBinAux (div2 x) (prod2 y) (binarySum res y True)
                   | otherwise   = prodBinAux (div2 x) (prod2 y) res



-- c) cociente y resto de la division por dos

normalize :: NumBin -> NumBin -> (NumBin,NumBin)
normalize x y   | cond == 0 = (x,y)  
                | cond > 0  = (x, addZeros y cond)
                | cond < 0  = (addZeros x (-cond), y)
                where cond  = (length x - length y) 

addZeros :: NumBin -> Int -> NumBin
addZeros b 0 = b 
addZeros b n = addZeros (b ++ [False]) (n - 1) 

comp2 :: NumBin -> NumBin
comp2 x = comp2Aux x False

comp2Aux :: NumBin -> Bool -> NumBin
comp2Aux []     _ = []
comp2Aux (x:xs) False = if x then x : comp2Aux xs True
                             else x : comp2Aux xs False
comp2Aux (x:xs) True  =       not x : comp2Aux xs True


sign :: Int -> Int
sign x | x < 0  = -1
       | x == 0 = 0
       | x > 0  = 1

-- 1 si b1 > b2, -1 caso contrario y 0 si son iguales
numBinCompare :: NumBin -> NumBin -> Int
numBinCompare x y | (lx > ly && last x) = 1
                  | (lx < ly && last y) = -1
                  | otherwise = sign (numBinCompareSameLength (reverse x) (reverse y))
                  where lx = length x
                  where ly = length y

numBinCompareSameLength :: NumBin -> NumBin -> Int
numBinCompareSameLength [] [] = 0
numBinCompareSameLength x y   = if head x == head y then numBinCompareSameLength (tail x) (tail y)
                                          else if head x && not (head y) then 1
                                                                         else -1  

divNumBin :: NumBin -> NumBin -> (NumBin, NumBin) -- Cociente y Resto
divNumBin x y = let n = normalize x y in divNumBinAux (fst n) (snd n) [False]
divNumBinAux :: NumBin -> NumBin -> NumBin -> (NumBin, NumBin)
divNumBinAux x [] coc              = (coc, x)
divNumBinAux x y coc  | cond == 0  = (binarySum coc [True] True, [False])
                      | cond == 1  = divNumBinAux divid y (binarySum coc [True] True)
                      | cond == -1 = divNumBinAux x [] coc
                      where cond   = numBinCompare x y
                            divid  = (binarySum x (comp2 y) False)