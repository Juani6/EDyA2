{-
1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores primarios. Cualquier
otro color se expresa en t ́erminos de los porcentajes de cada uno estos tres colores que es necesario combinar
en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada color de manera biun ́ıvoca, por lo que
usualmente se utilizan estos valores como representaci ́on de un color.
Definir un tipo Color en este modelo y una funci ́on mezclar que permita obtener el promedio componente a
componente entre dos colores.
-}


type Color = (Int, Int, Int)

mezclar :: Color -> Color -> Color
mezclar (r1,g1,b1) (r2,g2,b2) = (div (r1 + r2) 2, div (g1 + g2) 2, div (b1 + b2) 2)

{-
2. Consideremos un editor de l ́ıneas simple. Supongamos que una L ́ınea es una secuencia de caracteres c1, c2, . . . , cn
junto con una posici ́on p, siendo 0 6p 6n, llamada cursor (consideraremos al cursor a la derecha de un caracter
que ser ́a borrado o insertado, es decir como el cursor de la mayor ́ıa de los editores). Se requieren las siguientes
operaciones sobre l ́ıneas:
vac ́ıa :: L ́ınea
moverIzq :: L ́ınea →L ́ınea
moverDer :: L ́ınea →L ́ınea
moverIni :: L ́ınea →L ́ınea
moverFin :: L ́ınea →L ́ınea
insertar :: Char →L ́ınea →L ́ınea
borrar :: L ́ınea →L ́ınea
La descripci ́on informal es la siguiente: (1) la constante vac ́ıa denota la l ́ınea vac ́ıa, (2) la operaci ́on moverIzq
mueve el cursor una posici ́on a la izquierda (siempre que ello sea posible), (3) an ́alogamente para moverDer , (4)
moverIni mueve el cursor al comienzo de la l ́ınea, (5) moverFin mueve el cursor al final de la l ́ınea, (6) la operaci ́on
borrar elimina el caracterer que se encuentra a la izquierda del cursor, (7) insertar agrega un caracter en el lugar
donde se encontraba el cursor, dejando al caracter insertado a su izquierda.
Definir un tipo de datos L ́ınea e implementar las operaciones dadas
-}

type Linea = ([Char], Int)

moverIzq :: Linea -> Linea
moverIzq ([],p)  = ([],0)
moverIzq (xs,0)  = (xs,0)
moverIzq (xs,p)  = (xs,p-1)


moverDer :: Linea -> Linea
moverDer ([],p)  = ([],0)
moverDer (xs,p)  = if p < length xs then (xs,p+1) else (xs,p)

moverIni :: Linea -> Linea
moverIni (xs,p) = (xs,0)

moverFin :: Linea -> Linea
moverFin (xs,p) = (xs, length xs)

insertar :: Char -> Linea -> Linea 
insertar c ([], 0) = ([c], 1)
insertar c (xs, p) = (take (p-1) xs ++ [c] ++ drop (p-1) xs, p) 

borrar :: Linea -> Linea
borrar ([],0) = ([],0)
borrar (xs,p) = (take (p-1) xs ++ drop p xs,p)

l :: Linea
l = ("Hola", 2)


data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
          deriving(Show)
{-
Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit .
headCL y tailCL no estan definidos para una lista vacıa.
headCL toma una CList y devuelve el primer elemento de la misma (el de mas a la izquierda).
tailCL toma una CList y devuelve la misma sin el primer elemento.
isEmptyCL aplicado a una CList devuelve True si la CList es vac ́ıa (EmptyCL) o False en caso contrario.
isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso
contrario.
-}

headCL :: CList a -> a
headCL (CUnit x)       = x
headCL (Consnoc x y z) = x

lastCL :: CList a -> a
lastCL (CUnit x)       = x
lastCL (Consnoc x y z) = z

tailCL :: CList a -> CList a
tailCL (CUnit x)               = EmptyCL
tailCL (Consnoc x EmptyCL z)   = (CUnit z)
tailCL (Consnoc x (CUnit y) z) = (Consnoc y EmptyCL z)
tailCL (Consnoc x cl' z)       = (Consnoc (headCL cl') (tailCL cl') z)

popLastCL :: CList a -> CList a
popLastCL (CUnit x)               = EmptyCL
popLastCL (Consnoc x EmptyCL z)   = (CUnit x)
popLastCL (Consnoc x (CUnit y) z) = (Consnoc x EmptyCL y)
popLastCL (Consnoc x cl z)        = (Consnoc x (popLastCL cl) (lastCL cl))

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _       = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _         = False

-- b) Definir una funci´on reverseCL que toma una CList y devuelve su inversa.

reverseCL :: CList a -> CList a
reverseCL EmptyCL          = EmptyCL
reverseCL (CUnit x)        = (CUnit x)
reverseCL (Consnoc x cl z) = (Consnoc z (reverseCL cl) x)


-- c) Definir una funcion inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList

inits :: CList a -> CList (CList a)
inits EmptyCL         = (CUnit EmptyCL)
inits (CUnit x)       = CUnit (CUnit x)
inits cl              = (Consnoc (CUnit (headCL cl)) (tailCL (inits cl')) cl)
                        where (Consnoc x y z) = cl
                              cl' = (reverseCL (tailCL (reverseCL cl)))
                              
{-
d) Definir una funci´on lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la
CList.
-}                              

lasts :: CList a -> CList (CList a)
lasts l = inits (reverseCL l) 

{-
e) Definir una funci´on concatCL que toma una CList de CList 
   y devuelve la CList con todas ellas concatenadas
-}

isUnit :: CList a -> Bool
isUnit (CUnit a) = True
isUnit _        = False

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL                     = EmptyCL
concatCL (CUnit x)                   = x
concatCL (Consnoc EmptyCL EmptyCL z) = z
concatCL (Consnoc x EmptyCL EmptyCL) = x
concatCL (Consnoc x EmptyCL z)       = let y = concatCL (Consnoc (tailCL x) EmptyCL (popLastCL z)) in (Consnoc (headCL x) y (lastCL z))
concatCL (Consnoc x (CUnit y) z)     = concatCL (Consnoc (concatCL (Consnoc x EmptyCL y)) EmptyCL z)
concatCL (Consnoc x y z)             = concatCL (Consnoc x' y' z')
                                        where x' = concatCL (Consnoc x EmptyCL (headCL y))
                                              y' = if isUnit y then (popLastCL y) else (tailCL (popLastCL y))
                                              z' = concatCL (Consnoc (lastCL y) EmptyCL z)


cl :: CList Int
cl = (Consnoc 1 (Consnoc 2 (Consnoc 3 EmptyCL 4) 5) 6)

cl2 :: CList (CList Int)
cl2 = Consnoc (Consnoc 1 EmptyCL 2) (CUnit (CUnit 3)) (Consnoc 4 EmptyCL 5)    


{-
4. Defina un evaluador eval :: Exp →Int para el siguiente tipo algebraico:
data Exp = Lit Int |Add Exp Exp |Sub Exp Exp |Prod Exp Exp |Div Exp Exp
-}

data Exp =  Lit Int 
          | Add Exp Exp 
          | Sub Exp Exp 
          | Prod Exp Exp 
          | Div Exp Exp
          deriving(Show)

eval :: Exp -> Int
eval (Lit n)    = n
eval (Add x y)  = eval x + eval y
eval (Sub x y)  = eval x - eval y
eval (Prod x y) = eval x * eval y
eval (Div x y)  = div (eval x) (eval y)

{-
a) Defina una funci ́on parseRPN :: String → Exp que, dado un string que representa una expresi ́on escrita en
RPN, construya un elemento del tipo Exp presentado en el ejercicio 4 correspondiente a la expresi ́on dada. Por
ejemplo:
parseRPN “8 5 3 −3 ∗+” = Add (Lit 8) (Prod (Sub (Lit 5) (Lit 3)) (Lit 3))
Ayuda: para implementar parseRPN puede seguir un algoritmo similar al presentado anteriormente. En lugar
de evaluar las expresiones, debe construir un valor de tipo Exp
-}

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _   = False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isSpace :: Char -> Bool
isSpace c = c == ' '
 
operator :: Char -> Exp -> Exp -> Exp
operator '+' x y = (Add x y)
operator '-' x y = (Sub x y )
operator '*' x y = (Prod x y)
operator '/' x y = (Div x y)
operator err _ _  = error (err : "No es un operador")    

parseRPN :: String -> Exp
parseRPN xs = auxParseRPN xs []

eliminarEspacios :: String -> String
eliminarEspacios [] = []
eliminarEspacios (x:xs) = if isSpace x then eliminarEspacios xs else x : eliminarEspacios xs

auxParseRPN :: String -> [Exp] -> Exp
auxParseRPN [] []    = error("Nada")
auxParseRPN [] [x]   = x
auxParseRPN (x:xs) s | isOperator x = let (y:y':ys) = s
                                          exp = (operator x y' y)
                                          xs' = eliminarEspacios xs
                                      in if null ys then exp else auxParseRPN xs' (exp : ys)
                     | isDigit x    = let (n,xs') = span isDigit (x:xs) 
                                      in auxParseRPN (xs') ((Lit (read n)) : s)
                     | isSpace x    = auxParseRPN xs s
                     | otherwise    = error ("nose")

evalRPN :: String -> Maybe Int
evalRPN [] = error ("Entrada vacia")
evalRPN str = seval (parseRPN str)
--  let (x':xs') = s in (((operator x (read [x']) (read (head [xs'])))), xs)

fromJust :: Maybe a -> a
fromJust (Just a) = a


seval :: Exp -> Maybe Int
seval (Div x (Lit 0))  = Nothing
seval (Lit n)          = Just n
seval (Add x y)        = Just (fromJust (seval x) + fromJust (seval y))
seval (Sub x y)        = Just (fromJust (seval x) - fromJust (seval y))
seval (Prod x y)       = Just (fromJust (seval x) * fromJust (seval y))
seval (Div x y)        = Just (div (fromJust (seval x)) (fromJust (seval y)))