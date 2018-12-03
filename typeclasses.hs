{-
- Tarefa 1
- Verifique qual é o tipo inferido pelo ghci (o tipo principal), para cada uma das seguintes funções
- (pré-definidas): elem, sum e minimum. Justifique o tipo da função, com base no que deverá ser a
- sua definição.
-}

{-
- elem :: (Eq a, Foldable t) => a -> t a -> Bool
- A função recebe um parâmetro do tipo "a" cuja equivalência de valores pode ser testada (o que é essencial para verificar se um determinado valor se encontra na estrutura de dados)
- e um parâmetro que é uma estrutura de dados "dobrável" (de forma que é definida recursivamente) que armazena dados do tipo "a".
-}

{-
- sum :: (Num a, Foldable t) => t a -> a
- A função recebe uma estrutura de dados "dobrável", a fim de utilizarmos funções como foldr ou foldl para substitir o (cons) por (+) e
- o [] por 0.
-}

{-
- minimum :: (Ord a, Foldable t) => t a -> a
- A função recebe um parâmetro do tipo "a" ordenável, a fim de comparar valores para verificar quem é maior, e quem é menor, e aplicando essa mesma lógica
- de forma recursiva em uma estrura de dados "dobrável", que é definida nessa mesma forma. 
-}

{-
- Tarefa 2
- Considere as seguintes declarações de tipo usadas para representar as horas de um dia nos formatos
- usuais.
-}

data Part = AM | PM
    deriving (Eq, Show)
data TIME = Local Int Int Part
          | Total Int Int

-- 1. Defina algumas constantes do tipo TIME.

meioDiaLocal = Local 12 00 PM
meiaNoiteLocal = Local 12 00 AM
meiaDiaTotal = Total 12 00
meiaNoiteTotal = Total 00 00

-- 2. Defina a função totalMinutos :: TIME -> Int que conta o total de minutos de uma dada hora.

totalMinutos :: TIME -> Int
totalMinutos (Local h m p) | p == AM = 60 * h + m
                           | otherwise = 60 * (12 + h) + m

totalMinutos (Total h m) = 60 * h + m 

-- 3. Defina TIME como instância da classe Eq de forma a que a igualdade entre
-- horas seja independente do formato em que hora está guardada.

instance Eq TIME where
    (==) (Local h1 m1 p1) (Local h2 m2 p2) = (h1 == h2) && (m1 == m2) && (p1 == p2)
    (==) (Total h1 m1) (Total h2 m2) = (h1 == h2) && (m1 == m2)
    (==) (Total h2 m2) (Local h1 m1 p) = (==) (Local h1 m1 p) (Total h2 m2)
    (==) (Local h1 m1 p) (Total h2 m2) | p == AM = (h1 == h2) && (m1 == m2)
                                       | otherwise = (h1 == (h2 - 12)) && (m1 == m2)

-- 4. Defina TIME como instância da classe Ord.

instance Ord TIME where
    compare (Local h1 m1 p1) (Local h2 m2 p2) | (Local h1 m1 p1) == (Local h2 m2 p2) = EQ
                                              | (p1 == AM && p2 == PM) || (h1 < h2) || (m1 < m2) = LT
                                              | otherwise = GT
    compare (Total h1 m1) (Total h2 m2)       | (Total h1 m1) == (Total h2 m2) = EQ
                                              | (h1 < h2) || (m1 < m2) = LT
                                              | otherwise = GT
    compare (Local h1 m1 p) (Total h2 m2)     | (Local h1 m1 p) == (Total h2 m2) = EQ
                                              | (p == AM && h2 >= 12) = LT
                                              | (p == AM && h2 < 12) && ((h1 < h2) || (m1 < m2)) = LT
                                              | (p == PM && h2 >= 12) && ((h1 + 12 < h2) || (m1 < m2)) = LT
                                              | otherwise = GT
    compare (Total h1 m1) (Local h2 m2 p)     | (Total h1 m1) == (Local h2 m2 p) = EQ
                                              | (h1 < 12 && p == PM) = LT
                                              | (h1 < 12 && p == AM) && ((h1 < h2) || (m1 < m2)) = LT
                                              | (h1 >= 12 && p == PM) && ((h1 < h2 + 12) || (m1 < m2)) = LT
                                              | otherwise = GT


{-
- 5. Defina TIME como instância da classe Show, de modo a que a apresentação dos termos (Local
- 10 35 AM), (Local 4 20 PM) e (Total 17 30) seja respectivamente: 10:35 am, 4:20 pm
- e 17h30m.
-}

instance Show TIME where
    show (Local h m p) = (show h) ++ ":" ++ (show m) ++ " " ++ ((\x -> if x == AM then "am" else "pm") p)
    show (Total h m) = (show h) ++ "h" ++ ((\x -> if x < 10 then "0" ++ (show x) else (show x)) m) ++ "m"


{-
- 6. Defina a função seleciona :: TIME -> [(TIME,String)] -> [(TIME,String)] que re-
- cebe uma hora e uma lista de horários de cinema, e seleciona os filmes que começam depois
- de uma dada hora.
-}

seleciona :: TIME -> [(TIME,String)] -> [(TIME,String)]
seleciona _ [] = [] 
seleciona hora filmes | hora < horarioFilme = [filme] ++ seleciona hora (tail filmes)
                      | otherwise = seleciona hora (tail filmes) 
                     where
                        filme = head filmes
                        horarioFilme = fst filme

{-
- 7. Declare TIME como instância da classe Enum, de forma a que succ avance o relógio 1 minuto
- e pred recue o relógio 1 minuto. Assuma que o sucessor de 11:59 pm é 00:00 am. Depois,
- faça o interpretador calcular o valor das seguintes expressões: [(Total 10 30)..(Total 10 35)] e [(Total 10 30),(Local 10 35 AM)..(Total 15 20)].
-}

instance Enum TIME where
    fromEnum (Local h m p) | (p == PM) = (12 * 60) + (h * 60) + m
                           | otherwise = (h * 60) + m 
    fromEnum (Total h m) = (h * 60) + m
    toEnum minutos = (Total h m) 
            where
                h = (minutos `div` 60) `mod` 24
                m = minutos - (h * 60)

{-
- Tarefa 3
- Considere as declarações da classe FigFechada e da função fun a seguir apresentadas
-}

class FigFechada a where
    area :: a -> Float
    perimetro :: a -> Float

fun figs = filter (\fig -> (area fig) > 100) figs

{-
- 1. Indique, justificado, qual é o tipo inferido pelo interpretador Haskell para a função fun.
-}

{-
- fun :: FigFechada a => [a] -> [a]
- A função fun recebe um array de elementos do tipo "a", que devem adotar o comportamento da classe "FigFechada" para utilizar a função "area" definida na classe citada.
- A função fun retorna um array de elementos de tipo "a", afinal basicamente realiza uma aplicação da função filter. 
-}

{-
- 2. No plano cartesiano um rectângulo com os lados paralelos aos eixos pode ser univocamente
- determinado pelas coordenadas do vértice inferior esquerdo e pelos comprimentos dos lados,
- ou por uma diagonal dada por dois pontos. Assim, para representar esta figura geométrica,
- definiu-se em Haskell o seguinte tipo de dados:
-}

type Ponto = (Float,Float)
type Lado = Float
data Rectangulo = PP Ponto Ponto
                | PLL Ponto Lado Lado

instance FigFechada Rectangulo where
    area (PP a c) = (sen45 * diag) * (cos45 * diag)
                 where
                    sen45 = (sqrt 2) / 2
                    cos45 = sen45
                    xa = fst a
                    ya = snd a
                    xc = fst c
                    yc = snd c                    
                    diag = sqrt ((xc - xa) ** 2 + (yc - ya) ** 2)
    area (PLL p l1 l2) = l1 * l2
    
    perimetro (PP a c) = 2 * (sen45 * diag) +  2 * (cos45 * diag)
                 where
                    sen45 = (sqrt 2) / 2
                    cos45 = sen45
                    xa = fst a
                    ya = snd a
                    xc = fst c
                    yc = snd c                    
                    diag = sqrt ((xc - xa) ** 2 + (yc - ya) ** 2)
    
    perimetro (PLL p l1 l2) = 2 * l1 + 2 * l2