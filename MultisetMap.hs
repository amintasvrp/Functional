module MultisetMap ( MultisetMap.insert,
                     remove,
                     search,
                     MultisetMap.union,
                     intersection,
                     minus,
                     inclusion,
                     MultisetMap.sum,
                     size,
                     module M
)
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
 - cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
 - Eh recomendavel que voce consulte a documentacao de Data.Map
 -}
import Data.Map as Map hiding (intersection, union, size)
import Data.Map as M hiding (intersection, union, size, insert)

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem bag = Map.insertWith (+) elem 1 bag

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem bag = if member elem bag then remove' elem bag else bag

remove' elem bag = if next == 0 then Map.delete elem bag else Map.insert elem next bag
             where
               next = (bag ! elem) - 1

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem bag = if member elem bag then bag ! elem else 0

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {[("a",1),("c",3)]}, B = {[("b",2),("c",1)]}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}

union bag1 bag2 | Map.null bag1 = bag2
                | Map.null bag2 = bag1
                | otherwise = if quant1 < quant2 then MultisetMap.union newBag1 newBag2 else MultisetMap.union bag1 newBag2  
               where
                list2 = toList (bag2)
                elem2 = fst (head (list2))
                quant1 = search elem2 bag1
                quant2 = snd (head (list2))
                newBag1 = (iterate (MultisetMap.insert elem2) bag1) !! (quant2 - quant1)
                newBag2 = (fromList (tail list2))


{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}

intersection bag1 bag2 = if (size bag1) > (size bag2) then intersection' bag1 bag2 else intersection' bag2 bag1

intersection' big sml | (size sml) == 0 || (size big) == 0 = (fromList [])
                      | otherwise = if quant1 > quant2 then intersection' newBig sml else (iterate (MultisetMap.insert elem) intersectionRec) !! quant1
                       where 
                        list = toList big
                        elem = fst (head list)
                        quant1 = snd (head list)
                        quant2 = MultisetMap.search elem sml
                        newBig = MultisetMap.remove elem big
                        intersectionRec = intersection' (fromList (tail list)) sml

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 bag2 | (Map.null bag1 || Map.null bag2) = bag1
                | otherwise = if quant > 0 then Map.insert elem1 quant (minus newBag1 bag2) else minus newBag1 bag2  
               where
                list1 = toList bag1
                elem1 = fst (head list1)
                quant1 = snd (head list1)
                quant2 = search elem1 bag2
                quant = quant1 - quant2
                newBag1 = (fromList (tail list1))

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion bag1 bag2 | Map.null bag1 = True
                    | Map.null bag2 = False
                    | otherwise = if quant1 <= quant2 then inclusion newBag1 bag2 else False
                   where 
                    list1 = toList bag1
                    elem1 = fst (head list1)
                    quant1 = snd (head list1)
                    quant2 = search elem1 bag2
                    newBag1 = (fromList (tail list1))

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum bag1 bag2 | Map.null bag1 = bag2
              | Map.null bag2 = bag1
              | otherwise = MultisetMap.sum newBag1 newBag2
             where 
              list2 = toList bag2
              elem2 = fst (head list2)
              quant2 = snd (head list2)
              quant1 = search elem2 bag1
              newBag1 = Map.insert elem2 (quant1 + quant2) bag1
              newBag2 = (fromList (tail list2))

{-
 - Retorna a quantidade total de elementos no Bag
-}
size bag = Prelude.sum (elems bag)