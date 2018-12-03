module CartesianTree (
  buildMaxCT,
  buildMinCT,
  sizeCT,
  insertMaxCT,
  insertMinCT,
  removeMaxCT,
  removeMinCT,
  mergeMaxCT,
  mergeMinCT,
  mergeMaxCT',
  mergeMinCT',
  splitCT,
  heightCT,
  preOrder,
  order,
  postOrder,
  getValue,
  getLeft,
  getRight,
  CT(..)
)
 where

import Data.List as DL
import Data.Maybe as DM

-- Escreva as funcoes sobre a estrutura de dados cartesian tree

data CT x y = NIL | Node x y (CT x y) (CT x y) deriving (Eq,Show)

-- Retorna index

getIndex (Node x _ _ _) = x

-- Retorna valor

getValue (Node _ y _ _) = y

-- Retorna filho a esquerda

getLeft NIL = NIL
getLeft (Node _ _ left _) = left

-- Retorna filho a direita

getRight NIL = NIL
getRight (Node _ _ _ right) = right

-- Constrói a árvore

buildMaxCT :: Ord a => [a] -> CT Int a
buildMaxCT [] = NIL
buildMaxCT array = buildMaxCT' array array

buildMaxCT' :: Ord a => [a] -> [a] -> CT Int a
buildMaxCT' [] _ = NIL
buildMaxCT' array originalArray = (Node originalIndex maximus (buildMaxCT' before originalArray) (buildMaxCT' after originalArray))
                                  where
                                    maximus = maximum array
                                    originalIndex = DM.fromJust (DL.elemIndex maximus originalArray)
                                    index = DM.fromJust (DL.elemIndex maximus array)
                                    after = DL.drop (index+1) array
                                    before = DL.take (index) array

buildMinCT :: Ord a => [a] -> CT Int a
buildMinCT [] = NIL
buildMinCT array = buildMinCT' array array

buildMinCT' :: Ord a => [a] -> [a] -> CT Int a
buildMinCT' [] _ = NIL
buildMinCT' array originalArray = (Node originalIndex minimus (buildMinCT' before originalArray) (buildMinCT' after originalArray))
                                  where
                                    minimus = minimum array
                                    originalIndex = DM.fromJust (DL.elemIndex minimus originalArray)
                                    index = DM.fromJust (DL.elemIndex minimus array)
                                    after = DL.drop (index+1) array
                                    before = DL.take (index) array

-- Tamanho da árvore

sizeCT NIL = 0
sizeCT (Node x y left right) = 1 + sizeCT left + sizeCT right

-- Altura da árvore

heightCT NIL = 0
heightCT (Node x y left right) = max (1 + heightCT left) (1 + heightCT right)


-- Insere uma nova chave na CT retornando a CT modificada

insertMaxCT value node = mergeMaxCT node (Node 0 value NIL NIL)
insertMinCT value node = mergeMinCT node (Node 0 value NIL NIL)

-- Remove o elemento da CT

removeMaxCT _ NIL = NIL
removeMaxCT index node | index == 0 = decreaseCT (dropCT index node) (-1)
                       | otherwise = mergeMaxCT l r2
                       where
                        l = takeCT (index-1) node
                        r = dropCT (index-1) node
                        r2 = decreaseCT (dropCT index r) (sizeCT (takeCT index r))

removeMinCT _ NIL = NIL
removeMinCT index node | index == 0 = decreaseCT (dropCT index node) (-1)
                       | otherwise = mergeMinCT l r2
                       where
                        l = takeCT (index-1) node
                        r = dropCT (index-1) node
                        r2 = decreaseCT (dropCT index r) (sizeCT (takeCT index r))

decreaseCT NIL _ = NIL
decreaseCT (Node key value left right) size = (Node (key - size - 2) value (decreaseCT left size) (decreaseCT right size))

-- Une duas árvores

mergeMaxCT nodeA nodeB = mergeMaxCT' nodeA (increaseCT nodeB (sizeCT nodeA))
mergeMinCT nodeA nodeB = mergeMinCT' nodeA (increaseCT nodeB (sizeCT nodeA))

mergeMaxCT' NIL (Node xB yB leftB rightB) = (Node xB yB leftB rightB)
mergeMaxCT' (Node xA yA leftA rightA) NIL = (Node xA yA leftA rightA)
mergeMaxCT' (Node xA yA leftA rightA) (Node xB yB leftB rightB)  | yA > yB = (Node xA yA leftA (mergeMaxCT' rightA (Node xB yB leftB rightB)))
                                                                 | otherwise = (Node xB yB (mergeMaxCT' (Node xA yA leftA rightA) leftB) rightB)

mergeMinCT' NIL (Node xB yB leftB rightB) = (Node xB yB leftB rightB)
mergeMinCT' (Node xA yA leftA rightA) NIL = (Node xA yA leftA rightA)
mergeMinCT' (Node xA yA leftA rightA) (Node xB yB leftB rightB)  | yA < yB = (Node xA yA leftA (mergeMinCT' rightA (Node xB yB leftB rightB)))
                                                                 | otherwise = (Node xB yB (mergeMinCT' (Node xA yA leftA rightA) leftB) rightB)

increaseCT NIL _ = NIL
increaseCT (Node key value left right) size = (Node (key + size) value (increaseCT left size) (increaseCT right size))

-- Divide a árvore dado um índice

splitCT key (Node x y left right) = (takeCT key (Node x y left right), dropCT key (Node x y left right))

dropCT _ NIL = NIL
dropCT key (Node x y left right)  | key < x = (Node x y (dropCT key left) right)
                                  | key > x = dropCT key right
                                  | key == x = right

takeCT _ NIL = NIL
takeCT key (Node x y left right)  | key < x = takeCT key left
                                  | key > x = (Node x y left (takeCT key right))
                                  | key == x = (Node x y left NIL)

-- Retorna uma array com os dados da BST nos diversos tipos de caminhamento

preOrder NIL = []
preOrder (Node x y left right) = [y] ++ (preOrder left) ++ (preOrder right)

order NIL = []
order (Node x y left right) = (order left) ++ [y] ++ (order right)

postOrder NIL = []
postOrder (Node x y left right) = (postOrder left) ++ (postOrder right) ++ [y]