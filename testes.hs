{-
- Elabore casos de teste para as implementacoes de Multiset (Bag). Note que os testes podem ser
- unicos, pois as assinaturas das funcoes são iguais. Tente pensar nas diversas situações de uso
- da estrutura: quanto ela tem que funcionar com sucesso e quando tem que haver um erro. Para saber
- se o funcionamento está correto é bom também ter um “snapshot” da estrutura ou ver propriedades
- internas.
-}

import qualified MultisetList as L
import qualified MultisetMap as M
import qualified Test.HUnit as T

-- MultisetList

testSearchL = [T.TestCase (T.assertEqual "searchL0" 2 (L.search "a" [("a",2),("b",1)])), T.TestCase (T.assertEqual "searchL1" 0 (L.search "c" [("a",2),("b",1)]))]
testInsertL = [T.TestCase (T.assertEqual "insertL0" [("a",2),("b",1),("c",1)] (L.insert "c" [("a",2),("b",1)])), T.TestCase (T.assertEqual "insertL1" [("a",2),("b",2)] (L.insert "b" [("a",2),("b",1)]))]
testRemoveL = [T.TestCase (T.assertEqual "removeL0" [("a",1),("b",1)] (L.remove "a" [("a",2),("b",1)])), T.TestCase (T.assertEqual "removeL1" [("a",2)] (L.remove "b" [("a",2),("b",1)])), T.TestCase (T.assertEqual "removeL2" [("a",2)] (L.remove "c" [("a",2)]))]
testUnionL = [T.TestCase (T.assertEqual "unionL0" [("a",1),("c",3),("b",2)] (L.union [("a",1),("c",3)] [("b",2)])),T.TestCase (T.assertEqual "unionL1" [("b",2),("a",1),("c",3)] (L.union [("b",2)] [("a",1),("c",3)])), T.TestCase (T.assertEqual "unionL2" [("a",1),("c",3),("b",2)] (L.union [("a",1),("c",3)] [("b",2), ("c",1)]))]
testIntersecL = [T.TestCase (T.assertEqual "intersecL0" [("a",1)] (L.intersection [("a",3),("b",1)] [("a",1)])), T.TestCase (T.assertEqual "intersecL1" [("a",1)] (L.intersection [("a",1)] [("a",3),("b",1)])), T.TestCase (T.assertEqual "intersecL2" [] (L.intersection [("a",1)] []))]
testMinusL = [T.TestCase (T.assertEqual "minusL0" [("b",1)] (L.minus [("a",3),("b",1)] [("a",3)])), T.TestCase (T.assertEqual "minusL1" [] (L.minus [("a",3)] [("a",3)])), T.TestCase (T.assertEqual "minusL2" [] (L.minus [("a",3),("b",2)] [("a",3),("b",4)]))]
testInclusionL = [T.TestCase (T.assertEqual "inclusionL0" False (L.inclusion [("a",3),("b",1)] [("a",3)])), T.TestCase (T.assertEqual "inclusionL1" True (L.inclusion [("a",3)] [("a",3),("b",1)])), T.TestCase (T.assertEqual "inclusionL2" True (L.inclusion [("a",3)] [("a",3)])), T.TestCase (T.assertEqual "inclusionL3" True (L.inclusion [] [("a",3),("b",1)])), T.TestCase (T.assertEqual "inclusionL4" False (L.inclusion [("a",3)] []))]
testSumL = [T.TestCase (T.assertEqual "sumL0" [("a",3),("b",3)] (L.sum [("b",2)] [("a",3),("b",1)])), T.TestCase (T.assertEqual "sumL1" [("a",3),("b",1),("c",2)] (L.sum [("c",2)] [("a",3),("b",1)])), T.TestCase (T.assertEqual "sumL2" [("c",2)] (L.sum [("c",2)] [])),  T.TestCase (T.assertEqual "sumL3" [("c",2)] (L.sum [] [("c",2)]))]
testSizeL = [T.TestCase (T.assertEqual "sizeL0" 6 (L.size [("a",3),("b",3)])), T.TestCase (T.assertEqual "sizeL1" 3 (L.size [("a",3)])), T.TestCase (T.assertEqual "sizeL1" 0 (L.size []))]

testsL = T.TestList (testSearchL ++ testInsertL ++ testRemoveL ++ testUnionL ++ testIntersecL ++ testMinusL ++ testInclusionL ++ testSumL ++ testSizeL)
toTestL = T.runTestTT testsL

-- MultisetMap

emptyMap = M.remove "a" (M.fromList [("a",1)])

testSearchM = [T.TestCase (T.assertEqual "searchM0" 2 (M.search "a" (M.fromList [("a",2),("b",1)]))), T.TestCase (T.assertEqual "searchM1" 0 (M.search "c" (M.fromList [("a",2),("b",1)])))]
testInsertM = [T.TestCase (T.assertEqual "insertM0" (M.fromList [("a",2),("b",1),("c",1)]) (M.insert "c" (M.fromList [("a",2),("b",1)]))), T.TestCase (T.assertEqual "insertM1" (M.fromList [("a",2),("b",2)]) (M.insert "b" (M.fromList [("a",2),("b",1)])))]
testRemoveM = [T.TestCase (T.assertEqual "removeM0" (M.fromList [("a",1),("b",1)]) (M.remove "a" (M.fromList [("a",2),("b",1)]))), T.TestCase (T.assertEqual "removeM1" (M.fromList [("a",2)]) (M.remove "b" (M.fromList [("a",2),("b",1)]))), T.TestCase (T.assertEqual "removeM2" (M.fromList [("a",2)]) (M.remove "c" (M.fromList [("a",2)])))]
testUnionM = [T.TestCase (T.assertEqual "unionM0" (M.fromList [("a",1),("c",3),("b",2)]) (M.union (M.fromList [("a",1),("c",3)]) (M.fromList [("b",2)]))),T.TestCase (T.assertEqual "unionM1" (M.fromList [("b",2),("a",1),("c",3)]) (M.union (M.fromList [("b",2)]) (M.fromList [("a",1),("c",3)]))), T.TestCase (T.assertEqual "unionM2" (M.fromList [("a",1),("c",3),("b",2)]) (M.union (M.fromList [("a",1),("c",3)]) (M.fromList [("b",2), ("c",1)])))]
testIntersecM = [T.TestCase (T.assertEqual "intersecM0" (M.fromList [("a",1)]) (M.intersection (M.fromList [("a",3),("b",1)]) (M.fromList [("a",1)]))), T.TestCase (T.assertEqual "intersecM1" (M.fromList [("a",1)]) (M.intersection (M.fromList [("a",1)]) (M.fromList [("a",3),("b",1)]))), T.TestCase (T.assertEqual "intersecM2" (M.fromList []) (M.intersection (M.fromList [("a",1)]) (M.fromList [])))]
testMinusM = [T.TestCase (T.assertEqual "minusM0" (M.fromList [("b",1)]) (M.minus (M.fromList [("a",3),("b",1)]) (M.fromList [("a",3)]))), T.TestCase (T.assertEqual "minusM1" (M.fromList []) (M.minus (M.fromList [("a",3)]) (M.fromList [("a",3)]))), T.TestCase (T.assertEqual "minusM2" (M.fromList []) (M.minus (M.fromList [("a",3),("b",2)]) (M.fromList [("a",3),("b",4)])))]
testInclusionM = [T.TestCase (T.assertEqual "inclusionM0" False (M.inclusion (M.fromList [("a",3),("b",1)]) (M.fromList [("a",3)]))), T.TestCase (T.assertEqual "inclusionM1" True (M.inclusion (M.fromList [("a",3)]) (M.fromList [("a",3),("b",1)]))), T.TestCase (T.assertEqual "inclusionM2" True (M.inclusion (M.fromList [("a",3)]) (M.fromList [("a",3)]))), T.TestCase (T.assertEqual "inclusionM3" True (M.inclusion (M.fromList []) (M.fromList [("a",3),("b",1)]))), T.TestCase (T.assertEqual "inclusionM4" False (M.inclusion (M.fromList [("a",3)]) (M.fromList [])))]
testSumM = [T.TestCase (T.assertEqual "sumM0" (M.fromList [("a",3),("b",3)]) (M.sum (M.fromList [("b",2)]) (M.fromList [("a",3),("b",1)]))), T.TestCase (T.assertEqual "sumM1" (M.fromList [("a",3),("b",1),("c",2)]) (M.sum (M.fromList [("c",2)]) (M.fromList [("a",3),("b",1)]))), T.TestCase (T.assertEqual "sumM2" (M.fromList [("c",2)]) (M.sum (M.fromList [("c",2)]) (M.fromList []))),  T.TestCase (T.assertEqual "sumM3" (M.fromList [("c",2)]) (M.sum (M.fromList []) (M.fromList [("c",2)])))]
testSizeM = [T.TestCase (T.assertEqual "sizeM0" 6 (M.size (M.fromList [("a",3),("b",3)]))), T.TestCase (T.assertEqual "sizeM1" 3 (M.size (M.fromList [("a",3)]))), T.TestCase (T.assertEqual "sizeM1" 0 (M.size emptyMap))]

testsM = T.TestList (testSearchM ++ testInsertM ++ testRemoveM ++ testUnionM ++ testIntersecM ++ testMinusM ++ testInclusionM ++ testSumM ++ testSizeM)
toTestM = T.runTestTT testsM
