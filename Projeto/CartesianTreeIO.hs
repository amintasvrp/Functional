import CartesianTree as CAT
import qualified System.Process as SP
import Data.Tree as DT
import Data.Tree.Pretty as DTP

clearScreen = SP.system "clear" {- limpatela -}

descricao = do
  putStrLn "                                    -- CARTESIAN TREE --              "
  putStrLn "                                        *Descrição*"
  putStrLn "           Uma cartesian tree é uma arvore binaria que é derivada de uma sequencia"
  putStrLn "           de numeros. E tem as seguintes propriedades:"
  putStrLn "          -> Cada nó da árvore corresponde a um único valor na sequencia de números"
  putStrLn "          -> Percorrer a árvore no sentido em-ordem resulta na sequencia original"
  putStrLn "          -> A árvore possui a propriedade da Heap Tree: O pai de qualquer nó que não"
  putStrLn "             seja a raíz possui menor (Min-Heap Cartesian Tree) ou maior (Max-Heap"
  putStrLn "             Cartesian Tree) valor do que o valor do nó corrente."
  putStrLn "          -> A raíz possui o menor valor (Min-Heap Cartesian Tree) ou maior valor"
  putStrLn "             (Max-Heap Cartesian Tree) de toda a árvore."
  putStrLn ""
  putStrLn "Pressione ENTER"
  getChar

main = do
  descricao
  putStrLn "  -- Menu Principal --"
  putStrLn " "
  putStrLn "Escolha o tipo da Cartesian Tree:"
  putStrLn "1. Max-Heap CartesianTree"
  putStrLn "2. Min-Heap CartesianTree"
  op <- getLine
  putStrLn ""
  menuEscolhaCT op

menuEscolhaCT op
  |op == "1" = do
                  clearScreen
                  putStrLn "                 Max-Heap CartesianTree        "
                  putStrLn " "
                  putStrLn "Digite a sequência de números separadas por espaço:"
                  input <- getLine
                  let sequencia = (map read $ words input :: [Int])
                  let tipo = "MaxCT"
                  if(length(sequencia) == 0) then do
                    putStrLn ("Sequência de números inválida. Aperte para tentar novamente:")
                    getLine
                    menuEscolhaCT op
                  else do
                    let cartesianTree = CAT.buildMaxCT sequencia
                    putStrLn ("CartesianTree construída com sucesso ;). (Enter para continuar)")
                    getLine
                    menuOperacoes tipo cartesianTree

  |op == "2" =  do
                  clearScreen
                  putStrLn "                 Min-Heap CartesianTree        "
                  putStrLn " "
                  putStrLn "Digite a sequência de números separadas por espaço:"
                  input <- getLine
                  let sequencia = (map read $ words input :: [Int])
                  let tipo = "MinCT"
                  if(length(sequencia) == 0) then do
                    putStrLn "Sequência de números inválida. Aperte para tentar novamente:"
                    getLine
                    menuEscolhaCT op
                  else do
                    let cartesianTree = CAT.buildMinCT sequencia
                    putStrLn "CartesianTree construída com sucesso ;). (Enter para continuar)"
                    getLine
                    menuOperacoes tipo cartesianTree

  |otherwise = do
                  putStrLn "Você precisa escolher direito!!"
                  main

                    {-MENU DE OPERAÇÕES-}

menuOperacoes tipo cartesianTree = do
  clearScreen
  putStrLn "                                  --- MENU DE OPERAÇÕES --- "
  putStrLn " "
  putStrLn ("Agora que voce tem sua CartesianTree, você pode realizar sobre ela as seguintes operações:")
  putStrLn("1. Inserir um elemento")
  putStrLn("2. Remover um elemento")
  putStrLn("3. Split")
  putStrLn("4. Merge de duas cartesianTree")
  putStrLn("5. Caminhamentos (Pre-Ordem, Em-Ordem, Pos-Ordem)")
  putStrLn("6. Detalhes")
  putStrLn("7. Voltar ao Menu Principal")
  input <- getLine
  case input of

    "1" -> do
      clearScreen
      putStrLn "       --- INSERIR ---           "
      putStrLn " "
      putStr "Digite o valor que deseja inserir: "
      input2 <- getLine
      let elemento = (read input2 :: Int)
      insereNaCT elemento tipo cartesianTree

    "2"-> do
      clearScreen
      putStrLn "           --- REMOVER ---             "
      putStrLn " "
      putStrLn "Abaixo está o vetor em-ordem da sua CT:"
      print (CAT.order cartesianTree)
      putStrLn "Digite o índice do valor que deseja remover: "
      input2 <- getLine
      let indice = (read input2 :: Int)
      removeDaCT indice tipo cartesianTree

    "3"-> do
      clearScreen
      putStrLn "                             --- SPLIT ---       "
      putStrLn " "
      putStrLn "A função Split recebe como parâmetro o índice do nó onde o split será feito."
      putStrLn "A árvore então é dividida neste nó e duas sub-árvores são criadas a partir"
      putStrLn "destes dois conjuntos de valores."
      putStrLn "Retorno: Uma tupla de Cartesian Trees"
      putStrLn " "
      putStrLn "Digite o índice do nó onde deseja que o split seja feito:"
      print (CAT.order cartesianTree)
      input2 <- getLine
      let indice = (read input2 :: Int)
      split tipo indice cartesianTree

    "4" -> do
      clearScreen
      putStrLn "                     --- MERGE ---        "
      putStrLn "              Merge de duas Cartesian Tree"
      putStrLn " "
      putStrLn "Digite 1 para realizar o merge de duas Max-Heap CartesianTree"
      putStrLn "Digite 2 para realizar o merge de duas Min-Heap CartesianTree"
      tipoMerge <- getLine
      putStrLn " "
      putStrLn "Ok. Agora digite a sequencia de valores da primeira Cartesian Tree:"
      input2 <- getLine
      let sequencia1 = (map read $ words input2 :: [Int])
      putStrLn "Digite a sequencia de valores da segunda Cartesian Tree:"
      input2 <- getLine
      let sequencia2 = (map read $ words input2 :: [Int])
      mergeCT tipoMerge sequencia1 sequencia2

    "5" -> do
      clearScreen
      putStrLn "                  --- Caminhamentos --- "
      putStrLn " "
      putStrLn "Retorna um array; resultado da operação de navegar na árvore"
      putStrLn "de uma destas três formas: Pré-Ordem, Em-Ordem ou Pós-Ordem."
      putStrLn " "
      putStrLn "Escolha como deseja navegar na árvore atual:"
      putStrLn "1. Pré-Ordem"
      putStrLn "2. Em-Ordem"
      putStrLn "3. Pós-Ordem"
      opcao <- getLine
      navegarNaCT opcao tipo cartesianTree

    "6" -> do
      clearScreen
      detalhes tipo cartesianTree

    "7"-> do
      clearScreen
      main

    otherwise -> do
      putStrLn "Você precisa escolher direito! Tente de novo: (Apert Enter)"
      getChar
      menuOperacoes tipo cartesianTree


                                {-Lógica de Negocio-}

insereNaCT elemento tipo cartesianTree = do
  if tipo == "MaxCT"
    then do
      let cartesianTree2 = CAT.insertMaxCT elemento cartesianTree
      putStrLn "Elemento inserido!"
      print (CAT.order cartesianTree2)
      putStrLn ""
      putStrLn "Aperte Enter para voltar ao menu de operações:"
      getLine
      menuOperacoes tipo cartesianTree2
    else do
      let cartesianTree2 = CAT.insertMinCT elemento cartesianTree
      putStrLn "Elemento inserido!"
      print (CAT.order cartesianTree2)
      putStrLn ""
      putStrLn "Aperte Enter para voltar ao menu de operações:"
      getLine
      menuOperacoes tipo cartesianTree2

removeDaCT indice tipo cartesianTree = do
  if tipo == "MaxCT"
    then do
      let cartesianTree2 = CAT.removeMaxCT indice cartesianTree
      putStrLn "Elemento removido!"
      print (CAT.order cartesianTree2)
      putStrLn ""
      putStrLn "Aperte Enter para voltar ao menu de operações:"
      getLine
      menuOperacoes tipo cartesianTree2
    else do
      let cartesianTree2 = CAT.removeMinCT indice cartesianTree
      putStrLn "Elemento removido!"
      print (CAT.order cartesianTree2)
      putStrLn ""
      putStrLn "Aperte Enter para voltar ao menu de operações:"
      getLine
      menuOperacoes tipo cartesianTree2

mergeCT tipoMerge sequencia1 sequencia2
  | tipoMerge == "1" = do
    let ct1 = CAT.buildMaxCT sequencia1
    let ct2 = CAT.buildMaxCT sequencia2
    let cartesianMerge = CAT.mergeMaxCT ct1 ct2
    print (CAT.order cartesianMerge)
    putStrLn "Merge realizado com sucesso! Esta agora é a sua nova CartesianTree"
    putStrLn ""
    putStrLn "Aperte Enter para voltar ao menu de operações:"
    getChar
    menuOperacoes "MaxCT" cartesianMerge
  | otherwise = do
    let ct1 = CAT.buildMinCT sequencia1
    let ct2 = CAT.buildMinCT sequencia2
    let cartesianMerge = CAT.mergeMinCT ct1 ct2
    print (CAT.order cartesianMerge)
    putStrLn "Merge realizado com sucesso! Esta agora é a sua nova CartesianTree"
    putStrLn ""
    putStrLn "Aperte Enter para voltar ao menu de operações:"
    getChar
    menuOperacoes "MinCT" cartesianMerge

navegarNaCT opcao tipo cartesianTree
  | opcao == "1" = do
    print (CAT.preOrder cartesianTree)
    putStrLn ""
    putStrLn "Aperte Enter para voltar ao menu de operações:"
    getLine
    menuOperacoes tipo cartesianTree

  | opcao == "2" = do
    print (CAT.order cartesianTree)
    putStrLn ""
    putStrLn "Aperte Enter para voltar ao menu de operações:"
    getLine
    menuOperacoes tipo cartesianTree

  | opcao == "3" = do
    print (CAT.postOrder cartesianTree)
    putStrLn ""
    putStrLn "Aperte Enter para voltar ao menu de operações:"
    getLine
    menuOperacoes tipo cartesianTree

split tipo indice cartesianTree = do
  let cartesianTree2 = CAT.splitCT indice cartesianTree
  print (CAT.order (fst cartesianTree2), CAT.order (snd cartesianTree2))
  putStrLn ""
  putStrLn "Aperte Enter para voltar ao menu de operações:"
  getLine
  menuOperacoes tipo cartesianTree

detalhes tipo CAT.NIL = do
  putStr "Árvore Vazia"
detalhes tipo cartesianTree  = do
  putStrLn "Tamanho da árvore: "
  print (CAT.sizeCT cartesianTree)
  putStrLn "Altura da árvore: "
  print (CAT.heightCT cartesianTree)
  putStrLn "Array de origem: "
  print (CAT.order cartesianTree)
  putStrLn "Estrutura da árvore: "
  printCT cartesianTree
  putStrLn "Aperte Enter para voltar ao menu de operações:"
  getChar
  menuOperacoes tipo cartesianTree

printCT NIL = putStrLn "Árvore Vazia\n"
printCT (CAT.Node x y left right) = putStrLn $ DTP.drawVerticalTree tree
                                          where
                                            tree = stringTree (CAT.Node x y left right)

stringTree :: Show y => CT Int y -> DT.Tree String
stringTree (CAT.NIL) = (DT.Node "NIL" [])
stringTree (CAT.Node x y left NIL) = (DT.Node (show y) [(stringTree left), (DT.Node "NIL" [])])
stringTree (CAT.Node x y NIL right) = (DT.Node (show y) [(DT.Node "NIL" []), (stringTree right)])
stringTree (CAT.Node x y left right) = (DT.Node (show y) [(stringTree left), (stringTree right)])
  
  
