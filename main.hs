-- Nome: Vinicius de Abreu Bozzi

import System.IO
import System.Random
import Trabalho
import Data.List
import Numeric


main :: IO ()
main = do putStrLn "Forneca o nome do arquivo de entrada: "
          arq_entrada <- getLine
          putStrLn "Forneca o nome do arquivo de saida: "
          arquivo_saida <- getLine
          putStrLn "Forneca o percentual de exemplos de teste: "
          percent_testeS <- getLine
          putStrLn "Forneca o valor da semente para geracao randomizada: "
          sementeS <- getLine
          arquivo_entrada <- readFile arq_entrada          
          let flores = criaTupla $ map (separa ',') $ lines arquivo_entrada -- flores armazena todos os dados, no formato listas de tuplas [(Classe,[pontos])]
          let semente = (read sementeS :: Int)
          let percent_teste = (read percent_testeS :: Int)
          let g = mkStdGen semente
          let nteste = truncate (calculaPorcentagem percent_teste (length flores))
          let aleatorios = take nteste (removeDup (
                       (randomRs (0, length flores-1) g :: [Int])))
          let aleatoriosComplemento = [0..(length flores-1)] \\ aleatorios --funcao que gera o complemento da lista dos aleatorios e será usada para o treino
          let a = reverse aleatoriosComplemento
          let floresTreino = criaLista flores [] a  -- cria uma lista de treino de acordo com os indices do aleatorios complemento, no formato listas de tuplas [(Classe,[pontos])]
          let floresTeste = criaLista flores [] aleatorios  -- cria uma lista de teste de acordo com os indices do aleatorios, no formato listas de tuplas [(Classe,[pontos])]
          let vizinho = classificaClasse floresTreino floresTeste -- cria lista de tuplas da classificacao de acordo com o vizinho mais próximo, no formato [((Classe,[pontos]),ClasseChutada)]
          let acuracia_Vizinho = calculaAcuracia vizinho -- valor da acurácia pelo método do vizinho mais proximo
          let agrupaCentroide = centroideCalculo (juntaClasses floresTreino) -- cria uma lista de tuplas de cada classe com seus pontos(centroides), no formato [(Classe,[centoides])]
          let centroide = classificaClasse agrupaCentroide floresTeste -- cria lista de tuplas da classificacao de acordo com o distancia euclidiana, no formato [((Classe,[pontos]),ClasseChutada)]
          let acuracia_Centroide = calculaAcuracia centroide -- valor da acurácia do centroide 
          let armazena1 = armazenaNomeClasseVerdadeira vizinho -- armazena a lista de string de classes que sao verdadeiras, do metodo do vizinho mais proximo
          let armazena2 = armazenaNomeClasseClassificada vizinho -- armazena a lista de string de classes que sao chutadas, do metodo do vizinho mais proximo
          let armazena3 = armazenaNomeClasseVerdadeira centroide -- armazena a lista de string de classes que sao verdadeiras, do metodo dos centroides
          let armazena4 = armazenaNomeClasseClassificada centroide -- armazena a lista de string de classes que sao chutadas, do metodo dos centroides
          let classes = nub (map fst(flores)) -- filtra todas as classes existentes e armazena em uma lista de String
          let matrix_Vizinho = matrixContada armazena1 armazena2 classes -- gera a matrix de confusao do metodo do vizinho mais proximo, no formato [[Int]]
          let matrix_Centroide = matrixContada armazena3 armazena4 classes -- gera a matrix de confusao dos centroides, no formato [[Int]]
          let impressao_Matriz_Vizinho = imprimeMatrix matrix_Vizinho -- gera uma string da matrix de confusao dos vizinho mais proximo, formatada para impressao
          let impressao_Matriz_Centroide = imprimeMatrix matrix_Centroide -- gera uma string da matrix de confusao dos centroides, formatada para impressao
          writeFile arquivo_saida ( "Forneca o nome do arquivo de entrada: " ++ arq_entrada ++ "\n" ++ 
            "Forneca o nome do arquivo de saida: " ++ arquivo_saida ++ "\n" ++
            "Forneca o percentual de exemplos de teste: " ++ percent_testeS ++ "\n" ++
            "Forneca o valor da semente para geracao randomizada: " ++ sementeS ++ "\n\n" ++
            "Indices de exemplos de teste: " ++ (show aleatorios) ++ "\n\n" ++
            "Acuracia(vizinho): " ++ (showGFloat (Just 2) acuracia_Vizinho "") ++ "%\n" ++
            "Acuracia(centroide): " ++ (showGFloat (Just 2) acuracia_Centroide "") ++ "%\n\n" ++
            "vizinho mais próximo:" ++ "\n" ++
            impressao_Matriz_Vizinho ++ "\n" ++ 
            "centroides:" ++ "\n" ++
            impressao_Matriz_Centroide)
          return()

          

