module Trabalho where

import Data.List
import Data.Function
import Data.Ord
import Text.Read

tem :: Eq a => a -> [a] -> Bool
tem _ [] = False
tem y (x:xs)
   | x == y = True
   | otherwise = tem y xs

removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | tem x ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)


-- Funcao que separa a lista pelo caractere passada (split)
-- Entrada: um caracter e uma lista (string)
-- Saida: uma lista de listas (strings) que foram separadas pelo caractere
-- Exemplo: separa ',' "oi,tudo,bom" gera ["oi","tudo","bom"]
separa :: (Eq a) => a -> [a] -> [[a]]
separa _ [] = []
separa c as = w : separa c r
    where 
        (w,r) = separa' c as []

        separa' _ [] l1 = (l1,[])
        separa' c (a:as) l1
            | a /= c = separa' c as (l1++[a])
            | otherwise = (l1,as)


-- Funcao que retorna a porcentagem de um valor passado x pelo total y
-- Entrada: dois valores inteiros x e y
-- Saida: porcentagem calculada em ponto flutuante
calculaPorcentagem :: Int -> Int -> Double
calculaPorcentagem x y = (fromIntegral(x)/fromIntegral(100))*fromIntegral(y)


-- Funcao que converte uma string(numérica) em Double
-- Entrada: string (numerica)
-- Saida: o numero double correspondente
readDouble :: String -> Double
readDouble = read


-- Funcao de projeto que percorre toda a lista de listas de string que
-- corresponde a cada linha do arquivo de entrada e cria uma lista de 
-- tuplas da classe e dos pontos da classe sendo que o ultimo elemento
-- sempre é o nome da classe
-- Entrada: Uma lista de listas de strings
-- Saida: Uma lista de tuplas de String (nome da classe) e [Double] (pontos)
criaTupla :: [[String]] -> [(String,[Double])]
criaTupla xs = [ (ultimo x, coord x) | x <- xs ]
                where
                    coord = \x -> map (readDouble) (init x)
                    ultimo = last


-- Essa funcao separa a lista de teste de acordo com os indices randomicos
-- Funcao de projeto que "separa" e monta uma lista de tuplas de classe 
-- e pontos do primeiro argumento de acordo com uma lista de Int que 
-- corresponde ao indice de cada elemento dessa lista do primeiro argumento.
-- Entrada: Lista de tupla, lista vazia (auxiliar), lista dos indices (deve
-- ter numeros correspondentes aos indices da lista de tupla)
-- Saida: Lista de tuplas correspondentes aos indices da lista de inteiros
criaLista :: [(String,[Double])] -> [(String,[Double])] -> [Int]-> [(String,[Double])]
criaLista _ xs [] = xs
criaLista ys xs as = criaLista ys (( ys !! (head as) ) : xs) (tail as)


-- A funcao percorre a lista de tuplas de teste gera uma lista de tuplas 
-- de teste e a classificacao correspondente.
-- Entrada: lista de tuplas de treino, lista de tuplas de teste
-- Saida: Lista de tuplas de tuplas de teste e sua classificacao
classificaClasse :: [(String,[Double])] -> [(String,[Double])] -> [((String,[Double]),String)]
classificaClasse xs ys = [ defineClasse z xs (calculaDistancia (snd z) (snd (head xs))) (fst (head xs)) | z <- ys]


-- Funcao que precorre a lista de teste classifica uma tuplas de classe e pontos 
-- de acordo com a distancia de cada tupla da lista de treino
-- Entrada: tupla de classe e pontos, lista de tuplas de treino, Double (distancia)
-- e string (nome da classe de treinamento)
-- Saida: Tupla de tuplas de teste e sua classificacao
defineClasse :: (String,[Double]) -> [(String,[Double])] -> Double -> String -> ((String,[Double]),String)
defineClasse z [] _ nome = (z,nome)
defineClasse z (x:xs) dist nome = if calculaDistancia (snd z) (snd x) < dist 
                    then defineClasse z xs (calculaDistancia (snd z) (snd x)) (fst x)
                    else defineClasse z xs dist nome


-- Funcao calcula a distancia euclidiana entre duas listas de doubles.
-- A funcao recebe as coordenadas/pontos e calcula a distancia euclidiana
-- Entrada: Lista de double, Lista de Double
-- Saida: Distancia euclidiana desses pontos
calculaDistancia :: [Double] -> [Double] -> Double
calculaDistancia coord1 coord2 = sqrt (calculaDistancia coord1 coord2)
                    where
                      calculaDistancia [] _ = 0
                      calculaDistancia _ [] = 0
                      calculaDistancia (x1:xs1) (x2:xs2) = (x1 - x2) ^ 2 + calculaDistancia xs1 xs2


-- Funcao recebe uma lista de tuplas de teste ja classificadas e retorna a porcentagem
-- de acertos entre a classe real e a classe "chutada"
-- Entrada: Lista de tuplas de teste classificadas
-- Saida: Double (porcentagem de acertos)
calculaAcuracia :: [((String,[Double]),String)] -> Double
calculaAcuracia xs = fromIntegral((length (filtro xs))*100)/fromIntegral(length xs)


-- Funcao que monta uma lista de tuplas acordo com a classificacao, ou seja, a funcao
-- percorre a lista de tuplas de teste ja classificadas e compara se a sua real 
-- classe é igual a classe "chutada", se isso ocorre, é adicionada na lista.
-- Entrada Lista de tuplas de teste classificada
-- Saida: Lista de tuplas de teste classificada corretamente
filtro :: [((String,[Double]),String)] -> [((String,[Double]),String)]  
filtro xs = filter (\w -> (fst (fst w)) == (snd w)) xs


-- Funcao que junta os pontos da mesma classe em um uma lista de tuplas de classe
-- e lista de todos os pontos, ou seja, a funcao filtra todos os pontos de uma mesma
-- classe e agrupa em uma lista de tupla
-- Entrada: Lista de tuplas de treino
-- Saida: Lista de tuplas de treino com todos os pontos agrupados em uma lista
juntaClasses :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
juntaClasses = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)


-- Funcao que calcula o centroide (média) de todas as coordenadas (x1..xn) da lista de
-- todos os pontos agrupados de cada classe e retorna uma lista das classes e suas 
-- devidas coordenadas (x1..xn) calculadas.
-- Entrada: Lista de tuplas de classe e a lista de todos os pontos dessa classe.
-- Saida: Lista de tuplas de classe e os pontos calculados pelo centroide.
centroideCalculo :: [(String,[[Double]])] -> [(String,[Double])]
centroideCalculo xs = [(fst x, centroideUnica (length (snd x)) (centroide (snd x))) | x <- xs]


-- Funcao que divide cada elemento de uma lista de Double por uma valor inteiro,
-- ou seja, dada uma lista de double xs, cada elemento será dividida pelo Int tam
-- Entrada: Inteiro e Lista de double
-- Saida: Lista de double
centroideUnica :: Int -> [Double] -> [Double]
centroideUnica tam xs = map (/ fromIntegral(tam)) xs


-- Funcao que faz o somatorio das mesmas coordenadas de pontos de uma lista de lista
-- de double, por exemplo, centroide [[1,2,3],[1,3,4]] gera [2.0,5.0,7.0]
-- Entrada: Lista de lista de double
-- Saida: Lista de double
centroide :: [[Double]] -> [Double] 
centroide (x:[]) = x
centroide (x:xs) = zipWith (+) x $ centroide xs


-- Funcao de projeto que gera uma lista de string de classes verdadeiras, ou seja,
-- a funcao pega do conjunto de teste a classe verdadeira.
-- Entrada: Lista de tuplas de teste ja classificadas
-- Saida: Lista de String de teste com classe verdadeira
armazenaNomeClasseVerdadeira :: [((String,[Double]),String)] -> [String]
armazenaNomeClasseVerdadeira xs = [ fst(fst x) | x <- xs]


-- Funcao de projeto que gera uma lista de string de classes "chutadas", ou seja,
-- a funcao pega do conjunto de teste a classe classificada pelos metodos do vizinho
-- e do centroide.
-- Entrada: Lista de tuplas de teste ja classificadas
-- Saida: Lista de String de teste com classe chutadas
armazenaNomeClasseClassificada :: [((String,[Double]),String)] -> [String]
armazenaNomeClasseClassificada xs = [ snd x | x <- xs]


-- A funcao monta a matrix de confusao.
-- Essa funcao percorre a lista de strings de teste com classificacao verdadeira e
-- e a lista de strings de teste com a classificacao "chutada" e vai realizando
-- somas correspondente para cada classe para montar uma lista de lista de inteiros
-- com cada elemento dessa listas de inteiros é o somatorio das classes da matriz
-- de confusao.
-- Entrada: Lista de String de teste com classe verdadeira, Lista de String de 
-- teste com classe chutada e a lista de todas as classes.
-- Saida: lista de lista de inteiros, correspondente a matriz de confusao.
matrixContada :: [String] -> [String] -> [String] -> [[Int]]
matrixContada strVer strClass classes = [ [ realizasomas x y strVer strClass | x <- classes ] | y <- classes ]

-- Funcao auxiliar que conta a quandade de mesma classe de acordo com a string s1 e s2,
-- ou seja, de acordo com s1 e s2 a funcao vai contar a quantidade de vezes que s1 e s2
-- sao iguais nas mesmas posicoes da matrix lstr e lste.
-- Exemplo: realizasomas "ver" "ver" ["ver","ver","vir"] ["ver","vir","ver"] gera 1
-- Exemplo 2: realizasomas "ver" "ver" ["ver","ver","vir"] ["ver","ver","ver"] gera 2
-- Entrada: String 1, String 2, Lista de String (lista de string treino), Lista de
-- String (lista de string teste)
-- Saida: Inteiro
realizasomas :: String -> String -> [String] -> [String] -> Int
realizasomas s1 s2 lstr lste = sum [ 1 | (s, o) <- (zip lstr lste), s1 == s, s2 == o]


-- Funcao que pega a matrix de confusao e gera uma string.
-- A funcao chama funcoes auxiliares para percorrer a lista de lista de inteiros e
-- vai passando cada lista e concatenando os resultados recursivamente.
-- Entrada: Lista de Lista de Int (matriz de confusao)
-- Saida: String em linha unica correspondente a matriz de confusao para impressao
imprimeMatrix :: [[Int]] -> String
imprimeMatrix (x:[]) = print_ x
imprimeMatrix (x:xs) = print_ x ++ imprimeMatrix xs


-- Funcao auxiliar que pega um Vetor de inteiros e gera uma string.
-- A funcao vai passando cada elemento da lista de int e vai chamando outra funcao
-- para transformar esse numero em string e vai concatenando recursivamente até
-- gerar uma lista de string concatenada.
-- Entrada: Lista de Int
-- Saida: String concatenada 
print_ :: [Int] -> String
print_ (x:[]) = printAux_ x ++ "\n"
print_ (x:xs) = printAux_ x ++ "," ++ print_ xs


-- Funcao que tranforma um inteiro em uma string sempre de tamanho 3.
-- Entrada: Inteiro a ser convertido
-- Saida: String de tamanho 3
printAux_ :: Int -> String
printAux_ x = reverse $ take 3 ((reverse $ (show x)) ++ [' ' | i <- [0..]]) 


