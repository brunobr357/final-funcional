{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)
import Distribution.Simple.Setup (trueArg)
import Data.List (groupBy, sortOn)

{- Questão 1:
Dado um array ordenado e um número x, 
encontre um par (a,b) de números pertencentes ao array tal que (a + b) 
se aproxime o máximo possível de x
-}

menorDistancia :: (Ord b, Num b) => b -> [b] -> (b,b)
menorDistancia target [] = error "lista vazia"
menorDistancia target [x] = error "lista com 1 elem"
menorDistancia target (x:xs) = minimumBy (comparing (dist target)) (geraTuplasPossiveis (x:xs))

geraTuplasPossiveis :: [b] -> [(b, b)]
geraTuplasPossiveis [] = []
geraTuplasPossiveis [x] = []
geraTuplasPossiveis (x:xs) = map (\v -> (x,v)) xs ++ geraTuplasPossiveis xs

dist :: (Ord a, Num a) => a -> (a, a) -> a
dist x (y,z) = abs distancia
    where
        distancia = x - soma
        soma = y + z

{- Questão 2: 
Encontrar floor e ceil de um número x dentro de um array a. 
O número x pode não estar no array a. O floor(x) é o número do array a que é menor que x e que mais se aproxima de x 
(pode existir mais de um número menor que x, o floor é o maior deles). 
Dualmente, o ceil(x)  é o número do array a que é maior que x e que mais se aproxima de x 
(pode existir mais de um número maior do que x, o ceil é o menor deles)
-}

floorArray target = maximum . filter (< target)

ceilArray target = minimum . filter (> target)

{- Questão 3:
Implementar uma pilha e seus algoritmos em Haskell. 
Use a lista de Haskell como estrutura sobrejacente e operações que não sejam acesso pelo índice.
-}

adicionarPilha :: a -> [a] -> [a]
adicionarPilha y [] = [y]
adicionarPilha y x = y:x

removerPilha [] = []
removerPilha [x] = []
removerPilha (x:xs) = xs

primeiroElementoPilha [] = error "pilha vazia"
primeiroElementoPilha [x] = x
primeiroElementoPilha (x:xs) = x

isEmptyPilha [] = True
isEmptyPilha _ = False

tamanhoPilha [] = 0
tamanhoPilha (x:xs) = 1 + tamanhoPilha xs


{- Questão 4:
Implementar uma Fila e seus algoritmos em Haskell. 
Use a lista de Haskell como estrutura sobrejacente e operações que não sejam acesso pelo índice.
-}

adicionarFila :: a -> [a] -> [a]
adicionarFila y [] = [y]
adicionarFila y (x:xs) = x:(adicionarFila y xs)

removerFila :: [a] -> [a]
removerFila [] = []
removerFila [x] = []
removerFila (x:xs) = xs

primeiroElementoFila [] = error "fila vazia"
primeiroElementoFila [x] = x
primeiroElementoFila (x:xs) = x

isEmptyFila [] = True
isEmptyFila _ = False

tamanhoFila [] = 0
tamanhoFila (x:xs) = 1 + tamanhoFila xs

{- Questão 5:
Um aluno é representado como uma estrutura contando matrícula, um primeiro nome, um sobrenome, período de ingresso e CRA.
a) Implemente a estrutura que representa um aluno
b) Implemente uma função que calcula a média dos CRAs dos alunos (dispostos em uma lista) usando o operador foldr (você não pode usar map)
c) Implemente uma função que realiza o groupBy dos alunos por CRA. Ou seja, dada uma lista de alunos, a função retorna uma lista de pares 
(cra, [Aluno]), agrupando alunos com um mesmo CRA em pares cujo primeiro elemento é o CRA e o segundo é uma lista de aluno
-}

-- 5. a)
data Aluno = Aluno {
    matricula :: String,
    nome :: String,
    sobrenome :: String,
    periodoIngresso :: String,
    cra :: Double
}

instance Show Aluno where
  show (Aluno matricula primeiroNome sobrenome entrada cra) =
    show primeiroNome ++ " " ++ show sobrenome 

-- 5. b)
calculaMediaCRAs :: [Aluno] -> Double
calculaMediaCRAs [] = error "lista vazia"
calculaMediaCRAs listaAlunos =  soma / quantidade
    where
        soma = foldr (\y v -> v + y.cra) 0 listaAlunos
        quantidade = fromIntegral (length listaAlunos)

-- 5. c)
groupByCRA :: [Aluno] -> [(Double, [Aluno])]
groupByCRA alunos = 
    map criarGrupo grupos
  where
    alunosOrdenados = sortOn cra alunos
    grupos = groupBy mesmosCRA alunosOrdenados
    mesmosCRA aluno1 aluno2 = cra aluno1 == cra aluno2
    criarGrupo grupo = (cra (head grupo), grupo)

-- Testes (rodando o main, os testes são executados)

main :: IO ()
main = do
    -- Questão 1
  putStrLn "Teste menorDistancia:"
  print $ menorDistancia 10 [1, 3, 5, 7, 9] == (1, 9)
  print $ menorDistancia 8 [1, 2, 4, 6, 8] == (2, 6)

    -- Questão 2
  putStrLn "\nTeste floorArray:"
  print $ floorArray 5 [1, 2, 4, 6, 8] == 4
  print $ floorArray 7 [1, 3, 5, 9] == 5 

  putStrLn "\nTeste ceilArray:"
  print $ ceilArray 5 [1, 2, 4, 6, 8] == 6
  print $ ceilArray 3 [1, 2, 3, 5] == 5 

    -- Questão 3
  putStrLn "\nTeste adicionarPilha:"
  print $ adicionarPilha 5 [1, 2, 3] == [5, 1, 2, 3]
  print $ adicionarPilha 9 [] == [9]

  putStrLn "\nTeste removerPilha:"
  print $ removerPilha [1, 2, 3] == [2, 3]
  print $ removerPilha [5] == []

  putStrLn "\nTeste primeiroElementoPilha:"
  print $ primeiroElementoPilha [1, 2, 3] == 1
  print $ primeiroElementoPilha [5] == 5

  putStrLn "\nTeste isEmptyPilha:"
  print $ isEmptyPilha [] == True
  print $ isEmptyPilha [1, 2, 3] == False

  putStrLn "\nTeste tamanhoPilha:"
  print $ tamanhoPilha [1, 2, 3] == 3
  print $ tamanhoPilha [] == 0

    -- Questão 4
  putStrLn "\nTeste adicionarFila:"
  print $ adicionarFila 5 [1, 2, 3] == [1, 2, 3, 5]
  print $ adicionarFila 9 [] == [9]

  putStrLn "\nTeste removerFila:"
  print $ removerFila [1, 2, 3] == [2, 3]
  print $ removerFila [5] == []

  putStrLn "\nTeste primeiroElementoFila:"
  print $ primeiroElementoFila [1, 2, 3] == 1  
  print $ primeiroElementoFila [5] == 5

  putStrLn "\nTeste isEmptyFila:"
  print $ isEmptyFila [] == True
  print $ isEmptyFila [1, 2, 3] == False

  putStrLn "\nTeste tamanhoFila:"
  print $ tamanhoFila [1, 2, 3] == 3
  print $ tamanhoFila [] == 0

    -- Questão 5
    -- b)
  putStrLn "\nTeste calculaMediaCRAs:"
  print $ calculaMediaCRAs [Aluno "111" "Ana" "Beatriz" "2019.1" 7.6, 
                            Aluno "112" "Bruno" "Castro" "2020.1" 5.9, 
                            Aluno "113" "Caio" "Davi" "2021.2" 6.8,
                            Aluno "114" "Diego" "Emanuel" "2019.2" 7.1]
  
  putStrLn "\n Teste groupByCRA:"
  print $ groupByCRA [Aluno "111" "Ana" "Beatriz" "2019.1" 5.9, 
                            Aluno "112" "Bruno" "Castro" "2020.1" 5.9, 
                            Aluno "113" "Caio" "Davi" "2021.2" 7.1,
                            Aluno "114" "Diego" "Emanuel" "2019.2" 7.1]


