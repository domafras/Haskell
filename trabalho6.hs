-- Leonardo Mafra Salin

{-
1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.
-}
divisoresden :: Int -> [Int]
divisoresden n = [ x | x <- [1..(n - 1)], n `mod` x == 0]

{-
2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a  ocorrência de um caractere específico, em uma string dada.
-}
contaCaractere :: String -> Char -> Int
contaCaractere str char = length [ a | a <- str, a == char]

{-
3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. 
-}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo n = [ 2 * x | x <- n, x >= 0]

{- 
4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. 
-}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x + y > z && x + z > y && y + z > x]

{-
5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. 
-}
somaDigAux :: [Int] -> Int
somaDigAux [] = 0
somaDigAux (h:t) = h + somaDigAux t

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos x = [a | a <- [1..x], (somaDigAux (divisoresden a)) == a]

{-
6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no 
prelude que podem ser úteis. 
-}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar x y = somaDigAux [ fst n * snd n  | n <- zip x y ]


{-
7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2. 
-}
primo :: Int -> Bool
primo n = (length (divisoresden n)) == 1

primeirosPrimos :: Int -> [Int]
primeirosPrimos n = take n [ x | x <- [1..], primo x]

{-
8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. 
-}
paresOrdenados :: Double -> [(Double, Double)]
paresOrdenados n = [(x * x, x * x * x) | x <- [1..n]]

--Testes:
main = do
  putStrLn ("\nTrabalho 6 - List Comprehension")
  putStrLn ("Escrito por Leonardo Mafra Salin")

  putStrLn ("\nExercício 1")
  putStrLn ("Divisores de N: " ++ show(divisoresden 15))

  putStrLn ("\nExercício 2")
  putStrLn ("Conta caractere: " ++ show(contaCaractere "arara" 'a'))
  putStrLn ("Conta caractere: " ++ show(contaCaractere "arara" 'r'))

  putStrLn ("\nExercício 3")
  putStrLn ("Dobro nao negativo " ++ show(dobroNaoNegativo [1,2,(-3)]))  

  putStrLn ("\nExercício 4")
  putStrLn ("Pitagoras: " ++ show(pitagoras 2))

  putStrLn ("\nExercício 5")
  putStrLn ("Numeros perfeitos: " ++ show(numerosPerfeitos 100))

  putStrLn ("\nExercício 6")
  putStrLn ("Produto escalar: " ++ show(produtoEscalar [1,2,3] [1,2,3]))

  putStrLn ("\nExercício 7")
  putStrLn ("Primeiros primos: " ++ show(primeirosPrimos 5))

  putStrLn ("\nExercício 8")
  putStrLn ("Pares ordenados: " ++ show(paresOrdenados 5))