-- Leonardo Mafra Salin

{-
1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell.
-}
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci f = fibonacci (f -1) + fibonacci (f -2)

sequencia :: Int -> [Int]
sequencia 0 = []
sequencia s = [fibonacci f | f <- [1 .. s]]

{-
2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
-}
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

{-
3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e  recursividade.
-}
somaDigitos :: Int -> Int
somaDigitos d
  | d < 10 = d
  | otherwise = (d `mod` 10) + somaDigitos (d `div` 10)

{-
4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5.
-}
multiplos3ou5 = sum [n | n <- [1 .. 10000], mod n 3 == 0 || mod n 5 == 0]

{-
5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
-}
diff :: [Int] -> Int -> Int -> Int
diff [] q p = q - (p * p)
diff l q p = diff (tail l) (q + (head l) * (head l)) (p + head l)

{-
6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
-}
crivo :: Int -> [Int] -> [Int]
crivo limit [] = []
crivo limit list
  | head list > limit = list
  | otherwise = [head list] ++ crivo limit ([l | l <- tail list, l `mod` (head list) /= 0])

primo :: Int -> [Int]
primo p = crivo p [2..p]

{-
7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
-}
abaixo :: Int -> Int -> Int -> [Int] -> [Int]
abaixo penultimo ultimo limite elementos
  | limite > ultimo + penultimo = abaixo ultimo (penultimo + ultimo) limite (elementos ++ [ultimo + penultimo])
  | otherwise = elementos

lucas :: Int -> [Int]
lucas 1 = [1]
lucas 2 = [2]
lucas l = if l < 1 then [] else abaixo 2 1 l [2, 1]

{-
8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
-}
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x : xs) = aoContrario (xs) ++ (x : [])

{-
9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
-}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x y = x + somaRecursiva x (y - 1)

{-
10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
-}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (a : z) = comprimento z + 1

{-
Testes:
-}
main = do
  putStrLn "\nTrabalho 3 - Listas e Recursão"
  putStrLn "Escrito por: Leonardo Mafra Salin"

  putStrLn "\nExercício 1:"
  putStrLn ("Sequência Fibonacci: " ++ show (sequencia 6))

  putStrLn "\nExercício 2:"
  putStrLn ("Calculo MDC: " ++ show (mdc 13 22))

  putStrLn "\nExercício 3:"
  putStrLn ("Soma dígitos: " ++ show (somaDigitos 510))

  putStrLn "\nExercício 4:"
  putStrLn ("Soma multiplos: " ++ show (multiplos3ou5))

  putStrLn "\nExercício 5:"
  putStrLn ("Diferenca: " ++ show (diff [1, 2, 3] 0 0))

  putStrLn "\nExercício 6:"
  putStrLn ("Primos: " ++ show (primo (20)))

  putStrLn "\nExercício 7:"
  putStrLn ("Lucas: " ++ show (lucas (18)))

  putStrLn "\nExercício 8:"
  putStrLn ("Ao contrario: " ++ show (aoContrario [1,2,3]))
  
  putStrLn "\nExercício 9:"
  putStrLn ("Soma recursiva: " ++ show (somaRecursiva 5 5))

  putStrLn "\nExercício 10 :"
  putStrLn ("Comprimento: " ++ show (comprimento [1, 2, 3]))