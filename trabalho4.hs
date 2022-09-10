import Data.Char

{-
1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n. 
-}
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1..n]

{-
2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados. 
-}
quadradoReal :: [Double] -> [Double]
quadradoReal [] = []
quadradoReal n = map (^2) n

{-
3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
-}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras p = map length p

{-
4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29
-}
multiplo :: Int -> Bool
multiplo n = n `mod` 29 == 0

maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter multiplo [1..100000])

{-
5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
-}              
maiorMultiploDe :: Int  -> Int
maiorMultiploDe 0 = 0
maiorMultiploDe n = last (filter (\x -> x `mod` n == 0) [0..100000])

{-
6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1² +2² +3² +4²...+𝑛². 
-}
somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 [ p*p | p  <- [1..n]]

{-
7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
-}
comprimento :: [Int] -> Int
comprimento xs = foldl (\y _ -> y+1) 0 xs

{-
8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos
-}

{-
  putStrLn ("\nFlip: " ++ show (flip (++) "amo" "te "))
  putStrLn ("Flip: " ++ show (flip (++) "vigne" "avril la"))

  putStrLn ("\nOrd: " ++ show(ord 'a'))
  putStrLn ("Ord: " ++ show(ord '\n'))

  putStrLn ("\nMax: " ++ show(max 1 10))
  putStrLn ("Max: " ++ show(max 1.1 1))

  putStrLn ("\nMin: " ++ show(min 1 10))
  putStrLn ("Min: " ++ show(min 1.1 1))

  putStrLn ("\nCurry: " ++ show (curry fst 1 2))
  putStrLn ("Curry: " ++ show (curry snd 1 2))

  putStrLn ("\nUncurry: " ++ show (uncurry (+) (4, 6)))
  putStrLn ("Uncurry: " ++ show (uncurry mod (1, 2)))
-}


--Testes
main = do

  putStrLn "\nTrabalho 4 - Funcoes de alta ordem"
  putStrLn "Escrito por: Leonardo Mafra Salin"

  putStrLn "\nExercício 1:"
  putStrLn ("Fatorial: " ++ show(fatorialn 5))

  putStrLn "\nExercício 2:"
  putStrLn ("Quadrado real: " ++ show(quadradoReal [1, 5, 10]))

  putStrLn "\nExercício 3:"
  putStrLn ("Comprimento palavras: " ++ show(comprimentoPalavras ["", "l", "leo"]))

  putStrLn "\nExercício 4:"
  putStrLn ("Maior multiplo de 29: " ++ show(maiorMultiploDe29))

  putStrLn "\nExercício 5:"
  putStrLn ("Maior multiplo de X: " ++ show(maiorMultiploDe 29))

  putStrLn "\nExercício 6:"
  putStrLn ("Soma quadrados: " ++ show(somaQuadrados 5))

  putStrLn "\nExercício 7:"
  putStrLn ("Comprimento lista: " ++ show(comprimento [1,2,3,4,99]))

  putStrLn "\nExercício 8:"
  
  putStrLn ("\nFlip: " ++ show (flip (++) "amo" "te "))
  putStrLn ("Flip: " ++ show (flip (++) "vigne" "avril la"))

  putStrLn ("\nOrd: " ++ show(ord 'a'))
  putStrLn ("Ord: " ++ show(ord '\n'))

  putStrLn ("\nMax: " ++ show(max 1 10))
  putStrLn ("Max: " ++ show(max 1.1 1))

  putStrLn ("\nMin: " ++ show(min 1 10))
  putStrLn ("Min: " ++ show(min 1.1 1))

  putStrLn ("\nCurry: " ++ show (curry fst 1 2))
  putStrLn ("Curry: " ++ show (curry snd 1 2))

  putStrLn ("\nUncurry: " ++ show (uncurry (+) (4, 6)))
  putStrLn ("Uncurry: " ++ show (uncurry mod (1, 2)))