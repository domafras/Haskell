-- Leonardo Mafra Salin

{- 
1) Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada 
-}
soma1 :: Int -> Int
soma1 a = a + 1

{-
2) Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
-}
sempre :: tipo -> Int
sempre a = 0

{-
3) Escreva uma função chamada treco que receba três  valores  em  ponto  flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
-}
treco :: Double -> Double -> Double -> Double
treco a b c = (a + b) * c

{-
4) Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
-}
resto :: Int -> Int -> Int
resto a b = mod a b

{-
5) Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.
-}
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior a b c d = maximum[a, b, c, d]

{-
6a) Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
-}
impar :: Int -> Int -> Bool
impar a b = if mod (a * b) 2 /= 0 then True else False

{-
6b) Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
-}
somaPar :: (Int, Int) ->  Int
somaPar (a, b) = a + b

{-
7) Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦/2 +𝑧
-}
equacao :: Double -> Double -> Double -> Double
equacao x y z = (x * x) + (y / 2) + z

{-
8) Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).
-}
diagnostico :: Double -> Double -> String
diagnostico peso altura
  | imc <= 16.99 = "Muito abaixo do peso"
  | imc <= 18.49 = "Abaixo do peso"
  | imc <= 24.99 = "Peso normal"
  | imc <= 29.99 = "Sobrepeso"
  | imc <= 34.99 = "Obesidade Ieve"
  | imc <= 39.99 = "Obesidade severa"
  | otherwise = "Obesidade morbida"
  where
    imc = peso / (altura * altura)

{-
9) Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:
- 𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4
- 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100
- 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400
- 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
-}
bissexto :: Int -> Bool
bissexto year | mod year 4 == 0 && (mod year 400 == 0 || mod year 100 /= 0) = True
              | otherwise = False


main = do
  putStrLn "\nTrabalho 2 - Introducao Haskell"
  putStrLn "Escrito por: Leonardo Mafra Salin"

  putStrLn "\nExercício 1:"
  --print (soma1 9)
  putStrLn ("Soma1: " ++ show(soma1 9))

  putStrLn "\nExercício 2:"
  --print (sempre "nao é zero")
  putStrLn ("Sempre0: " ++ show(sempre "não é zero"))

  putStrLn "\nExercício 3:"
  --print (treco 1 2 3)
  putStrLn ("Treco: " ++ show(treco 1 2 3))

  putStrLn "\nExercício 4:"
  --print (resto 20 3)
  putStrLn ("Resto: " ++ show(resto 20 3))

  putStrLn "\nExercício 5:"
  --print (precoMaior 1 2 3 4)
  putStrLn ("precoMaior: " ++ show(precoMaior 1 2 3 4))

  putStrLn "\nExercício 6a:"
  --print (impar 3 1)
  putStrLn ("Impar: " ++ show(impar 3 1))

  putStrLn "\nExercício 6b:"
  --print (somaPar (3, 1))
  putStrLn ("Soma par: " ++ show(somaPar (3, 1)))

  putStrLn "\nExercício 7:"
  --print (equacao 1 1 1)
  putStrLn ("Equacao: " ++ show(equacao 1 1 1))

  putStrLn "\nExercício 8:"
  --print (diagnostico 65 1.70)
  putStrLn ("Diagnostico: " ++ show(diagnostico 65 1.70))

  putStrLn "\nExercício 9:"
  --print (bissexto 2000)
  putStrLn ("Bissexto: " ++ show(bissexto 2000))
  
