-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C    --- Loop E C: Executa E vezes o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String, Float)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Float
procuraVar [] s = error ("Variável " ++ s ++ " não definida no estado")
procuraVar ((s, i):xs) v
  | s == v    = i
  | otherwise = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Float -> Memoria
mudaVar [] v n = error ("Variável " ++ v ++ " não definida no estado")
mudaVar ((s, i):xs) v n
  | s == v    = ((s, n):xs)
  | otherwise = (s, i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

ebigStep :: (E, Memoria) -> Float
ebigStep (Var x, s) = procuraVar s x
ebigStep (Num n, s) = fromIntegral n
ebigStep (Soma e1 e2, s) = ebigStep (e1, s) + ebigStep (e2, s)
ebigStep (Sub e1 e2, s) = ebigStep (e1, s) - ebigStep (e2, s)
ebigStep (Mult e1 e2, s) = ebigStep (e1, s) * ebigStep (e2, s)
ebigStep (Div e1 e2, s) = ebigStep (e1, s) / ebigStep (e2, s)

bbigStep :: (B, Memoria) -> Bool
bbigStep (TRUE, _)  = True
bbigStep (FALSE, _) = False
bbigStep (Not b, s) 
   | bbigStep (b, s) = False
   | otherwise       = True 
bbigStep (And b1 b2, s) =
   bbigStep (b1, s) && bbigStep (b2, s)
bbigStep (Or b1 b2, s) =
   bbigStep (b1, s) || bbigStep (b2, s)
bbigStep (Leq e1 e2, s) =
   ebigStep (e1, s) <= ebigStep (e2, s)
bbigStep (Igual e1 e2, s) =
   ebigStep (e1, s) == ebigStep (e2, s)

cbigStep :: (C, Memoria) -> (C, Memoria)
cbigStep (Skip, s) = (Skip, s)
cbigStep (If b c1 c2, s) 
  | bbigStep (b, s) = cbigStep (c1, s)
  | otherwise       = cbigStep (c2, s)
cbigStep (Seq c1 c2, s) =
  let (_, s1) = cbigStep (c1, s)
  in cbigStep (c2, s1)
cbigStep (Atrib (Var x) e, s) = 
  let valor = ebigStep (e, s)
      s' = mudaVar s x valor
  in (Skip, s')
cbigStep (While b c, s) 
  | bbigStep (b, s) = 
      let (_, s1) = cbigStep (c, s)
      in cbigStep (While b c, s1)
  | otherwise = (Skip, s)

cbigStep (DoWhile c b, s) =
  let (_, s1) = cbigStep (c, s)
  in if bbigStep (b, s1)
     then cbigStep (DoWhile c b, s1)
     else (Skip, s1)

cbigStep (Loop e c, s) =
  let n = floor (ebigStep (e, s))  -- Converte Float para Int
      loop 0 s' = (Skip, s')
      loop n s' =
        let (_, s1) = cbigStep (c, s')
        in loop (n - 1) s1
  in loop n s

cbigStep (Swap (Var x) (Var y), s) =
  let valorX = procuraVar s x
      valorY = procuraVar s y
      s1 = mudaVar s x valorY
      s2 = mudaVar s1 y valorX
  in (Skip, s2)

cbigStep (DAtrrib (Var x) (Var y) e1 e2, s) =
  let valorE1 = ebigStep (e1, s)
      valorE2 = ebigStep (e2, s)
      s1 = mudaVar s x valorE1
      s2 = mudaVar s1 y valorE2
  in (Skip, s2)

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


-- exemplo loop
programaLoopComplexo :: C
programaLoopComplexo = Seq
    (Atrib (Var "s") (Num 0))      -- Inicializa `s` com 0
    (Seq
        (Atrib (Var "i") (Num 1))  -- Inicializa `i` com 1
        (Loop (Var "n")             -- Loop que executa `n` vezes
            (Seq
                (Atrib (Var "s") (Soma (Var "s") (Var "i"))) -- Adiciona `i` a `s`
                (Atrib (Var "i") (Soma (Var "i") (Num 1))) -- Incrementa `i`
            )
        )
    )

--exemplo dupla atribuição
programaDuplaAtribuicao :: C
programaDuplaAtribuicao = DAtrrib (Var "x") (Var "y") (Num 10) (Num 20)

--exemplo While
programaWhile :: C
programaWhile = While (Leq (Var "counter") (Num 10)) 
                       (Atrib (Var "counter") (Soma (Var "counter") (Num 1)))

-- Exemplo que usa While, Dupla Atribuição e Loop
programaExemplo :: C
programaExemplo = Seq
  (Loop (Num 5) (Seq 
    (DAtrrib (Var "a") (Var "b") (Soma (Var "a") (Num 1)) (Soma (Var "b") (Num 2)))
    (While (Leq (Var "a") (Num 10)) 
      (Seq 
        (Atrib (Var "a") (Soma (Var "a") (Num 1)))
        (Atrib (Var "b") (Soma (Var "b") (Num 1)))
      )
    )
  ))
  Skip