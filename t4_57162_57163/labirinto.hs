-- T3 PP
-- Guilherme Dias     n57163
-- Francisco Resendes n57162
module Labirinto (
    inicializa,
    jogador,
    chaves,
    terminado,
    move,
    posicaoDeC,
    EstadoJogo(..)
)where

import Data.List
import Data.Char
-- A
data EstadoJogo = EstadoJogo{labirinto:: [String],
                            chavesDoJogador:: [Char],
                            posicaoDoJogador:: (Int,Int)
                            }

-- A1
inicializa :: [String] -> EstadoJogo
inicializa lab = EstadoJogo lab [] (posicaoDeC lab 'S')

-- A2
jogador :: EstadoJogo -> (Int,Int)
jogador = posicaoDoJogador

-- A3
chaves :: EstadoJogo -> String
chaves jogo = foldr (:) [] (chavesDoJogador jogo)

-- A4
terminado :: EstadoJogo -> Bool --A posicao do jogador é igual à do final
terminado jogo = posicaoDoJogador jogo == posicaoDeC (labirinto jogo) 'F'

giveLabirinto :: EstadoJogo -> [String]
giveLabirinto = labirinto

-- A5
instance Show EstadoJogo where
    show (EstadoJogo lab chaves posJog) = printLabirinto(colocaJogador lab posJog) ++ "chaves: " ++ chaves


-- FUNÇÕES AUXILIARES A
printLabirinto :: [[Char]] -> [Char]
printLabirinto [] = []
printLabirinto  (x:xs) = x ++ "\n" ++ printLabirinto xs

colocaJogador :: [[Char]] -> (Int, Int) -> [[Char]]
colocaJogador lab posJog = replace lab posJog (0,0) 'P'


--B
-- move o jogador nas direções dadas
move :: EstadoJogo -> [Char] -> EstadoJogo
move = foldl (\lab comando -> posicaoFinal (mexe lab comando) comando)

--FUNCOES AUXILIARES B

--move o jogador uma unidade
mudaPos :: (Int, Int) -> Char -> (Int, Int)
mudaPos (y,ys) c
            |c == 'u' = (y-1,ys)
            |c == 'r' = (y,ys+1)
            |c == 'd' = (y+1,ys)
            |otherwise = (y,ys-1)

--Verifica se o caracter recebido é uma chave
isKey :: Char -> Bool
isKey x = x == 'a' || x == 'b' || x == 'c'

--Verifica se o caracter recebido é uma porta
isDoor :: Char -> Bool
isDoor x = x == 'A' || x == 'B' || x == 'C'

--Recebendo uma direcao 'u','d','r' ou 'l' vai uma posição nessa direcão
mexe :: EstadoJogo -> Char -> EstadoJogo
mexe lab c
        | c == 'u'  = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'u')
        | c == 'd'  = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'd')
        | c == 'r'  = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'r')
        | otherwise = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'l')

-- Dada uma posição de uma lista de listas retorna o elemento que está nessa posicao
procuraPosicao :: (Int, Int) -> [[Char]] -> Char
procuraPosicao (x,y) lab = (lab!!x)!!y

--Vê qual o caracter para onde o jogador quer ir e caso o jogador mova para essa posição faz as acoes necessárias
posicaoFinal :: EstadoJogo -> Char -> EstadoJogo
posicaoFinal jogo x
            | isDoor (procuraPosicao pos lab) = vaiParaPorta  jogo x
            | isKey (procuraPosicao pos lab)  = obtemChave    jogo
            | procuraPosicao pos lab == '@'   = vaiParaPortal jogo
            | procuraPosicao pos lab == '*'   = anteriorPos   jogo x
            | otherwise = jogo
            where
            lab = labirinto jogo
            pos = jogador jogo

-- volta atras para a posicao onde estava antes de receber a direcao 'u','d','r' ou 'l'
anteriorPos :: EstadoJogo -> Char -> EstadoJogo                                     --Não entendi muito bem a função desta função
anteriorPos lab x   | x == 'u'  = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'd') --a função 'posicaoFinal' atualiza automaticamente a posição do jogador?
                    | x == 'd'  = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'u')
                    | x == 'r'  = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'l')
                    | otherwise = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 'r')

---- Vai devolver uma lista de listas já com o elemento em (y,ys) substituido pelo parametro c
replace :: [[Char]] -> (Int, Int) -> (Int, Int) -> Char -> [[Char]]
replace [] _ _ _ = []
replace x (y,ys) (z,zs) c
                    |z /= y = head x : replace (tail x) (y,ys) (z+1,zs) c
                    |otherwise = rplI (head x) (y,ys) (z,zs) c : replace (tail x) (y,ys) (z+1,zs) c

--Vai na lista recebida 
rplI :: [Char] -> (Int, Int) -> (Int, Int) ->  Char -> [Char]
rplI [] _ _ _ =  []
rplI (x:xs) (y,ys) (z,zs) c
                        |(y,ys) == (z,zs) =  c : rplI xs (y,ys) (z,zs + 1) c
                        |otherwise = x : rplI xs (y,ys) (z,zs + 1) c

--A posicao para onde o jogador vai é obtida usando a funcao posicaoDeC de um labirinto já sem o @ onde o jogador está. Assim a funcao
--posicaoDeC retorna a posicao do unico @ que existe que é para onde o jogador tem de ir. Este labirinto em que um dos @ foi trocado
-- por ' ' não é usado na criação do EstadoJogo atualizado 
-- Devolve jogador já no outro portal
vaiParaPortal :: EstadoJogo -> EstadoJogo
vaiParaPortal lab = EstadoJogo (labirinto lab) (chaves lab) (posicaoDeC (replace (labirinto lab) (jogador lab) (0,0) ' ') '@')

--Substitui a chave onde o jogador está por ' '
substituiChave :: EstadoJogo -> EstadoJogo
substituiChave lab = EstadoJogo (replace (labirinto lab) (jogador lab) (0,0) ' ')  (chaves lab) (jogador lab)

--Devolve EstadoJogo adicionando a chave que estava na posicao do jogador à lista de chaves (pondo também a lista de chaves por ordem)
obtemChave :: EstadoJogo -> EstadoJogo
obtemChave lab = EstadoJogo (labirinto (substituiChave lab))  (sort(chaves lab ++ (procuraPosicao (jogador lab) (labirinto lab) : ""))) (jogador lab)

--Verifica se o jogador tem a chave correspondente a porta onde quer ir
temChave :: [Char] -> Char -> Int
temChave [] c = 0 --Explica-me esta func, pls não consegui perceber como funciona
temChave [x] c |x == toLower c = 1
               |otherwise = 0
temChave (x:xs) c |x == toLower c = 1
                  |otherwise = temChave xs c

--Substitui a porta onde o jogador está por ' '
substituiPorta :: EstadoJogo -> [[Char]]
substituiPorta lab = replace (labirinto lab) (jogador lab) (0,0) ' '

--Se tiver a chave para a porta onde quer ir vai para lá senão volta para onde estava
vaiParaPorta  :: EstadoJogo -> Char -> EstadoJogo
vaiParaPorta lab c
                | temChave (chaves lab ) (procuraPosicao (jogador lab) (labirinto lab)) == 1 = EstadoJogo (substituiPorta lab) (chaves lab) (jogador lab)
                | otherwise = anteriorPos lab c

--Indica a primeira posicao onde o parametro c aparece
posicaoDeC :: [[Char]] -> Char -> (Int, Int)
posicaoDeC lab c = (index lab c, countStart ( lab !! index lab c) c-1)

-- como a função try não comeca a contagem do zero é necessario decrementar o valor recebido
index :: [[Char]] -> Char -> Int
index x c = try x c - 1

--verifica se existe C na lista
checkStart :: [Char] -> Char -> Int
checkStart [] _ = 0
checkStart (x:xs) c
                | x == c = 1 + checkStart xs c
                | otherwise = checkStart xs c

-- indica o index do C na lista (será o j de (i,j))
countStart :: [Char] -> Char -> Int
countStart [] _ = 0
countStart (x:xs) c
                | x == c = 1 + countStart [] c
                | otherwise = 1 + countStart xs c

-- indica o index da lista que tem o C (será o i de (i,j))
try :: [[Char]] -> Char -> Int
try [] _ = 0
try x  c
    | checkStart (head x) c > 0 = 1 + try[] c
    | otherwise = 1 + try (tail x) c


lab :: [String]
lab = ["*****" ,"*S  *" , "*A* *" ,"*F* *" ,"*****\n"]