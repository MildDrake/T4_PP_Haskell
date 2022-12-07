-- T3 PP
-- Guilherme Dias    n57163
-- Francisco Resendes n57162

module Labirinto (
    inicializa,
    jogador,
    chaves,
    terminado, 
    move,
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
jogador jogo = posicaoDoJogador jogo

-- A3
chaves :: EstadoJogo -> String
chaves jogo = foldr (:) [] (chavesDoJogador jogo)

-- A4
terminado :: EstadoJogo -> Bool --A posicao do jogado é igual à do final
terminado jogo = posicaoDoJogador jogo == posicaoDeC (labirinto jogo) 'F'

-- A5
instance Show EstadoJogo where
    show (EstadoJogo a b c) = prt(replaceJ a c (0,0)) ++ "chaves: " ++ b


-- FUNÇÕES AUXILIARES A
prt :: [[Char]] -> [Char]
prt [] = []
prt  (x:xs) = x ++ "\n" ++ prt xs

--Devove uma lista de lista com 'P' na posição do jogador
replaceJ :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
replaceJ [] (y,ys) (z,zs) = []
replaceJ x (y,ys) (z,zs) |z /= y = head x : replaceJ (tail x) (y,ys) (z+1,zs)
                         |otherwise = replaceI (head x) (y,ys) (z,zs)  : replaceJ (tail x) (y,ys) (z+1,zs)

--recebe uma lista e se for a posicão recebida altera o elemento dessa posicão por 'P'
replaceI :: [Char] -> (Int, Int) -> (Int, Int) -> [Char]
replaceI [] (y,ys) (z,zs) =  []
replaceI (x:xs) (y,ys) (z,zs) |(y,ys) == (z,zs) =  'P' : replaceI xs (y,ys) (z,zs + 1)
                              |otherwise = x : replaceI xs (y,ys) (z,zs + 1)


--B
-- move o jogador nas direções dadas
move :: EstadoJogo -> [Char] -> EstadoJogo
move lab x = foldl faz lab x

--FUNCOES AUXILIARES B
--move o jogador na direcão dada
faz :: EstadoJogo -> Char -> EstadoJogo
faz lab x = mexe (proximaPos lab x) x

--move o jogador uma unidade
mudaPos :: (Int, Int) -> Int -> (Int, Int)
mudaPos (y,ys) a |a == 0 = (y-1,ys)
                |a == 1 = (y,ys+1)
                |a == 2 = (y+1,ys)
                |otherwise = (y,ys-1)

--Verifica se o caracter recebido é uma chave
isKey :: Char -> Bool
isKey x = x == 'a' || x == 'b' || x == 'c'

--Verifica se o caracter recebido é uma porta
isDoor :: Char -> Bool
isDoor x = x == 'A' || x == 'B' || x == 'C'

--Recebendo uma direcao 'u','d','r' ou 'l' vai uma posição nessa direcão
proximaPos :: EstadoJogo -> Char -> EstadoJogo
proximaPos lab c | c == 'u' = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 0)
                     | c == 'd' = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 2)
                     | c == 'r' = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 1)
                     | otherwise = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 3)

-- Dada uma posição de uma lista de listas retorna o elemento que está nessa posicao
procuraPosicao :: (Int, Int) -> [[Char]] -> Char
procuraPosicao (x,y) z = (z!!x)!!y

--Vê qual o caracter para onde o jogador quer ir e caso o jogador mova para essa posição faz as acoes necessárias
mexe :: EstadoJogo -> Char -> EstadoJogo
mexe jogo x | isDoor (procuraPosicao pos lab) = vaiParaPorta jogo x
            | isKey (procuraPosicao pos lab) = obtemChave jogo
            | procuraPosicao pos lab == '@'= vaiParaPortal  jogo
            | procuraPosicao pos lab == '*' = anteriorPos jogo x
            | otherwise = jogo
            where
            lab = labirinto jogo
            pos = jogador jogo

-- volta atras para a posicao onde estava antes de receber a direcao 'u','d','r' ou 'l'
anteriorPos :: EstadoJogo -> Char -> EstadoJogo
anteriorPos lab x   | x == 'u' = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 2)
                    | x == 'd' = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 0)
                    | x == 'r' = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 3)
                    | otherwise = EstadoJogo (labirinto lab) (chaves lab)  (mudaPos (jogador lab) 1)

---- Vai devolver uma lista de listas já com o elemento em (y,ys) substituido pelo parametro c
rplJ :: [[Char]] -> (Int, Int) -> (Int, Int) -> Char -> [[Char]]
rplJ [] (y,ys) (z,zs) c = []
rplJ x (y,ys) (z,zs) c |z /= y = head x : rplJ (tail x) (y,ys) (z+1,zs) c
                           |otherwise = rplI (head x) (y,ys) (z,zs) c : rplJ (tail x) (y,ys) (z+1,zs) c

--Vai na lista recebida 
rplI :: [Char] -> (Int, Int) -> (Int, Int) ->  Char -> [Char]
rplI [] (y,ys) (z,zs) c =  []
rplI (x:xs) (y,ys) (z,zs) c |(y,ys) == (z,zs) =  c : rplI xs (y,ys) (z,zs + 1) c
                                |otherwise = x : rplI xs (y,ys) (z,zs + 1) c

--A posicao para onde o jogador vai é obtida usando a funcao posicaoDeC de um labirinto já sem o @ onde o jogador está. Assim a funcao
--posicaoDeC retorna a posicao do unico @ que existe que é para onde o jogador tem de ir. Este labirinto em que um dos @ foi trocado
-- por ' ' não é usado na criação do EstadoJogo atualizado 
-- Devolve jogador já no outro portal
vaiParaPortal :: EstadoJogo -> EstadoJogo
vaiParaPortal lab = EstadoJogo (labirinto lab) (chaves lab) (posicaoDeC (rplJ (labirinto lab) (jogador lab) (0,0) ' ') '@')

--Substitui a chave onde o jogador está por ' '
substituiChave :: EstadoJogo -> EstadoJogo
substituiChave lab = EstadoJogo (rplJ (labirinto lab) (jogador lab) (0,0) ' ')  (chaves lab) (jogador lab)

--Devolve EstadoJogo adicionando a chave que estava na posicao do jogador à lista de chaves (pondo também a lista de chaves por ordem)
obtemChave :: EstadoJogo -> EstadoJogo
obtemChave lab = EstadoJogo (labirinto (substituiChave lab))  (sort(chaves lab ++ (procuraPosicao (jogador lab) (labirinto lab) : ""))) (jogador lab)

--Verifica se o jogador tem a chave correspondente a porta onde quer ir
temChave :: [Char] -> Char -> Int
temChave [] c = 0
temChave [x] c |x == toLower c = 1
               |otherwise = 0
temChave (x:xs) c |x == toLower c = 1
                  |otherwise = temChave xs c

--Substitui a porta onde o jogador está por ' '
substituiPorta :: EstadoJogo -> [[Char]]
substituiPorta lab = rplJ (labirinto lab) (jogador lab) (0,0) ' '

--Se tiver a chave para a porta onde quer ir vai para lá senão volta para onde estava
vaiParaPorta  :: EstadoJogo -> Char -> EstadoJogo
vaiParaPorta lab c | temChave (chaves lab ) (procuraPosicao (jogador lab) (labirinto lab)) == 1 = EstadoJogo (substituiPorta lab) (chaves lab) (jogador lab)
            | otherwise = anteriorPos lab c

--Indica a primeira posicao onde o parametro c aparece
posicaoDeC :: [[Char]] -> Char -> (Int, Int)
posicaoDeC x c = (index x c, countStart (x!! index x c) c-1)

-- como a função try não comeca a contagem do zero é necessario decrementar o valor recebido
index :: [[Char]] -> Char -> Int 
index x c = try x c - 1

--verifica se existe C na lista
checkStart :: [Char] -> Char -> Int  
checkStart [] c = 0
checkStart (x:xs) c | x == c = 1 + checkStart xs c
                   | otherwise = checkStart xs c

-- indica o index do C na lista (será o j de (i,j))
countStart :: [Char] -> Char -> Int  
countStart [] c = 0
countStart (x:xs) c | x == c = 1 + countStart [] c
                    | otherwise = 1 + countStart xs c

-- indica o index da lista que tem o C (será o i de (i,j))
try :: [[Char]] -> Char -> Int  
try [] c = 0
try x  c| checkStart (head x) c > 0 = 1 + try[] c
        | otherwise = 1 + try (tail x) c
