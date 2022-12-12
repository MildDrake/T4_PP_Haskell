-- T4 PP
-- Guilherme Dias    n57163
-- Francisco Resendes n57162

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Labirinto
import TestaMain
import Data.Char
import Test.QuickCheck

--função auxiliar que converte uma string num tuplo
stringToTuple :: [Char] -> (Int, Int)
stringToTuple str = getMeTheTupleOf $ init $ tail str --remover a cabeça e o último elemento

--Devolve um tuplo com dois numeros dados numa string a partir de um input
--deste género:  xx,xx
getMeTheTupleOf :: [Char] -> (Int,Int)
getMeTheTupleOf str = (getBigNum [digitToInt x | x <- a] 1 , getBigNum [digitToInt x | x <-b] 1)
                      where
                        i = indexOf ',' str
                        a = take i str
                        b = drop (i+1) str

--gets a list of ints and turns it into an int [5,4,1,8] -> 5418
getBigNum :: [Int] -> Int -> Int
getBigNum [] _ = 0
getBigNum xs i = last xs * i + getBigNum (init xs) (i*10)

--Devolve o Index de um primeiro caracter numa determinada lista
indexOf :: Char -> String -> Int
indexOf c str = snd $ foldr ehChar (length str-1, -1) str
              where
                ehChar x (i, f)
                  |c == x = (i-1, i)
                  |otherwise = (i-1, f)

--função auxiliar que converte um tuplo para string
tupleToString :: (Int,Int)-> [Char]
tupleToString t = "(" ++ (intToDigit (fst t):",") ++ (intToDigit (snd t) : "") ++ ")"

--recebe o nome do ficheiro e um jogo. Guarda o progresso do jogo no ficheiro
save :: FilePath -> Labirinto.EstadoJogo -> IO ()
save ficheiro jogo = do
                print jogo
                writeFile ficheiro (tupleToString (jogador jogo) ++ "\n")
                appendFile ficheiro (chaves jogo ++ "\n")
                appendFile ficheiro (unlines (labirinto jogo))

--recebe um path para o ficheiro a ser retirada a informação
--cria um estadoJogo com a informação e mostra o jogo e lança
--a funçao query deste novo jogo
load :: FilePath -> IO ()
load ficheiro = do
                handle <- openFile ficheiro ReadMode
                jogador <- hGetLine handle
                chaves <- hGetLine handle
                conteudo <- hGetContents handle
                let jogo = EstadoJogo (lines conteudo) chaves (stringToTuple jogador)
                print jogo
                hClose handle
                query jogo

--trata dos inputs recebidos de forma adequada
query :: Labirinto.EstadoJogo -> IO ()
query jogo = do
  qry <- getLine
  if qry == "exit" || qry == "" then do
    putStr ""
  else if take 4 qry == "load" then do
    load (drop 5 qry)
  else if take 4 qry == "save" then do
    save (drop 5 qry) jogo
    query jogo
  else if take 4 qry == "move" then do
    let novoJogo = move jogo (drop 5 qry)
    print novoJogo
    query novoJogo
  else do
    query jogo

--inicia o programa ou informa como inciá-lo corretamente
main = do
    args <- getArgs --Argumentos fornecidos quando excuta Main.hs 
    if null args then do
      load "default.map"
    else if length args == 1 && head args == "-t" then do
      quickCheck prop_move_lab_length
      quickCheck prop_move_lab_bounds
      quickCheck prop_move_lab_keys
      quickCheck prop_move_lab_door
      quickCheck prop_move_lab_pos_not_on_wall
      quickCheck prop_move_lab_startnfinish_pos
      quickCheck prop_move_lab_portal
      quickCheck prop_move_lab_wall
      quickCheck prop_move_lab_space
    else if length args == 1 then do
      exists <- doesFileExist (head args)
      if not exists then
        putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\n./Main.hs -t -Corre testes"
      else do
        load $ head args -- load ao argumento fornecido
    else putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\n./Main.hs -t -Corre testes"
