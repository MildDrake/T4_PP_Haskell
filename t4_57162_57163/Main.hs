-- T4 PP
-- Guilherme Dias    n57163
-- Francisco Resendes n57162

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Labirinto
import Data.Char

--import System.Random

--função auxiliar que converte uma string num tuplo
stringToTuple :: [Char] -> (Int, Int)
stringToTuple str = (digitToInt(str!!1), digitToInt(str!!3))

--função auxiliar que converte um tuplo para string
tupleToString :: (Int,Int)-> [Char]
tupleToString t = "(" ++ (intToDigit (fst t):",") ++ (intToDigit (snd t) : "") ++ ")"

--recebe o nome do ficheiro e um jogo. Guarda o progresso do jogo no ficheiro
save :: FilePath -> Labirinto.EstadoJogo -> IO ()
save ficheiro jogo = do
                putStrLn (tupleToString (jogador jogo))
                putStrLn (chaves jogo)
                putStr (unlines (labirinto jogo))
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
                putStrLn $ show jogo
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
    putStrLn $ show novoJogo
    query novoJogo
  else do
    query jogo

--inicia o programa ou informa como inciá-lo corretamente
main = do
    args <- getArgs --Argumentos fornecidos quando excuta Main.hs 
    if null args then do
      load "default.map"
    else if length args == 1 then do
      exists <- doesFileExist (head args)
      if not exists then
        putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\n./Main.hs -t -Corre testes"
      else do
        load $ head args -- load ao argumento fornecido
    else putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\n./Main.hs -t -Corre testes"
