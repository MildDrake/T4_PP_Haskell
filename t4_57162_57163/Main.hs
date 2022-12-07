
import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Labirinto
--import System.Random

load :: FilePath -> IO ()
load ficheiro = do
                handle <- openFile ficheiro ReadMode -- 1. abrir ficheiro
                conteudo <- hGetContents handle -- 2. ler ficheiro todo
                putStrLn conteudo -- 3. escrever conteudo no stdout
                hClose handle -- 4. fechar ficheiro
save :: FilePath -> String -> IO ()
save ficheiro txt = do
                writeFile ficheiro txt
                handle <- openFile ficheiro ReadWriteMode -- 1. abrir ficheiro
                conteudo <- hGetContents handle -- 2. ler ficheiro todo
                putStrLn conteudo -- 3. escrever conteudo no stdout
query :: String -> IO ()
query jogo = do
  qry <- getLine
  if qry == "exit" then do 
    putStr ""
  else if take 4 qry == "load" then do 
    load (drop 5 qry)
    query jogo
  else if take 4 qry == "save" then do
    save (drop 5 qry) jogo
    query jogo
  else if take 4 qry == "move" then do
    let novoJogo = move jogo (drop 5 qry)
    putStrLn (jogador novoJogo)
    putStrLn (chaves novoJogo)
    putStrLn (labirinto novoJogo) --verificar se necessita de funcao auxiliar para imprimir corretamente no stdout
    query novoJogo
  else do 
    putStr ""
main = do  
    args <- getArgs --Argumentos fornecidos quando excuta Main.hs 

    if null args then do
        handle <- openFile (head args) ReadMode 
        conteudo <- hGetContents handle 
        putStrLn conteudo
        -- usar lines conteudo para conver para [String] e depois iterar com funcao auxiliar para criar um EstadoJogo?
        hClose handle

        query "jogo" --inicia ciclo infinito ate ser escrito exit





    else if length args == 1 then do
      exists <- doesFileExist (head args)
      if not exists then  putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\nMain.hs -t -Corre testes"
      else do
        handle <- openFile (head args) ReadMode -- 1. abrir ficheiro
        conteudo <- hGetContents handle -- 2. ler ficheiro todo
        putStrLn conteudo -- 3. escrever conteudo no stdout
        hClose handle -- 4. fechar ficheiro
    else putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\nMain.hs -t -Corre testes"






 


