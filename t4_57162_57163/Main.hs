
import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Labirinto
import Data.Char

--import System.Random
stringToTuple :: [Char] -> (Int, Int)
stringToTuple str = (digitToInt(str!!1), digitToInt(str!!3))

tupleToString :: (Int,Int)-> [Char]
tupleToString t = "(" ++ (intToDigit (fst t):",") ++ (intToDigit (snd t) : "") ++ ")"

save :: FilePath -> Labirinto.EstadoJogo -> IO ()
save ficheiro jogo = do
                putStrLn (tupleToString (jogador jogo))
                putStrLn (chaves jogo)
                putStr (unlines (labirinto jogo))
                writeFile ficheiro (tupleToString (jogador jogo) ++ "\n")
                appendFile ficheiro (chaves jogo ++ "\n")
                appendFile ficheiro (unlines (labirinto jogo))


query :: Labirinto.EstadoJogo -> IO ()
query jogo = do
  qry <- getLine
  if qry == "exit" then do
    putStr ""
  else if take 4 qry == "load" then do
    handle <- openFile (drop 5 qry) ReadMode
    jogador <- hGetLine handle
    chaves <- hGetLine handle
    conteudo <- hGetContents handle
    putStrLn jogador
    putStrLn chaves
    putStrLn conteudo
    hClose handle
    query (EstadoJogo (lines conteudo) chaves (stringToTuple jogador))
  else if take 4 qry == "save" then do
    save (drop 5 qry) jogo
    query jogo
  else if take 4 qry == "move" then do
    let novoJogo = move jogo (drop 5 qry)
    putStrLn (tupleToString (jogador novoJogo))
    putStrLn (chaves novoJogo)
    putStr (unlines (labirinto novoJogo))
    query novoJogo
  else do
    putStr ""
main = do
    args <- getArgs --Argumentos fornecidos quando excuta Main.hs 
    if null args then do
      handle <-  openFile "default.map" ReadMode
      jogador <- hGetLine handle
      chaves <- hGetLine handle
      conteudo <- hGetContents handle
      putStrLn jogador
      putStrLn chaves
      putStrLn conteudo
      hClose handle
      query (EstadoJogo (lines conteudo) chaves (stringToTuple jogador))
    else if length args == 1 then do
      exists <- doesFileExist (head args)
      if not exists then
        putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\nMain.hs -t -Corre testes"
      else do
        handle <-  openFile (head args) ReadMode
        jogador <- hGetLine handle
        chaves <- hGetLine handle
        conteudo <- hGetContents handle
        putStrLn jogador
        putStrLn chaves
        putStrLn conteudo
        hClose handle
        query (EstadoJogo (lines conteudo) chaves (stringToTuple jogador))
    else putStrLn "Como utilizar:\n./Main [ficheiro] ->Carrega um ficheiro para jogar\n./Main.hs ->Carrega o ficheiro default para jogar\nMain.hs -t -Corre testes"









