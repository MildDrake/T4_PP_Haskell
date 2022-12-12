-- T4 PP
-- Guilherme Dias    n57163
-- Francisco Resendes n57162

module TestaMain (
    prop_move_lab_length,
    prop_move_lab_bounds,
    prop_move_lab_keys,
    prop_move_lab_door,
    prop_move_lab_startnfinish_pos,
    prop_move_lab_pos_not_on_wall,
    prop_move_lab_portal,
    prop_move_lab_wall,
    prop_move_lab_space
)where

import Data.Char
import Labirinto
import Test.QuickCheck

-- Conta o número de caracteres iguais ao caracter fornecido numa determinada string
contaChar :: String -> Char -> Int
contaChar [] _ = 0
contaChar (x:xs) caracter | x == caracter = 1 + contaChar xs caracter
                          | otherwise = 0 + contaChar xs caracter

-- Conta o número de portas numa determinada string                          
contaPortas :: String -> Int
contaPortas lab = contaChar lab 'A' + contaChar lab 'B' + contaChar lab 'C'

-- Verifica qual o caractere na posição fornecida
vePosNoLab :: [String] -> (Int, Int) -> Char
vePosNoLab lab pos = (lab!!fst pos)!!snd pos

-- Devolve um gerador de movimentos válidos
movesValidos :: Gen String
movesValidos = listOf1 (elements "rdul") --justificar o limite inferior

--Decide a partir do input se o jogo tem ou não portais
talvezPortal ::  Int -> [Char]
talvezPortal n | n == 1 = "@@"
               | otherwise = ""

-- Cria uma string "parede" com n caractéres            
criaParede :: Int -> String
criaParede 0 = []
criaParede n = "*" ++ criaParede (n-1)

-- Cria um labirintoa partir de uma string. O 2º parâmtero deve ser sempre = 0
deStringParaLab :: String -> Int -> Int -> String
deStringParaLab [] _ _ = []
deStringParaLab (x:xs) n j  | n == 0 =  criaParede (j+2) ++ "\n" ++ deStringParaLab (x:xs) (n+1) j --parede inicial
                            | n `mod` j == 1 = "*" ++ (x : "") ++ deStringParaLab xs (n+1) j       --parede do lado esquedo
                            | n `mod` j == 0 = (x : "*") ++ "\n" ++ deStringParaLab xs (n+1) j     --parede do lado direito
                            | n == (j+1) = (x : "\n") ++ criaParede (j+2)             --talvez por condição adicional na 2a guarda para o ciclo chegar aqui
                            | otherwise = (x : "") ++ deStringParaLab xs (n+1) j

-- Testa se a length to labirinto não se altera depois de um move
prop_move_lab_length :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_length jogo cmd = length (labirinto jogo) == length (labirinto (move jogo (paraString cmd))) &&
                               length (head (labirinto jogo)) == length (head  (labirinto (move jogo (paraString cmd))))

-- Testa se um jogador não sai do labirinto depois de um move
prop_move_lab_bounds :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_bounds jogo cmd = fst (jogador (move jogo (paraString cmd))) >= 0 && fst (jogador (move jogo (paraString cmd))) < length (labirinto jogo) &&
                                snd (jogador (move jogo (paraString cmd))) >= 0 && snd (jogador (move jogo (paraString cmd))) < length (head (labirinto jogo))

-- Testa se o número de chaves não diminui depois de um move
prop_move_lab_keys :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_keys jogo cmd = length (chaves jogo) <= length (chaves (move jogo (paraString cmd)))

-- Testa se o número de portas não aumenta depois de um move
prop_move_lab_door :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_door jogo cmd = contaPortas (unlines (labirinto jogo)) >= contaPortas (unlines (labirinto (move jogo (paraString cmd))))


prop_move_lab_pos_not_on_wall :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_pos_not_on_wall jogo cmd = vePosNoLab (labirinto (move jogo (paraString cmd))) (jogador (move jogo (paraString cmd))) /= '*'

--Testa se a casa de partida e de chegada não alteram de posição depois de um move
prop_move_lab_startnfinish_pos :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_startnfinish_pos jogo cmd = posicaoDeC (labirinto jogo) 'S' == posicaoDeC (labirinto (move jogo (paraString cmd))) 'S' &&
                                        posicaoDeC (labirinto jogo) 'F' == posicaoDeC (labirinto (move jogo (paraString cmd))) 'F'

-- Testa se o número de portais não se altera depois de um move
prop_move_lab_portal :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_portal jogo cmd =  contaChar (unlines (labirinto jogo)) '@' == contaChar (unlines (labirinto (move jogo (paraString cmd)))) '@'

-- Testa se o número de paredes não se altera depois de um move
prop_move_lab_wall :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_wall jogo cmd =  contaChar (unlines (labirinto jogo)) '*' == contaChar (unlines (labirinto (move jogo (paraString cmd)))) '*'

-- Testa se o número de espaços não diminui depois de um move
prop_move_lab_space :: EstadoJogo -> Movimentos -> Bool
prop_move_lab_space jogo cmd =  contaChar (unlines (labirinto jogo)) ' ' <= contaChar (unlines (labirinto (move jogo (paraString cmd)))) ' '

instance Arbitrary EstadoJogo where
    arbitrary = do
       i <- (arbitrary :: Gen Int) `suchThat`(> 1) --justificar limite (por causa de 2x@ + SF = 4 espaços livres necessários e por isso i * j = 4 logo minimo valor para i e j tem que ser 2)
       j <- (arbitrary :: Gen Int) `suchThat`(> 1) --justificar limite (por causa de 2x@ + SF = 4 espaços livres necessários e por isso i * j = 4 logo minimo valor para i e j tem que ser 2)
       portal <- choose (0,1)
       let caracteresExtra | portal == 0 = 2
                           | otherwise = 4
       str <- vectorOf (i*j-caracteresExtra) (elements "ABCabc* ")
       randomLab <- shuffle ("SF"++str++talvezPortal portal)
       let validoLab = lines (deStringParaLab randomLab 0 j)
       return $ inicializa (validoLab ++[criaParede (j +2)])

newtype Movimentos = Movimentos String deriving (Show)

paraString :: Movimentos -> String
paraString (Movimentos str) = str

instance Arbitrary Movimentos where
    arbitrary = do
        Movimentos <$> movesValidos
      --  a <- movesValidos
      --  return (Movimentos a)




