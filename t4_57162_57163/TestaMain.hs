module TestaMain (
    prop_move_lab_length,
    prop_move_lab_bounds,
    prop_move_lab_keys,
    prop_move_lab_door,
    prop_move_lab_pos_not_on_wall,
    prop_move_lab_portal,
    prop_move_lab_wall,
    prop_move_lab_space
)where

import Data.Char
import Labirinto
import Test.QuickCheck


contaChar :: String -> Char -> Int
contaChar [] caracter = 0
contaChar (x:xs) caracter | x == caracter = 1 + contaChar xs caracter
                          | otherwise = 0 + contaChar xs caracter

contaPortas :: String -> Int
contaPortas lab = contaChar lab 'A' + contaChar lab 'B' + contaChar lab 'C'

vePosNoLab :: [String] -> (Int, Int) -> Char
vePosNoLab lab pos = (lab!!fst pos)!!snd pos

movesValidos :: Gen String
movesValidos = listOf1 (elements "rdul") --justificar o limite inferior

talvezPortal ::  Int -> [Char]
talvezPortal n | n == 1 = "@@"
               | otherwise = ""

criaParede :: Int -> String
criaParede 0 = []
criaParede n = "*" ++ criaParede (n-1)
             
--deStringParaLab [] n j = [] 
--deStringParaLab (x:xs) n j | n `mod` j == 0 = x :"" ++"\n" ++ deStringParaLab xs (n+1) j
--                           | otherwise = x : deStringParaLab xs (n+1) j
deStringParaLab :: String -> Int -> Int -> String
deStringParaLab [] n j = []
deStringParaLab (x:xs) n j  | n == 0 =  criaParede (j+2) ++ "\n" ++ deStringParaLab (x:xs) (n+1) j
                            | n `mod` j == 1 = "*" ++ (x : "") ++ deStringParaLab xs (n+1) j
                            | n `mod` j == 0 = (x : "*") ++ "\n" ++ deStringParaLab xs (n+1) j
                            | n == (j+1) = (x : "\n") ++ criaParede (j+2)
                            | otherwise = (x : "") ++ deStringParaLab xs (n+1) j


prop_move_lab_length :: EstadoJogo -> [Char] -> Bool
prop_move_lab_length jogo cmd = length (labirinto jogo) == length (labirinto (move jogo cmd)) &&
                               length (head (labirinto jogo)) == length (head  (labirinto (move jogo cmd)))

prop_move_lab_bounds :: EstadoJogo -> [Char] -> Bool
prop_move_lab_bounds jogo cmd = fst (jogador (move jogo cmd)) >= 0 && fst (jogador (move jogo cmd)) < length (labirinto jogo) &&
                                snd (jogador (move jogo cmd)) >= 0 && snd (jogador (move jogo cmd)) < length (head (labirinto jogo))

prop_move_lab_keys :: EstadoJogo -> [Char] -> Bool
prop_move_lab_keys jogo cmd = length (chaves jogo) <= length (chaves (move jogo cmd))

prop_move_lab_door :: EstadoJogo -> [Char] -> Bool
prop_move_lab_door jogo cmd = contaPortas (unlines (labirinto jogo)) >= contaPortas (unlines (labirinto (move jogo cmd)))

prop_move_lab_pos_not_on_wall :: EstadoJogo -> [Char] -> Bool
prop_move_lab_pos_not_on_wall jogo cmd = vePosNoLab (labirinto (move jogo cmd)) (jogador (move jogo cmd)) /= '*'

prop_move_lab_portal :: EstadoJogo -> [Char] -> Bool
prop_move_lab_portal jogo cmd =  contaChar (unlines (labirinto jogo)) '@' == contaChar (unlines (labirinto (move jogo cmd))) '@'

prop_move_lab_wall :: EstadoJogo -> [Char] -> Bool
prop_move_lab_wall jogo cmd =  contaChar (unlines (labirinto jogo)) '*' == contaChar (unlines (labirinto (move jogo cmd))) '*'

prop_move_lab_space :: EstadoJogo -> [Char] -> Bool
prop_move_lab_space jogo cmd =  contaChar (unlines (labirinto jogo)) ' ' <= contaChar (unlines (labirinto (move jogo cmd))) ' '

instance Arbitrary EstadoJogo where
    arbitrary = do
       i <- (arbitrary :: Gen Int) `suchThat`(> 1) --justificar limite (por causa de 2x@ + SF = 4 espaços livres necessários e por isso i * j = 4 logo minimo valor para i e j tem que ser 2)
       j <- (arbitrary :: Gen Int) `suchThat`(> 1) --justificar limite (por causa de 2x@ + SF = 4 espaços livres necessários e por isso i * j = 4 logo minimo valor para i e j tem que ser 2)
       portal <- choose (0,1)
       let caracteresExtra | portal == 0 = 2
                           | otherwise = 4
       str <- vectorOf (i*j-caracteresExtra) (elements "ABCabc* ")
       randomLab <- shuffle ("SF"++str++talvezPortal portal)
       let validoLab = lines ( deStringParaLab randomLab 0 j)
       return (inicializa (validoLab ++[criaParede (j +2)]))

newtype Movimentos = Movimentos String

instance Arbitrary Movimentos where
    arbitrary = do
        a <- movesValidos
        return (Movimentos a)




