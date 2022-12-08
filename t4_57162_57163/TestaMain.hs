{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import Data.Char
--stringToTuple :: [Char] -> (Int, Int)
--stringToTuple str = (digitToInt(str!!1), digitToInt(str!!3))

--tupleToString :: (Int,Int)-> [Char]
--tupleToString t = "(" ++ (intToDigit (fst t):",") ++ (intToDigit (snd t) : "") ++ ")"

import Labirinto
import Test.QuickCheck
contaChar  :: String -> Char -> Int
contaChar [] caracter = 0
contaChar (x:xs) caracter | x == caracter = 1 + contaChar xs caracter
                          | otherwise = 0 + contaChar xs caracter

contaPortas :: String -> Int
contaPortas lab = contaChar lab 'A' + contaChar lab 'B' + contaChar lab 'C'

vePosNoLab :: [String] -> (Int, Int) -> Char
vePosNoLab lab pos = (lab!!fst pos)!!snd pos

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
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (EstadoJogo a b c)

newtype Movimentos = Movimentos String

instance Arbitrary Movimentos where
    arbitrary = do
        a <- arbitrary
        return (Movimentos a)