{- |
Module : Tarefa 1
Description: Estas sao as funçoes da tarefa 1.
Copyright: Nelson Sousa, Rui Ribeiro

Estas funçoes que tem como objetivo gerar um mapa com base num tamanho e numa seed fornecidos pelo utilizador.
                                                                                                                       -}

module Main where
import Bomberman
import System.Environment
import Text.Read
import Data.Maybe
import System.Random


mapa :: 
  Int  -- ^ Inteiro recebido como argumento que representa a dimensao do mapa.
 ->  Int -- ^ Inteiro recebido como argumento que representa a seed escolhida pelo jogador.
  -> [String] -- ^ Mapa produzido , onde cada string representa uma linha.
mapa = mapaBM 


main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"