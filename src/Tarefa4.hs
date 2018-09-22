{- |
Module : Tarefa 4
Description: Estas sao as funçoes da tarefa 4.
Copyright: Nelson Sousa, Rui Ŕibeiro
Estas funçoes têm como objetivo fazer avançar o tempo no jogo.                                           
                                                                                                                       -}


module Main where

import Bomberman
import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe



 
{- | Esta função faz avançar em uma unidade o tempo de jogo , recebendo o mapa, o instantes de tempo que faltam para acabar e produzindo assim o mapa resultante de o tempo avançar uma unidade.

 
= Exemplos 


>>> avanca ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","* 2 2 0 1 10","0 1 1"] 120 =
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","* 2 2 0 1 9","0 1 1"]	

-}
avanca :: 
   [String] -- ^ Mapa Recebido 
      -> Int -- ^ Intantes que faltam para o fim da partida 
         -> [String] -- ^ Mapa resultante
avanca = avancaBM
          



main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
