
{- |
Module : Tarefa 2
Description: Estas sao as funçoes da tarefa 2.
Copyright: Nelson Sousa, Rui Ŕibeiro
Estas funçoes tem como objetivo responder a comandos efectuados pelos jogadores.                                           
                                                                                                                       -}


module Main where
import System.Environment
import Data.Char
import Bomberman 
{- | Esta funçao, e responsavel por executar os comandos que o jogador pretende.Com base noutras funçoes.

=Exemplos

>>>move ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 1 1"] 0 'R' =
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 2 1"]
-}
move :: 
 [String] -- ^ mapa recibido 
   -> Int -- ^ numero do jogador recibido
      -> Char -- ^ comando executado pelo jogador
         -> [String] -- ^ mapa com o resultado da açao executada
move = moveBM
        
                       


main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
