{- |
Module : Tarefa6_li1g049
Description: Estas sao as funçoes da tarefa 6.
Copyright: Nelson Sousa, Rui Ŕibeiro
Estas funçoes tem como objetivo automatizar um jogador, ou seja a criaçao de um bot para jogar bomberman.                                           
                                                                                                                       -}

module Tarefa6_li1g049 where

import Bomberman
{- | Esta funçao é a que faz mover o bot. 

-}
bot :: 
   [String] -- ^ mapa recebido 
      -> Int -- ^ numero de jogador 
         -> Int -- ^ numero de instantes que faltam para acabar o jogo 
           -> Maybe Char -- ^ resposta do bot
bot [] _ _ = Nothing
bot mapa player ticks | player == 0 = middle_0 mapa
                      | player == 1 = middle_1 mapa
                      | player == 3 = middle_3 mapa
                      | player == 2 = middle_2 mapa
                      | otherwise = Nothing
{- | Esta funçao é a que faz mover o bot caso este seja o jogador 0. 

-}
middle_0 :: 
   [String] -- ^ mapa recebido 
      -> Maybe Char -- ^ resposta do bot
middle_0 [] = Nothing
middle_0 m | elem (imAt m 0) (dangerZone m) = desviaBomba m 0
           | elem (imAt (moveBM m 0 'U') 0) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 0 'L') 0) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 0 'D') 0) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 0 'R') 0) (dangerZone m) = Nothing
           | imAt m 0 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2))) && even ((tamanhomp m - 5)`div`2) = Nothing
           | imAt m 0 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2)+1)) && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 0 'U') 0 /= imAt m 0) then Just 'U' else Just 'B'
           | imAt m 0 == middle m && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 0 'L') 0 /= imAt m 0) then Just 'L' else Just 'B'
           | imAt m 0 == middle m = Nothing
           | snd(imAt m 0) < snd (middle m) && fst(imAt m 0) > 1 && (imAt (moveBM m 0 'L') 0 == imAt m 0) =Just 'D'
           | snd(imAt m 0) < snd (middle m) && fst(imAt m 0) > 1 =Just 'L'
           | snd(imAt m 0) < snd (middle m) && (imAt (moveBM m 0 'D') 0 == imAt m 0) = Just 'B'
           | snd(imAt m 0) < snd (middle m) && (imAt (moveBM m 0 'D') 0 /= imAt m 0) = Just 'D'
           | snd(imAt m 0) == snd (middle m) && imAt (moveBM m 0 'R') 0 /= imAt m 0 && fst(imAt m 0) < fst (middle m) = Just 'R'
           | snd(imAt m 0) == snd (middle m) && imAt (moveBM m 0 'R') 0 == imAt m 0 && fst(imAt m 0) < fst (middle m) = Just 'B'
           | snd(imAt m 0) == snd (middle m) && fst(imAt m 0) > fst (middle m) = Just 'L'
           | otherwise = if (imAt (moveBM m 0 'U') 0 == imAt m 0) then Just 'L'
                         else Just 'U'

{- | Esta funçao é a que faz mover o bot caso este seja o jogador 1. 

-}
middle_1 :: 
   [String] -- ^ mapa recebido 
      -> Maybe Char -- ^ resposta do bot
middle_1 [] = Nothing
middle_1 m | elem (imAt m 1) (dangerZone m) = desviaBomba m 1
           | elem (imAt (moveBM m 1 'U') 1) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 1 'L') 1) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 1 'D') 1) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 1 'R') 1) (dangerZone m) = Nothing
           | imAt m 1 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2))) && even ((tamanhomp m - 5)`div`2) = Nothing
           | imAt m 1 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2)+1)) && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 1 'U') 1 /= imAt m 1) then Just 'U' else Just 'B'
           | imAt m 1 == middle m && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 1 'L') 1 /= imAt m 1) then Just 'L' else Just 'B'
           | imAt m 1 == middle m = Nothing
           | snd(imAt m 1) < snd (middle m) && fst(imAt m 1) < (tamanhomp m)-2 && (imAt (moveBM m 1 'R') 1 == imAt m 1) =Just 'D'
           | snd(imAt m 1) < snd (middle m) && fst(imAt m 1) < (tamanhomp m)-2 =Just 'R'
           | snd(imAt m 1) < snd (middle m) && imAt (moveBM m 1 'D') 1 == imAt m 1 = Just 'B'
           | snd(imAt m 1) < snd (middle m) && imAt (moveBM m 1 'D') 1 /= imAt m 1 = Just 'D'
           | snd(imAt m 1) == snd (middle m) && imAt (moveBM m 1 'L') 1 /= imAt m 1 && fst(imAt m 1) > fst (middle m) = Just 'L'
           | snd(imAt m 1) == snd (middle m) && imAt (moveBM m 1 'L') 1 == imAt m 1 && fst(imAt m 1) > fst (middle m) = Just 'B'
           | snd(imAt m 1) == snd (middle m) && fst(imAt m 1) < fst (middle m) = Just 'R'
           | otherwise = if (imAt (moveBM m 1 'U') 1 == imAt m 1) then Just 'L'
                         else Just 'U'           
       

{- | Esta funçao é a que faz mover o bot caso este seja o jogador 3. 

-}       
middle_3 :: 
   [String] -- ^ mapa recebido
      -> Maybe Char -- ^ resposta do bot
middle_3 [] = Nothing
middle_3 m | elem (imAt m 3) (dangerZone m) = desviaBomba m 3
           | elem (imAt (moveBM m 3 'U') 3) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 3 'L') 3) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 3 'D') 3) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 3 'R') 3) (dangerZone m) = Nothing
           | imAt m 3 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2))) && even ((tamanhomp m - 5)`div`2) = Nothing
           | imAt m 3 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2)+1)) && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 3 'U') 3 /= imAt m 3) then Just 'U' else Just 'B'
           | imAt m 3 == middle m && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 3 'L') 3 /= imAt m 3) then Just 'L' else Just 'B'
           | imAt m 3 == middle m = Nothing
           | snd(imAt m 3) > snd (middle m) && fst(imAt m 3) > 1  && (imAt (moveBM m 3 'L') 3 == imAt m 3) =Just 'U'
           | snd(imAt m 3) > snd (middle m) && fst(imAt m 3) > 1 = Just 'L'
           | snd(imAt m 3) > snd (middle m) && (imAt (moveBM m 3 'U') 3 == imAt m 3) = Just 'B'
           | snd(imAt m 3) > snd (middle m) && (imAt (moveBM m 3 'U') 3 /= imAt m 3) = Just 'U'
           | snd(imAt m 3) == snd (middle m) && imAt (moveBM m 3 'R') 3 /= imAt m 3 && fst(imAt m 3) < fst (middle m) = Just 'R'
           | snd(imAt m 3) == snd (middle m) && imAt (moveBM m 3 'R') 3 == imAt m 3 && fst(imAt m 3) < fst (middle m) = Just 'B'
           | snd(imAt m 3) == snd (middle m) && fst(imAt m 3) > fst (middle m) = Just 'L'
           | otherwise = if (imAt (moveBM m 3 'D') 3 == imAt m 3) then Just 'L'
                         else Just 'D'


{- | Esta funçao é a que faz mover o bot caso este seja o jogador 2. 

-}
middle_2 :: 
   [String] -- ^ mapa recebido
     -> Maybe Char -- ^ resposta do bot
middle_2 [] = Nothing
middle_2 m | elem (imAt m 2) (dangerZone m) = desviaBomba m 2
           | elem (imAt (moveBM m 2 'U') 2) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 2 'L') 2) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 2 'D') 2) (dangerZone m) = Nothing
           | elem (imAt (moveBM m 2 'R') 2) (dangerZone m) = Nothing
           | imAt m 2 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2))) && even ((tamanhomp m - 5)`div`2) = Nothing
           | imAt m 2 == (1+((tamanhomp m - 5)`div`2),(2+((tamanhomp m - 5)`div`2)+1)) && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 2 'U') 2 /= imAt m 2) then Just 'U' else Just 'B'
           | imAt m 2 == middle m && even ((tamanhomp m - 5)`div`2) = if (imAt (moveBM m 2 'L') 2 /= imAt m 2) then Just 'L' else Just 'B'
           | imAt m 2 == middle m = Nothing
           | snd(imAt m 2) > snd (middle m) && fst(imAt m 2) < (tamanhomp m) -2 && (imAt (moveBM m 2 'R') 2 == imAt m 2) =Just 'U'
           | snd(imAt m 2) > snd (middle m) && fst(imAt m 2) < (tamanhomp m) -2 = Just 'R'
           | snd(imAt m 2) > snd (middle m) && (imAt (moveBM m 2 'U') 2 == imAt m 2) = Just 'B'
           | snd(imAt m 2) > snd (middle m) && (imAt (moveBM m 2 'U') 2 /= imAt m 2) = Just 'U'
           | snd(imAt m 2) == snd (middle m) && imAt (moveBM m 2 'L') 2 /= imAt m 2 && fst(imAt m 2) > fst (middle m) = Just 'L'
           | snd(imAt m 2) == snd (middle m) && imAt (moveBM m 2 'L') 2 == imAt m 2 && fst(imAt m 2) > fst (middle m) = Just 'B'
           | snd(imAt m 2) == snd (middle m) && fst(imAt m 2) < fst (middle m) = Just 'R'
           | otherwise = if (imAt (moveBM m 2 'D') 2 == imAt m 2) then Just 'L'
                         else Just 'D'           

{- | Esta funçao é a que faz o bot sair do raio de açao das bombas. 

-}
desviaBomba :: 
   [String] -- ^ mapa recebido 
      -> Int -- ^ jogador
         -> Maybe Char -- ^ resposta do bot
desviaBomba [] _ = Nothing
desviaBomba m j | not (elem (imAt (moveBM m j 'U') j) (dangerZone m)) = Just 'U'
                | not (elem (imAt (moveBM m j 'D') j) (dangerZone m)) = Just 'D'
                | not (elem (imAt (moveBM m j 'L') j) (dangerZone m)) = Just 'L'
                | not (elem (imAt (moveBM m j 'R') j) (dangerZone m)) = Just 'R'
                | otherwise = desviaBomba2 m j



{- | Esta funçao é a que faz o bot sair do raio de açao das bombas,quando e preciso mais do que um comando para isso acontecer. 

-} 
                 
desviaBomba2 :: 
    [String] -- ^ mapa recebido 
       -> Int -- ^ jogador
          -> Maybe Char -- ^ resposta do bot
desviaBomba2 [] _ = Nothing
desviaBomba2 m j | not (elem (imAt (moveBM (moveBM m j 'U') j 'U') j) (dangerZone m)) = Just 'U'
                 | not (elem (imAt (moveBM (moveBM m j 'U') j 'D') j) (dangerZone m)) = Just 'U'
                 | not (elem (imAt (moveBM (moveBM m j 'U') j 'L') j) (dangerZone m)) = Just 'U'
                 | not (elem (imAt (moveBM (moveBM m j 'U') j 'R') j) (dangerZone m)) = Just 'U'
                 | not (elem (imAt (moveBM (moveBM m j 'D') j 'U') j) (dangerZone m)) = Just 'D'
                 | not (elem (imAt (moveBM (moveBM m j 'D') j 'D') j) (dangerZone m)) = Just 'D'
                 | not (elem (imAt (moveBM (moveBM m j 'D') j 'L') j) (dangerZone m)) = Just 'D'
                 | not (elem (imAt (moveBM (moveBM m j 'D') j 'R') j) (dangerZone m)) = Just 'D'
                 | not (elem (imAt (moveBM (moveBM m j 'L') j 'U') j) (dangerZone m)) = Just 'L'
                 | not (elem (imAt (moveBM (moveBM m j 'L') j 'D') j) (dangerZone m)) = Just 'L'
                 | not (elem (imAt (moveBM (moveBM m j 'L') j 'L') j) (dangerZone m)) = Just 'L'
                 | not (elem (imAt (moveBM (moveBM m j 'L') j 'R') j) (dangerZone m)) = Just 'L'
                 | not (elem (imAt (moveBM (moveBM m j 'R') j 'U') j) (dangerZone m)) = Just 'R'
                 | not (elem (imAt (moveBM (moveBM m j 'R') j 'D') j) (dangerZone m)) = Just 'R'
                 | not (elem (imAt (moveBM (moveBM m j 'R') j 'L') j) (dangerZone m)) = Just 'R'
                 | not (elem (imAt (moveBM (moveBM m j 'R') j 'R') j) (dangerZone m)) = Just 'R'
                 | otherwise = escolhe (desviaBomba3 [(moveBM m j 'U'),(moveBM m j 'D'),(moveBM m j 'L'),(moveBM m j 'R')] j) 1

{- | Esta funçao é a que complementa a __desviabomba3__ escolhendo um dos comandos uteis para o bot fugir. 

-}
escolhe :: 
   [Maybe Char] -- ^ lista com as opçoes de resposta 
      -> Int -- ^ contador para identificar qual a resposta a dar
         -> Maybe Char -- ^ resposta do bot
escolhe [] n = Nothing
escolhe (h:t) n | h == Nothing = escolhe t (n+1)
                | otherwise = if n == 1 then Just 'U'
                              else if n == 2 then  Just 'D'
                                   else if n == 3 then Just 'L'
                                        else Just 'R'
 

{- | Esta funçao é a que faz o bot sair do raio de açao das bombas,quando e preciso mais do que dois comandos para isso acontecer. 

-}  
desviaBomba3 :: 
   [[String]] -- ^ lista de mapas com os 4 diferente comandos ja executados  
       -> Int -- ^ jogador
           -> [Maybe Char] -- ^ lista de respostas do bot
desviaBomba3 [] j= [Nothing]
desviaBomba3 (m:t) j | not (elem (imAt (moveBM (moveBM m j 'U') j 'U') j) (dangerZone m)) = Just 'U' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'U') j 'D') j) (dangerZone m)) = Just 'U' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'U') j 'L') j) (dangerZone m)) = Just 'U' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'U') j 'R') j) (dangerZone m)) = Just 'U' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'D') j 'U') j) (dangerZone m)) = Just 'D' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'D') j 'L') j) (dangerZone m)) = Just 'D' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'D') j 'R') j) (dangerZone m)) = Just 'D' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'L') j 'U') j) (dangerZone m)) = Just 'L' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'L') j 'L') j) (dangerZone m)) = Just 'L' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'L') j 'R') j) (dangerZone m)) = Just 'L' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'R') j 'U') j) (dangerZone m)) = Just 'R' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'R') j 'L') j) (dangerZone m)) = Just 'R' : desviaBomba3 t j
                     | not (elem (imAt (moveBM (moveBM m j 'R') j 'R') j) (dangerZone m)) = Just 'R' : desviaBomba3 t j              
                     | otherwise = Nothing : desviaBomba3 t j

--}


{- | Esta funçao é a que descobre as coordenadas do meio do mapa, em caso de ter uma pedra no meio, ela da como resposta a coordenada emediatamente a baixo. 

-} 
middle :: 
   [String] -- ^ mapa recebido 
      -> (Int,Int) -- ^ coordenadas do meio
middle [] = (0,0)
middle m  | odd ((tamanhomp m - 5)`div`2) = (2+((tamanhomp m - 5)`div`2),2+(((tamanhomp m) - 5)`div`2)) 
          | otherwise = (2+((tamanhomp m - 5)`div`2),(2+((tamanhomp m) - 5)`div`2)+1) 




{- | Esta funçao é a que descobre as coordenadas do jogador no mapa

-}
imAt :: 
  [String] -- ^ mapa recebido 
     -> Int -- ^ jogador
        -> (Int,Int) -- ^ coordenadas do jogador
imAt mapa player = (read c :: Int,read l :: Int)
       where (_,c,l,_,_) = retiraInfJog (lJog mapa player)

{- | Esta funçao é a que descobre a lista coordenadas que estao sobre o raio de açao de bombas prestes a explodir, ou seja a 3 ticks ou menos de explodir.

-}
dangerZone :: 
   [String] -- ^ mapa recebido 
      -> [(Int,Int)] -- ^ lista de coordenadas
dangerZone m  = centros (zone (dangerbombs(bombas m))) ++ concat (map (tiraCoord m) (zonesUp (zone (dangerbombs(bombas m))))) ++ concat (map (tiraCoord m) (zonesDown (zone (dangerbombs(bombas m))))) ++ concat (map (tiraCoord m) (zonesRight (zone (dangerbombs(bombas m))))) ++ concat (map (tiraCoord m) (zonesLeft (zone (dangerbombs(bombas m)))))


{- | Esta funçao é a que descobre as coordenadas dos centros das bombas que estao prestes a explodir.


-} 
centros :: 
   [(Int,Int,Int)] -- ^ lista de bombas que vao explodir (coluna,linha,raio) 
      -> [(Int,Int)] -- ^ lista de centros (coluna,linha)
centros [] = []
centros ((c,l,_):t) = (c,l) : centros t     





