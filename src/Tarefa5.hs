{- |
Module : Tarefa 2
Description: Estas sao as funçoes da tarefa 2.
Copyright: Nelson Sousa, Rui Ŕibeiro
Estas funçoes tem como objetivo executar a parte grafica do jogo.                                           
                                                                                                                       -}

module Main where

import Bomberman
import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import Data.Char

type Estado_ = (Estado,Int,[Picture])



{- | Esta função faz o estado inicial do jogo, que é constituido por um estado (defenido no modulo Bomberman), um inteiro que representa os instantes que faltam para acabar a partida e a lista de imagens utilizadas em jogo.Para isso sao necessarios o tamanho a seed e a lista de imagens a utilizar.


  

-}
estadoInicial :: 
  [Picture] -- ^ lista de imagens a utilizar
     -> String -- ^ tamanho do mapa
         -> String -- ^ seed do mapa
            -> String -- ^ numero de jogadores 
               -> Estado_ -- ^ estado inicial
estadoInicial l t s num = (convrtpaEstado (acresentajogs num t (mapaBM (read t :: Int) (read s :: Int))),4*((read t :: Int)-2)^2,l)

{- | Esta função acresenta jogadores ao mapa gerado inicialmente.
  

-}
acresentajogs :: 
   String -- ^ numero de jogadores 
    -> String -- ^ tamanho do mapa
      -> Mapa -- ^ mapa inicial 
         -> Mapa -- ^ mapa com os jogadores
acresentajogs num _ [] = []
acresentajogs num t m |num == "1" = m ++ ["0 1 1"]
                      |num == "2" = m ++ ["0 1 1","1 "++show((read t :: Int)-2) ++" 1"]
                      |num == "3" = m ++ ["0 1 1","1 "++show((read t :: Int)-2) ++" 1","2 "++show((read t :: Int)-2) ++" "++show((read t :: Int)-2)]
                      |otherwise = m ++ ["0 1 1","1 "++show((read t :: Int)-2) ++" 1","2 "++show((read t :: Int)-2) ++" "++show((read t :: Int)-2),"3 " ++"1 " ++ show((read t :: Int)-2)]
{- | Esta funçao traduz graficamente os jogadores representados no estado.

-}
desenhaJogs :: 
    [Picture] -- ^ icons dos jogadores
        -> Dados -- ^ liste de dados do estado
           -> [Picture] -- ^ resultado de desenhar os jogadores
desenhaJogs [q,jg1,jg2,jg3] [] = []
desenhaJogs [q,jg1,jg2,jg3] (h:t) | j == "0" = (Translate (((read c :: Float))*50) ((-(read l :: Float))*50) (Scale 1 1 q)) : desenhaJogs [q,jg1,jg2,jg3] t
                                  | j == "1" = (Translate (((read c :: Float))*50) ((-(read l :: Float))*50) (Scale 1 1 jg1))  : desenhaJogs [q,jg1,jg2,jg3] t
                                  | j == "2" = (Translate (((read c :: Float))*50) ((-(read l :: Float))*50) (Scale 1 1 jg2))  : desenhaJogs [q,jg1,jg2,jg3] t
                                  | j == "3" = (Translate (((read c :: Float))*50) ((-(read l :: Float))*50) (Scale 1 1 jg3))  : desenhaJogs [q,jg1,jg2,jg3] t
                                  | otherwise = desenhaJogs [q,jg1,jg2,jg3] t
       where (j,c,l,_,_) = retiraInfJog h          

{- | Esta funçao traduz graficamente uma linha do tabuleiro representados no estado.

-}
desenhalinha :: 
   [Picture] -- ^ lista de imagens (tijolo, pedra) 
      -> (Float,Float) -- ^ coordenadas iniciais da linha 
          -> String -- ^ linha que se pretende desenhar  
             -> [Picture] -- ^ linha desenhada
desenhalinha [p,c]  _ []  = []
desenhalinha [p,c] (x,y) (h:t) | h == '#' = (Translate x y (Scale 1 1 p)) : desenhalinha [p,c]  ((x+50),y) t
                               | h == '?' = (Translate x y (Scale 1 1 c)) : desenhalinha [p,c] ((x+50),y) t
                               | h == ' ' = desenhalinha [p,c] ((x+50),y) t

{- | Esta funçao traduz graficamente os power ups contidos no mapa

-}
desenhaPowerUps :: 
   Dados -- ^ dados do jogo
      -> [Picture] -- ^ lista de icons necessarios
         -> [Picture] -- ^ power ups desenhados
desenhaPowerUps [] [b,pwB,pwF] = []
desenhaPowerUps (h:t) [b,pwB,pwF] | head h == '*'  = Translate ((read c :: Float)*50) (-(read l :: Float)*50) b  : desenhaPowerUps t [b,pwB,pwF]
                                  | head h == '!'  = Translate ((read c :: Float)*50) (-(read l :: Float)*50) pwF : desenhaPowerUps t [b,pwB,pwF]
                                  | head h == '+'  = Translate ((read c :: Float)*50) (-(read l :: Float)*50) pwB : desenhaPowerUps t [b,pwB,pwF]
                                  | otherwise = desenhaPowerUps t [b,pwB,pwF]
            where (_,c,l,_,_,_) = retiraInfBomba h


{- | Esta funçao imprime no jogo, uma imagem de vitoria qnd resta apenas um só jogador.

-}          
desenhaGanhou :: 
  Mapa -- ^ Mapa 
    -> Dados -- ^ Dados do mapa
      -> Picture -- ^ icon da vitoria recebido como argumento
        -> [Picture] -- ^ icon da vitora em formato fisco
desenhaGanhou m [] _ = []
desenhaGanhou m l ganhou1 = if ganhou l then [Translate ((fromInteger(toInteger (((tamanhomp m))) * 50)/2)) (((fromInteger(toInteger (-tamanhomp m)) * 50)/2)) (Scale 0.5 0.5 ganhou1)]
                          else []

{- | Esta funçao conta o numero de jogadores vivos.


-}
jgs :: 
   [String] -- ^ Mapa
     -> Int -- ^ numero de jogadores
jgs [] = 0
jgs (h:t) | isDigit (head h) = 1 + jgs t    
          | otherwise = jgs t 

{- | Esta funçao diz se o existe algum vencedor, ou seja se sobra apenas um jogador vivo.

-}
ganhou ::
 [String] -- ^ Mapa
     -> Bool -- ^ resultado da verificaçao
ganhou (h:t) = if jgs (h:t) > 1 then False
               else True

{- | Esta funçao que desenha o jogo

-}
desenhaEstado :: 
   Estado_  -- ^ estado que representa o jogo
      -> Picture -- ^ jogo desenhado
desenhaEstado ((E m d n),t,[p,b,q,jg1,jg2,jg3,c,fh,cf,fv,pwB,pwF,ganhou1]) = if n<=19 then Translate ((fromInteger(toInteger ((-(tamanhomp m))) * 50)/2)+25) (((fromInteger(toInteger (tamanhomp m)) * 50)/2)-25) (Pictures[Pictures (desenhaPowerUps d [b,pwB,pwF]),Pictures (desenhaJogs [q,jg1,jg2,jg3] d),Pictures (desenhaFlames m (bombasQvaoExp (bombas d)) [fh,cf,fv]),Pictures  (aux m (0,0)),Pictures (desenhaGanhou m d ganhou1)])
                                                                             else Scale (1-(((fromInteger (toInteger n) )+15)/100)) (1-(((fromInteger (toInteger n) )+15)/100)) (Translate ((fromInteger(toInteger ((-(tamanhomp m))) * 50)/2)+25) (((fromInteger(toInteger (tamanhomp m)) * 50)/2)-25) (Pictures[Pictures (desenhaPowerUps d [b,pwB,pwF]),Pictures (desenhaJogs [q,jg1,jg2,jg3] d),Pictures (desenhaFlames m (bombasQvaoExp (bombas d)) [fh,cf,fv]),Pictures  (aux m (0,0)),Pictures (desenhaGanhou m d ganhou1)]))
    where aux :: Mapa -> (Float,Float) -> [Picture]        
          aux [] _ = []
          aux  (h:t) (x,y) =  (desenhalinha [p,c] (x,y) h) ++ aux t (0,(y-50))

{- | Esta funçao mostra as bombas que estao prestes a explodir

-}

bombasQvaoExp :: 
     Dados -- ^ dados do jogo 
        -> Dados -- ^ bombas que estao prestes a explodir
bombasQvaoExp [] = []
bombasQvaoExp (x:xs) | t == "1" = x : bombasQvaoExp xs
                     | otherwise = bombasQvaoExp xs
     where (_,_,_,_,_,t) = retiraInfBomba x                                   
 

{- | Esta funçao traduz graficamente a explosao da bomba, desenhando chamas.

-} 
desenhaFlames :: 
   Mapa -- ^ mapa
      -> Dados -- ^ dados do jogo
         -> [Picture] -- ^ lista de icons necessarios 
             -> [Picture] -- ^ explosoes desenhadas
desenhaFlames m [] f = []
desenhaFlames m (x:xs) [fh,cf,fv] = desenhaFlamesCentro (zone(x:xs)) cf ++ desenhaFlamesAux m (zonesUp(zone (x:xs))) fv ++ desenhaFlamesAux m (zonesDown(zone (x:xs))) fv ++ desenhaFlamesAux m (zonesRight(zone (x:xs))) fh ++ desenhaFlamesAux m (zonesLeft(zone (x:xs))) fh

{- | Esta funçao traduz graficamente o centro da explosao, desenhado uma chama.

-}

desenhaFlamesCentro :: 
      [(Int,Int,Int)] -- ^ (coluna,linha,raio da bomba) 
         -> Picture -- ^ icon necessario
            -> [Picture] -- ^ resultado de desenha o centro da explosao
desenhaFlamesCentro [] cf  = []
desenhaFlamesCentro ((c,l,_):t) cf = Translate ((fromInteger (toInteger c) )* 50) (-(fromInteger (toInteger l))*50) cf : desenhaFlamesCentro t cf         


{- | Esta funçao vai desenhar as flames,de todas as bombas, que estao todas na mesma posiçao em relaçao ao centro ,ou seja, por exemplo, desenha todas as flames que estao por cima do centro.

-}
desenhaFlamesAux :: 
   Mapa -- ^ mapa do jogo
     -> [[(Int,Int)]] -- ^ lista das listas (coluna, linha) de flames para desenhar, onde cada lista de flames pertece a uma bomba diferente
        -> Picture -- ^ icon necessario
           -> [Picture] -- ^ resultado de desenhar as flames
desenhaFlamesAux m [] f = []
desenhaFlamesAux m (h:t) f = desenhaFlamesAux_ (tiraCoord m h) f ++ desenhaFlamesAux m t f


{- | Esta funçao vai desenhar as flames,de uma, que estao todas na mesma posiçao em relaçao ao centro ,ou seja, por exemplo, desenha todas as flames que estao por cima do centro.

-}
desenhaFlamesAux_ :: 
   [(Int,Int)] -- ^ lista de flames 
      -> Picture -- ^ icon necessario
         -> [Picture] -- ^ resultado de desenhar as flames 
desenhaFlamesAux_ [] f = []
desenhaFlamesAux_ ((c,l):xs) fh = Translate ((fromInteger (toInteger c) )* 50) (-(fromInteger (toInteger l))*50) fh : desenhaFlamesAux_ xs fh
{- | Função que altera o estado do jogo quando acontece um evento.

-}
reageEvento :: 
  Event -- ^ evento passado extriormente (primir de uma tecla) 
    -> Estado_ -- ^ estado que representa o jogo 
       -> Estado_ -- ^ estado que resulta da execuçao de evento pretendido no anterior estado de jogo
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 0 'U'),t,l)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 0 'D'),t,l)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 0 'L'),t,l)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 0 'R'),t,l)
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 0 'B'),t,l)
reageEvento (EventKey (Char 'w') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 1 'U'),t,l)
reageEvento (EventKey (Char 's') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 1 'D'),t,l)
reageEvento (EventKey (Char 'a') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 1 'L'),t,l)
reageEvento (EventKey (Char 'd') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 1 'R'),t,l)
reageEvento (EventKey (Char 'q') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 1 'B'),t,l)
reageEvento (EventKey (Char 'u') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 2 'U'),t,l)
reageEvento (EventKey (Char 'j') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 2 'D'),t,l)
reageEvento (EventKey (Char 'h') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 2 'L'),t,l)
reageEvento (EventKey (Char 'k') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 2 'R'),t,l)
reageEvento (EventKey (Char 'y') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 2 'B'),t,l)
reageEvento (EventKey (Char 'f') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 3 'U'),t,l)
reageEvento (EventKey (Char 'v') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 3 'D'),t,l)
reageEvento (EventKey (Char 'c') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 3 'L'),t,l)
reageEvento (EventKey (Char 'b') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 3 'R'),t,l)
reageEvento (EventKey (Char 'x') Down _ _) ((E m d n),t,l) = (convrtpaEstado (moveBM (convrtpaString (E m d n)) 3 'B'),t,l)
reageEvento _ e = e

{- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.

-}
reageTempo :: 
   Float -- ^ numero de segundos que passaram desde a ultima vez que esta funçao foi corrida 
      -> Estado_ -- ^ estado que representa o jogo 
         -> Estado_ -- ^ estado que representa o jogo apos avançar n segundos
reageTempo s ((E m d n),t,l) = (convrtpaEstado (avancaBM (convrtpaString (E m d n)) t),(t-1),l)

-- | Frame rate
fr :: Int
fr = 2

-- | Display mode
dm :: [Char] -> Display
dm t = if (read t :: Int)<=19 then InWindow "Bomberman" (50*(read t :: Int), 50*(read t :: Int)) (0,0)
            else InWindow "Bomberman" (round(50*(1-(((read t :: Float) +15)/100)))*(read t :: Int),round(50*(1-(((read t :: Float) +15)/100)))*(read t :: Int)) (0,0)


-- | Função principal que invoca o jogo.
main :: IO ()
main = do putStrLn "Qual o tamanho?"
          t <- getLine
          putStrLn "Qual a seed?"
          s <- getLine
          putStrLn "Quantos jogadores?"
          num <- getLine
          cf <- loadBMP "centerflame.bmp"
          fh <- loadBMP "flameh.bmp"
          fv <- loadBMP "flamev.bmp"
          p <- loadBMP "pedra2d.bmp"
          b <- loadBMP "bomb.bmp"
          q <- loadBMP "jg0.bmp"
          jg1 <- loadBMP "jg1.bmp"
          jg2 <- loadBMP "jg2.bmp"
          jg3 <- loadBMP "jg3.bmp"
          c <- loadBMP "caixa.bmp"
          pwB <- loadBMP "powerupBomba" 
          pwF <- loadBMP "powerupFlame" 
          ganhou1 <- loadBMP "ganhou.bmp"
          play (dm t)             -- display mode
               (greyN 0.5)     -- côr do fundo da janela
               fr              -- frame rate
               (estadoInicial [p,b,q,jg1,jg2,jg3,c,fh,cf,fv,pwB,pwF,ganhou1] t s num)  -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo     -- reage ao passar do tempo