module Bomberman where
import Data.Char
import System.Random


data Estado = E Mapa Dados Int

           deriving Show

type Mapa = [String]
type Dados = [String]

{- | Esta funçao é responsavel verificar se tem tijolo na coordenada para onde o jogador pretende se mover.
=Exemplos

>>>temtijolo ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 1 1"] ("3","1") = True
-}
temtijolo :: 
  [String] -- ^ mapa recibido    
     -> (String,String) -- ^ Cordenadas para onde o jogador se vai mover.
        -> Bool -- ^ Resultado da verifcaçao
temtijolo [] _ = False
temtijolo l1 (c,l) | take 1 (drop (read c :: Int) (unlines (take 1 (drop (read l :: Int) l1)))) == "?" = True
                   | otherwise = False 

{- | Esta funçao acrescenta um power up a um jogador caso necessario, atravez de uma auxiliar.
=Exemplos

>>>acrescentaPrUp_Jog ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 1 1"] 0 "+" = 
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 1 1 +"]
-}
acrescentaPrUp_Jog :: 
  [String] -- ^ mapa recebido
     -> Int -- ^ jogador
        -> String-- ^ power up 
           -> [String] -- ^ resultado de adicinar o power up
acrescentaPrUp_Jog m j p = aux m j [] 
  where aux [] j new  = new
        aux (h:ts) j new  | head h == intToDigit j = aux ts j (new ++ [(acrescentaPrUp_JogAux p h)]) 
                          | otherwise = aux ts j (new ++ [h]) 

{- | Esta e a auxiliar da funçao acrescentaPrUp_Jog.
=Exemplos

>>>acrescentaPrUp_JogAux "0 1 1" "+" = "0 1 1 +"
-}
acrescentaPrUp_JogAux :: 
  String -- ^ linha que representa o jogador no mapa 
    -> String -- ^ power up 
       -> String-- ^ linha resultante de dicionar o power up.
acrescentaPrUp_JogAux [] _ = []                          
acrescentaPrUp_JogAux _ [] = []
acrescentaPrUp_JogAux p lista | p == "+" = jg ++  " " ++ c ++ " " ++ l ++ " " ++ b ++"+" ++ r 
                             | p == "!" = jg ++  " " ++ c ++ " " ++ l ++ " " ++ b ++ "!" ++ r    
           where (jg,c,l,b,r) = retiraInfJog lista

{- | Esta funçao verifica se tem um power up nas coordenadas para as quais o jogador se vai mover, e em caso afirmativo devolve o power up em questao.
=Exemplos

>>>TemPrUp ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 1 1"] ("3","1") = "+"
-}
temPrUp ::
  [String] -- ^ mapa recebido
      -> (String,String) -- ^ coordenadas para onde o jogador se vai mover 
         -> String -- ^ power up, ou caso nao exista lista []
temPrUp [] _ = []
temPrUp (h:t) (c,l) | (cp == c && lp == l) && (s == "+" || s == "!")  = s
                    | otherwise = temPrUp t (c,l)
       where (s,cp,lp,_,_) = retiraInfJog h

{- | Esta funçao retira um power up do mapa , quando por exemplo, este e apanhado pelo jogador
=Exemplos

>>>retiraPrUp ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","+ 3 1","! 3 4","0 1 1"] ("3","1") = 
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 1"]    
-}
retiraPrUp :: 
  [String] -- ^ mapa recebido
    -> (String,String) -- ^ coordenadas onde e necessario retirar o power up 
       -> [String] -- ^ mapa resultante de retirar o power up
retiraPrUp [] _ = []
retiraPrUp (h:t) (c,l) = aux (h:t) (c,l) []
    where aux [] (c,l) new = new
          aux (h:t) (c,l) new | veInf (drop 2 h) == c && veInf (drop (2 + length (veInf (drop 2 h)) + 1 ) h) == l && (head h == '+'|| head h == '!')  = new ++ t
                              | otherwise =  aux t (c,l) (new ++ [h])
          
{- | Esta funçao ordena o mapa , segundo o pretendido. Mapa, power ups de bombas, power ups de flames, bombas e por fim jogadores.
=Exemplos

>>>OrdenaMapa ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","0 1 1"] = 
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","0 1 1"]   
-} 
ordenaMapa :: 
  [String] -- ^ mapa recebido 
     -> [String] -- ^ mapa ordenado
ordenaMapa (h:t) = filter (\h -> head h == '#') (h:t) ++ filter (\h -> head h == '+' ) (h:t) ++ filter (\h -> head h == '!' ) (h:t) ++ ordenabombas(bombas (h:t)) ++ filter (\h -> isDigit (head h)) (h:t)


{- | Esta funçao e responsavel por ordenar as bombas postas pelos jogadores consoante a posiçao que ocupam no mapa.
=Exemplos

>>>Ordenabombas ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 2 2 0 1 10","* 1 2 0 1 8","0 1 1 +"] = 
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 1 2 0 1 10","* 2 2 0 1 8","0 1 1 +"]   
-}
ordenabombas :: 
  [String] -- ^ lista de bombas 
     -> [String] -- ^ lista de bombas ordenadas
ordenabombas [] = []
ordenabombas [h] = [h]
ordenabombas (h:y:t) | (read l :: Int) < (read e :: Int) = [h] ++ ordenabombas (y:t)
                     | (read l :: Int) == (read e :: Int) && (read c :: Int) <= (read d :: Int) = [h] ++ ordenabombas (y:t)
                     | otherwise = [y] ++ ordenabombas (h:t)
    where (_,c,l,_,_,_) = retiraInfBomba h
          (_,d,e,_,_,_) = retiraInfBomba y 

{- | Esta funçao e responsavel por colocar bombas no mapa.
=Exemplos

>>>poeBomba ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 2 2 0 1 10","* 1 2 0 1 8","0 1 1 +"] = 
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 1 2 0 1 10","* 2 2 0 1 8","0 1 1 +"]   
-}
poeBomba  :: 
  [String] -- ^ mapa recebido 
     -> Int -- ^ jogador 
        -> [String] -- ^ resultado de adicionar a bomba ao mapa 
poeBomba m j = if nTemBomba (cordBombas (bombas m)) (c,l) && podePorBomba (bombas m) (length b) j then ordenaMapa (m ++ ["* " ++ c ++ " " ++ l ++ " " ++ jg ++ " " ++ show(1+length r) ++ " "++ "10"])   else m 
     where (jg,c,l,b,r) = retiraInfJog (lJog m j)
 
{- | Esta funçao verifica se um jogador pode por uma bomba, tendo em conta as bombas que ja pos no mapa e os power ups de bomba que tem.
=Exemplos

>>>poeBomba ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","0 1 1 +"] 1 0  = True 
-}        
podePorBomba :: 
  [String] -- ^ mapa recebido 
     -> Int -- ^ numero de power ups de bomba
        -> Int -- ^ jogador
           -> Bool -- ^ resultado da verificaçao
podePorBomba [] _ j = True
podePorBomba m n j | length (bombasjog m j) < 1 + n = True
                   | otherwise = False
                
{- | Esta funçao faz a lista de bombas postas por um determinado jogador.
=Exemplos


>>>bombasjog ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 2 2 0 1 10","* 1 2 0 1 8","0 1 1 +"] 0 =
["* 2 2 0 1 10","* 1 2 0 1 8"]    
-}
bombasjog :: 
  [String] -- ^ mapa recebido
     -> Int-- ^ numero do jogador 
        -> [String]-- ^ lista de bombas do jogador
bombasjog l j = filter (eBombaDoJog j)  l

{- | Esta funçao verifica se determinada bomba posta no mapa e de determindado jogador.
=Exemplos


>>>eBombaDoJog 0 "* 2 2 0 1 10" = True 
-}
eBombaDoJog :: 
  Int -- ^ jogador 
     -> String -- ^ string que representa a bomba
       -> Bool -- ^ resultado da verificaçao   
eBombaDoJog j [] = False
eBombaDoJog j l | show j == jg = True
                | otherwise = False   
            where (_,_,_,jg,_,_) = retiraInfBomba l
            
{- | Esta funçao traduz as informaçoes de uma bomba para um produto cartesiano, de forma a conseguir operar melhor os componentes.
=Exemplos


>>>retiraInfBomba "* 2 2 0 1 10" = ("*","2","2","0","1","10") 
-}
retiraInfBomba :: 
   String -- ^ linha que representa a bomba 
      -> (String,String,String,String,String,String) -- ^ (simbolo,coluna,linha,jogador,raio,tempo)
retiraInfBomba [] = ([],[],[],[],[],[])
retiraInfBomba lista = (s,c,l,jg,r,t) 
      where  s = take 1 lista
             c = veInf (drop 2 lista)
             l = veInf (drop (2 + length c + 1 ) lista)
             jg = veInf (drop (2 + length c + 1 + length l + 1) lista)
             r = veInf (drop (2 + length c + 1 + length l + 1 + length jg + 1) lista)
             t = veInf (drop (2 + length c + 1 + length l + 1 + length jg + 1 + length r + 1) lista)

{- | Esta funçao traduz as informaçoes de um jogador para um produto cartesiano, de forma a conseguir operar melhor os componentes.
=Exemplos


>>>retiraInfJog "0 1 1 +!!" = ("0","1","1","+","!!") 
-}
retiraInfJog :: 
    String -> -- ^ linha que representa o jogador 
         (String,String,String,String,String) -- ^ (jogador,coluna,linha,power up de bombas,power up de flames)
retiraInfJog [] = ([],[],[],[],[])
retiraInfJog lista = (j,c,l,b,r)
      where j = take 1 lista 
            c = veInf (drop 2 lista)
            l = veInf (drop (2 + length c + 1 ) lista)
            b = filter (\h -> h == '+') (veInf (drop (2 + length c + 1 + length l + 1) lista))
            r = filter (\h -> h == '!') (veInf (drop (2 + length c + 1 + length l + 1) lista))

{- | Esta funçao retira do mapa a linha que representa um determindado jogador
=Exemplos


>>>lJog ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 2 2 0 1 10","* 1 2 0 1 8","0 1 1 +"] 0 = "0 1 1 +" 
-}
lJog :: 
   [String] -- ^ mapa recebido
      -> Int -- ^ numero do jogador
         -> String -- ^ linha que representa o jogador
lJog [] j = []
lJog (h:t) j | head h == intToDigit j = h
             | otherwise = lJog t j


{- | Esta funçao verifica se existe alguma bomba posta no mapa na posiçao onde o jogador quer por uma bomba.
=Exemplos


>>>nTemBomba [("2","2"),("1","2")] ("3","3") = True 
-}
nTemBomba :: 
   [(String,String)] -- ^ lista com as coordenadas das bombas que estao no mapa 
      -> (String,String) -- ^ cordenadas onde o jogador quer por a bomba
          -> Bool -- ^ resultado da verificaçao
nTemBomba [] cj = True
nTemBomba (h:t) cj | h == cj = False
                   | otherwise = nTemBomba t cj

{- | Esta funçao faz a lista de todas as bombas do mapa.
=Exemplos


>>>bombas ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","+ 3 1","* 2 2 0 1 10","* 1 2 0 1 8","0 1 1 +"] = 
["* 2 2 0 1 10","* 1 2 0 1 8"] 
-}
bombas :: 
   [String] -- ^ mapa recebido 
       -> [String]-- ^ lista de bombas
bombas [] = []
bombas (h:t) = filter (\h -> head h == '*') (h:t)

{- | Esta funçao transforma a lista de bombas na lista com as respetivas coordenadas
=Exemplos


>>>cordBombas ["* 2 2 0 1 10","* 1 2 0 1 8"]  = [("2","2"),("1","2")]
-}
cordBombas :: [String] -> [(String,String)]
cordBombas [] = []
cordBombas (h:t) = ( c , l ) : cordBombas t
   where c = veInf (drop 2 h)
         l = veInf (drop (2 + length c + 1) h)

{- | Esta funçao altera as coordenadas do jogador, movendo-o para a coluna à direita (se possível)

=Exemplos

>>>moveRight ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 1"] =
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 2 1"]    
-}              

moveRight :: 
   [String] -- ^ mapa recebido 
       -> Int  -- ^ jogador
          -> [String] -- ^ mapa com o resultado de mover o jogador
moveRight m j = aux m j [] t
  where aux [] j new t = new
        aux (h:ts) j new t | head h == intToDigit j = aux ts j (new ++ [(alteracoordRight h t)]) t
                           | otherwise = aux ts j (new ++ [h]) t
        t = length (head m)

{- | Esta funçao e a auxiliar da __moveRight__, que altera a linha do jogador.
=Exemplos

>>>movealteracoordRight "0 1 1" 0 = "0 2 1"    
-}
alteracoordRight ::
   String -- ^ linha que representa o jogador 
      -> Int -- ^ tamanho do mapa
         -> String -- ^ linha alterada
alteracoordRight [] t = []
alteracoordRight lista t = jg ++  " " ++ show ((read c :: Int)  + 1) ++ " " ++ l ++ " " ++ b ++ r 
                                                                              
      where (jg,c,l,b,r) = retiraInfJog lista

{- | Esta funçao altera as coordenadas do jogador, movendo-o para a coluna à esquerda (se possível)

=Exemplos

>>>moveLeft ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 2 1"] =
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 1"]    
-}

moveLeft ::
   [String] -- ^ mapa recebido
       -> Int -- ^ jogador
          -> [String] -- ^ mapa com o resultado de mover o jogador
moveLeft m j = aux m j [] t
  where aux [] j new t = new
        aux (h:ts) j new t | head h == intToDigit j = aux ts j (new ++ [(alteracoordLeft h t)]) t
                           | otherwise = aux ts j (new ++ [h]) t
        t = length (head m)

{- | Esta funçao e a auxiliar da __moveLeft__, que altera a linha do jogador.
=Exemplos

>>>movealteracoordLeft "0 2 1" 0 = "0 1 1"    
-}
alteracoordLeft :: 
  String -- ^ linha que representa o jogador 
     -> Int -- ^ tamanho do mapa
        -> String-- ^ linha alterada
alteracoordLeft [] t = []
alteracoordLeft lista t =jg ++  " " ++ show ((read c :: Int) - 1) ++ " " ++ l ++ " " ++ b ++ r 
                                                                       
      where (jg,c,l,b,r) = retiraInfJog lista

{- | Esta funçao altera as coordenadas do jogador, movendo-o para a linha abaixo (se possível)

=Exemplos

>>>moveDown ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 1"] =
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 2"]    
-}

moveDown :: 
   [String] -- ^ mapa recebido 
      -> Int -- ^ jogador
          -> [String] -- ^ mapa com o resultado de mover o jogador
moveDown m j = aux m j [] t
  where aux [] j new t = new
        aux (h:ts) j new t | head h == intToDigit j = aux ts j (new ++ [(alteracoordDown h t)]) t
                           | otherwise = aux ts j (new ++ [h]) t
        t = length (head m)
      
{- | Esta funçao e a auxiliar da __moveDown__, que altera a linha do jogador.
=Exemplos

>>>movealteracoordDown "0 1 1" 0 = "0 1 2"    
-}
alteracoordDown :: 
   String -- ^ linha que representa o jogador 
      -> Int -- ^ tamanho do mapa
         -> String-- ^ linha alterada
alteracoordDown  [] t = []  
alteracoordDown lista t = jg ++  " " ++ c ++ " " ++ show ((read l :: Int) + 1) ++ " " ++ b ++ r  
                                                                         
      where (jg,c,l,b,r) = retiraInfJog lista

{- | Esta funçao altera as coordenadas do jogador, movendo-o para a linha acima (se possível)

=Exemplos

>>>moveUp ["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 2"] =
["#######","#  ?  #","# # # #","#     #","# #?# #","#  ?  #","#######","! 3 4","0 1 1"]    
-}

moveUp :: 
  [String] -- ^ mapa recebido    
     -> Int -- ^ jogador
        -> [String] -- ^ mapa com o resultado de mover o jogador
moveUp m j = aux m j [] t
  where aux [] j new t = new
        aux (h:ts) j new t | head h == intToDigit j = aux ts j (new ++ [(alteracoordUp  h t)]) t
                           | otherwise = aux ts j (new ++ [h]) t
        t = length (head m)

{- | Esta funçao e a auxiliar da __moveUp__, que altera a linha do jogador.
=Exemplos

>>>movealteracoordUp "0 1 2" 0 = "0 1 1"    
-}
alteracoordUp :: 
  String -- ^ linha que representa o jogador
      -> Int -- ^ tamanho do mapa
          -> String -- ^ linha alterada
alteracoordUp [] t = []
alteracoordUp lista t = jg ++  " " ++ c ++ " " ++ show ((read l :: Int) - 1) ++ " " ++ b  ++ r 
                                                                     
      where (jg,c,l,b,r) = retiraInfJog lista
          
{- | Esta funçao retira a informaçao contida nas linha a baixo do mapa, sabendo que estas estao espaçadas entre si.
=Exemplos

>>>VeInf "0 1 2" 0 = "0"    
-}
veInf :: 
   String -- ^ linha com a informaçao   
      -> String -- ^ informaçao retirada
veInf [] = []
veInf (h:t) = aux [] (h:t)
   where aux inf [] = inf
         aux inf (h:t) | h /= ' ' =  h : (aux inf t)
                       | h == ' ' = inf


{- | Esta funçao controi a lista de numeros aleotorios necessarios para o mapa, consoante o seu _tamanho__ e a sua __seed__. 
= Exemplos 


>>> blocosuteis 9 0 = [83,93,63,38,0,87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99]


-}
blocosUteis ::
   Int -- ^ tamanho do mapa 
   -> Int -- ^ seed
     -> [Int] -- ^ lista de lista de numeros aleatorios
blocosUteis t s = take (t*t-(2*t+2*(t-2)+3*4+a^2)) $ randomRs (0,99) (mkStdGen s)
     where a = if t == 5 then 1 else 1 + ((t-5)`div`2)




{- | Esta função traduz a lista de numeros gerados aleatoreamente para os respetivos simbolos. Onde os numeros entre 0 e 1 representam um __'+'__, entre 2 e 3 um __'!'__, entre 4 e 39 um __'?'__ e , para os restantes um __' '__.
 
= Exemplos 


>>> tradLayout [83,93,63,38,0,87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99] = "   ?+  +  ? ?  ? ?  !  ???? "


-}


tradLayout :: 
  [Int] -- ^  lista de numeros aleatorios 
    -> String -- ^ lista de simbolos para o mapa
tradLayout [] = []
tradLayout (h:s) | h <= 1 = '+': tradLayout s
                 | h <= 3 = '!' : tradLayout s
                 | h <= 39 = '?' : tradLayout s
                 | otherwise = ' ' : tradLayout s


{- | Esta função constroi o mapa com os power ups descobertos, conforme o seu __tamanho__ e a __seed__ dada.
 
= Exemplos 


>>> formamapa 9 0 = ["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"]

-}
formamapa :: 
  Int -- ^ tamanho do mapa 
    -> Int -- ^ seed 
       -> [String] -- ^ mapa gerado
formamapa t s = aux t s 1 b
    where aux t s l b | l == 1 || l==t = replicate t '#'  : aux t s (l+1) b
                      | (l == 2 || l==(t-1)) && t == 5 = ("#  " ++  " "  ++ "#") : aux t s (l+1) b
                      | (l == 2 || l==(t-1)) && t /= 5 = ("#  " ++ take (t-6) b  ++ "  #") : aux t s (l+1) (drop (t-6) b)
                      | l == 3 || l==(t-2) = ("# " ++ "#"++ (fazMeiol3 t b) ++ " #") : aux t s (l+1) (drop ((t-5) `div` 2)  b)
                      | l >= 4 && l<=(t-3) && odd l = ("#" ++ take 1 b ++ (fazMeiolImpar t (drop 1 b)) ++ "#")  : aux t s (l+1) (drop (((t-2) `div` 2)+1) b)
                      | l >= 4 && l<=(t-3) && even l = ("#" ++ take (t-2) b ++ "#") : aux t s (l+1) (drop (t-2) b)
                      | l > t = []
          b = tradLayout (blocosUteis t s)


{- | Esta função faz a parte central da 3º linha do mapa, conforme o seu tamanho e a lista dos numeros aletorios ja traduzidos para o layoutdo mapa.
 
= Exemplos 


>>> fazmeiol3 9 "?+"  = "?#+#"

-} 
fazMeiol3 :: 
  Int -- ^ tamanho do mapa 
    -> String -- ^ lista de simbolos para o mapa
       -> String -- ^ parte central da linha
fazMeiol3 t b = aux t n b
         where aux t 0 b = [] 
               aux t n b = take 1 b ++ "#" ++ aux t (n-1) (drop 1 b)   
               n = (t-5) `div` 2

{- | Esta função faz a parte central das linhas impares do mapa, conforme o seu tamanho e a lista dos numeros aletorios ja traduzidos para o layout do mapa.
 
= Exemplos 


>>> fazmeiolImpar 9 "  ?"  = "# # #?"

-}
fazMeiolImpar :: 
  Int  -- ^ tamanho do mapa
    -> String -- ^ lista de simbolos para o mappa
        -> String -- ^ parte central da linha
fazMeiolImpar t b = aux t n b
         where aux t 0 b = [] 
               aux t n b = "#" ++ take 1 b ++ aux t (n-1) (drop 1 b)   
               n = (t-3) `div` 2



{- | Esta função e responsavel por fazer as coordenadas dos power ups de um mapa no seu estado mais bruto, ou seja com os power ups ainda descobertos, sendo que as __colunas__ representam os __x__ e as __linhas__ o __y__
 
= Exemplos 


>>> fazCoord ["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"] = 
  ["+ 5 2","+ 3 3","! 5 5"]

-}
fazCoord :: 
  [String] -- ^ mapa 
    -> [String] -- ^ lista de coordenadas
fazCoord [] = []
fazCoord (h:t) = aux (h:t) 0
      where aux [] _ = []
            aux (h:t) l = fazLCoord h l ++ aux t (l+1)


{- | Esta função e responsavel por fazer as coordenadas dos power ups de uma linha no seu estado mais bruto, ou seja com os power ups ainda descobertos.
 
= Exemplos 


>>> fazLCoord "# #?#+# #" 2 = ["+ 5 2"]

-}
fazLCoord :: 
  String -- ^ linha do mapa 
    -> Int  -- ^ numero da linha
      -> [String] -- ^ lista de coordenadas
fazLCoord (h:t) l = aux (h:t) 0 l
     where aux [] _ _ = []
           aux (h:t) c l | h == '+' = ("+" ++ " " ++ show c ++ " " ++ show l) : (aux t (c+1) l)
                         | h == '!' = ("!" ++ " " ++ show c ++ " " ++ show l) : (aux t (c+1) l)
                         | otherwise = aux t (c+1) l

{- | Esta função vai esconder linha a linha os power ups do mapa, ou seja vai transformar todos os __+__ e __!__ em __?__.
 
= Exemplos 


>>> escondePUp ["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"] = 
 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]

-}
escondePUp :: 
  [String] -- ^ mapa 
    -> [String] -- ^ mapa com os power ups escondidos
escondePUp (h:t) = map escondeLPUp (h:t)
   
{- | Esta função vai esconder os power ups contidos numa linha do mapa, ou seja vai transformar todos os __+__ e __!__duma linha em __?__.
 
= Exemplos 


>>> escondeLPUp ["# #?#+# #"] = ["# #?#?# #"]

-}

escondeLPUp :: 
  String -- ^ linha do mapa  
    -> String -- ^ linha do mapa com os power ups escondidos
escondeLPUp [] = []
escondeLPUp (h:t) | h == '+' = '?' : escondeLPUp t
                  | h == '!' = '?' : escondeLPUp t
                  | otherwise =  h : escondeLPUp t         

{- | Esta função e responsavel por ordenar a lista de cordenadas do mapa, sendo que primeiro estao os power ups de bomb e depois os de flame
 
= Exemplos 


>>> ordCoord ["! 5 5","+ 5 3","+ 4 3"] =["+ 5 3","+ 4 3","! 5 5"]

-}
ordCoord :: 
  [String] -- ^ lista de coordenadas 
    -> [String] -- ^ lista ordenada das coordenadas 
ordCoord [] = []
ordCoord (h:t) = filter (\h ->  head h == '+') (h:t) ++ filter (\h ->  head h == '!') (h:t)




{- | Esta função que produz um mapa, consoante o __tamanho__ e a __seed__ dada.
 
= Exemplos 


>>> mapa 9 0 
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

-}                        
mapaBM :: 
  Int  -- ^ Inteiro recebido como argumento que representa a dimensao do mapa.
 ->  Int -- ^ Inteiro recebido como argumento que representa a seed escolhida pelo jogador.
  -> [String] -- ^ Mapa produzido , onde cada string representa uma linha.
mapaBM t s = escondePUp (formamapa t s) ++ ordCoord (fazCoord (formamapa t s))

{- | Esta função recebe o comando executado pelo jogador e vê se é possível ou não executá-lo. Caso não seja possível o resultado é o mesmo que o recebido

=Exemplos

>>> moveBM ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 2"] 0 'U'
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 1"]

-}
moveBM :: 
 [String] -- ^ mapa recibido 
   -> Int -- ^ numero do jogador recibido
      -> Char -- ^ comando executado pelo jogador
         -> [String] -- ^ mapa com o resultado da açao executada
moveBM [] _ _ =  []
moveBM m j x | x == 'U' && lJog m j /= [] = if not(temParede m (c,l)) then if not(temtijolo m (c,l)) then if temPrUp (moveUp m j) (c,l) == "+" ||temPrUp (moveUp m j) (c,l) == "!" then retiraPrUp (acrescentaPrUp_Jog(moveUp m j) j a) (c,l) else moveUp m j else m else m
             | x == 'D' && lJog m j /= [] = if not(temParede m (c1,l1)) then if not(temtijolo m (c1,l1)) then if temPrUp m (c1,l1) == "+" ||temPrUp (moveDown m j) (c1,l1) == "!" then retiraPrUp (acrescentaPrUp_Jog(moveDown m j) j b) (c1,l1) else moveDown m j else m else m
             | x == 'L' && lJog m j /= [] = if not(temParede m (c2,l2)) then if not(temtijolo m (c2,l2)) then if temPrUp m (c2,l2) == "+" ||temPrUp (moveLeft m j) (c2,l2) =="!" then retiraPrUp (acrescentaPrUp_Jog(moveLeft m j) j e) (c2,l2) else moveLeft m j else m else m
             | x == 'R' && lJog m j /= [] = if not(temParede m (c3,l3)) then if not(temtijolo m (c3,l3)) then if temPrUp m (c3,l3) == "+" ||temPrUp (moveRight m j) (c3,l3) =="!" then retiraPrUp (acrescentaPrUp_Jog(moveRight m j) j d) (c3,l3) else moveRight m j else m else m
             | x == 'B' && lJog m j /= [] = poeBomba m j
             | otherwise = m
   where (_,c,l,_,_) = retiraInfJog (lJog (moveUp m j) j)
         (_,c1,l1,_,_) = retiraInfJog (lJog (moveDown m j) j)
         (_,c2,l2,_,_) = retiraInfJog (lJog (moveLeft m j) j)
         (_,c3,l3,_,_) = retiraInfJog (lJog (moveRight m j) j)
         a = temPrUp m (c,l)
         b = temPrUp m (c1,l1)
         e = temPrUp m (c2,l2)
         d = temPrUp m (c3,l3)


{- | Esta função que produz um mapa, consoante o __tamanho__ e a __seed__ dada.
 
= Exemplos 


>>> mapa 9 0 
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

-}
avancaBM :: [String] -> Int -> [String]
avancaBM e i | i > (tamanhoMapa(convrtpaEstado e)-2)^2 = retiraExplosoes(convrtpaString (boom (passa1inst (convrtpaEstado e))))
             | otherwise = retiraExplosoes(convrtpaString (fazCaracol (boom (passa1inst (convrtpaEstado e)))))        

{- | Esta função transforma um estado do jogo do tipo [String] no novo estado de jogo onde separa o mapa das coordenadas.

= Exemplos 


>>> convrtpaEstado ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"] =
  E ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9
-}
convrtpaEstado :: [String] -> Estado
convrtpaEstado [] = E [] [] 0
convrtpaEstado (h:t) = E (take (length h) (h:t)) (drop (length h) (h:t)) (length h)

{- | Esta função transforma um estado do jogo no mapa com as coordenadas do tipo __[String]__

= Exemplos 


>>> convrtpaString E ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
-}

convrtpaString :: 
 Estado -- ^ estado do jogo recebido 
  -> [String] -- ^ mapa com as coordenadas (numa só string)
convrtpaString (E m d n) = m ++ d

{- | Esta função, dado o estado do jogo dá o tamanho do mapa 

= Exemplos
 
>>> tamanhaMapa  ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9
9
-}

tamanhoMapa :: 
 Estado -- ^ Estado do jogo recebido 
  -> Int -- ^ tamanho do mapa
tamanhoMapa (E m d n) = n        

{- | Esta função executa o efeito caracol no mapa.

= Exemplos
 
>>> fazCaracol ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9 =
  ["#########","##      #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9

-}

fazCaracol :: 
 Estado -- ^ estado do jogo
  -> Estado -- ^ estado do jogo apos ter sido posto uma peça do caracol
fazCaracol (E m d n) = fazCaracolAux (fst(explode_X (E m d n) (c,l) "1"))
      where (c,l) = iniCaracol m ("1","1") 'r' 1

{- | Esta função auxilia a faz caracol, pondo o tijolo na coordenada necessaria. 

= Exemplos
 
>>> fazCaracolAux ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9 =
   ["#########","##      #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5"] 9

-}

fazCaracolAux :: 
 Estado -- ^ estado do jogo
  -> Estado -- ^ estado do jogo apos ter sido colocado uma pedra no tabuleiro
fazCaracolAux (E m d n) = (E (poeParede m (c,l)) d n)  
        where (c,l) = iniCaracol m ("1","1") 'r' 1

{- | Esta função mete uma parede na coluna e linha dados

= Exemplos
 
>>> poeParede ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ("1","1") =
["#########","##      #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]
-}   

poeParede :: 
 Mapa -- ^ mapa recebido
  -> (String,String) -- ^ coordenadas para meter a parede
   -> Mapa -- ^ mapa recebida depois de inserida a parede
poeParede [] _ = []
poeParede m (c,l) = poeParedeAux m (read c :: Int) (read l :: Int) 
       where poeParedeAux (h:t) c 0 = (poeParedeAux2 h c) : t  
             poeParedeAux (h:t) c l = h : (poeParedeAux t c (l-1))
             poeParedeAux [] _ _ = []               
             poeParedeAux2 (x:xs) 0 = "#" ++ xs
             poeParedeAux2 (x:xs) c = x : (poeParedeAux2 xs (c-1))
             poeParedeAux2 [] _ = []

{- | Esta função dá-nos as coordenadas de onde por a proxima pedra do caracol.Sabendo que este e feito por voltas

= Exemplos
 
>>> iniCaracol ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ("1","1") 'r' 1 = ("1","1")

-}

iniCaracol :: 
 Mapa -- ^ mapa recebido
  -> (String,String) -- ^ coordenadas iniciais da volta
   -> Char -- ^ sentido de orientaçao de caracol (r - avança para a direita por exemplo)
    -> Int -- ^ numero da volta
     -> (String,String)
iniCaracol [] (c,l) _ _ = ("","")
iniCaracol m (c,l) d a  | temParede m (c,l) && d == 'r' && (read c :: Int) == (length (head m) - 1-a) = iniCaracol m (c,show ((read l :: Int)+1)) 'd' a
                        | temParede m (c,l) && d == 'r' = iniCaracol m (show((read c :: Int)+1),l) 'r' a 
                        | temParede m (c,l) && d == 'd' && (read l :: Int) == (length (head m) - 1-a) = iniCaracol m (show((read c :: Int)-1),l) 'l' a
                        | temParede m (c,l) && d == 'd' = iniCaracol m (c,show((read l ::Int)+1)) 'd' a
                        | temParede m (c,l) && d == 'l' && c == show a = iniCaracol m (c,show ((read l :: Int)-1)) 'u' a
                        | temParede m (c,l) && d == 'l' = iniCaracol m (show((read c :: Int)-1),l) 'l' a
                        | temParede m (c,l) && d == 'u' && l == show a = iniCaracol m (show (a+1), show (a+1)) 'r' (a+1)
                        | temParede m (c,l) && d == 'u' = iniCaracol m (c,show((read l ::Int)-1)) 'u' a
                        | otherwise = (c,l) 

{- | Esta função diminui 1 instante às bombas que se encontram no mapa

= Exemplos
 
>>> passa1inst E ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 4","0 2 1"] 9
E ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 3","0 2 1"]
-}

passa1inst :: 
 Estado -- ^ Estado do jogo recebido 
  -> Estado -- ^ Estado do jogo no qual as bombas ja avançaram 1 instante
passa1inst (E m d n) = (E m (passaDados d) n)


{- | Esta função filtra as coordenadas do mapa com as mesmas e avança 1 instante a todas as bombas

= Exemplos
 
>>> passaDados ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 4","0 2 1"]
["+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 3","0 2 1"]
-}
passaDados :: 
 [String] -- ^ Mapa com as coordenadas 
  -> [String] -- ^ As coordenadas nas quais a(s) bomba(s) avançaram 1 instante
passaDados l = filter (\l -> head l == '+') l ++ filter (\l -> head l == '!') l ++ diminuitBombas (filter (\l -> head l == '*') l) ++ filter (\l -> isDigit(head l)) l

{- | Esta função pega na lista das bombas e avança cada uma 1 instante 

= Exemplos
 
>>> diminuitBombas ["* 1 2 0 2 5","* 9 2 1 2 3"]
["* 1 2 0 2 4","* 9 2 1 2 2"]

-}

diminuitBombas :: 
 [String] -- ^ lista das bombas
  -> [String] -- ^ lista das bombas que avançaram 1 instante
diminuitBombas l = map diminuitBomba l

{- | Esta função pega numa bomba e avança-a 1 instante

= Exemplos
 
>>> diminuitBomba ["* 1 2 0 2 3"]
["* 1 2 0 2 2"]

-}

diminuitBomba :: 
 String -- ^ lista com a bomba
  -> String -- ^ lista com a bomba que avançou 1 instante
diminuitBomba [] = []
diminuitBomba l | t /= "1" = s ++ " " ++c++ " " ++ l1++" " ++ jg++ " " ++ r ++ " "++ show ((read t :: Int) -1) 
                | otherwise = "explode" ++ " " ++ c ++" " ++ l1 ++" " ++ r 
   where (s,c,l1,jg,r,t) = retiraInfBomba l

{- | Esta função causa uma explosao numa determinada coordenada, devolvendo o resultado e a a informaçao se deve ou nao continuar a explodir

 

-}
explode_X :: 
 Estado -- ^ estado do jogado
  -> (String,String) -- ^ coordenadas para explodir
   -> String -- ^ raio da explosao
    -> (Estado,String) -- ^ (estado com a explosao feita, raio resultante)
explode_X e ([],[]) _ = (e,"0")
explode_X (E m d t) (c,l) r = if temtijolo m (c,l) 
                              then (E (retiraTijolo m (c,l)) d t,"0") 
                              else 
                                   if temParede m (c,l) 
                                   then (E m d t,"0")
                                   else 
                                       if nTemBomba (cordBombas (bombas d)) (c,l) 
                                       then if temJog d (c,l) 
                                            then if temPrUp d (c,l) /= []
                                                 then (E m (retiraPrUp (killJog d (c,l))  (c,l)) t, "0")
                                                 else (E m (killJog d (c,l)) t , show((read r :: Int) - 1)) 
                                            else 
                                                 if temPrUp d (c,l) /= []
                                                 then (E m (retiraPrUp d  (c,l)) t ,"0")
                                                 else (E m d t, show ((read r:: Int) -1))
                                       else if temJog d (c,l) 
                                            then if temPrUp d (c,l) /= [] 
                                                 then (E m (alterabomba (retiraPrUp (killJog d (c,l)) (c,l)) (c,l)) t, "0")
                                                 else (E m (alterabomba (killJog d (c,l)) (c,l)) t , show((read r :: Int) - 1) )
                                            else 
                                                 if temPrUp d (c,l) /= [] 
                                                 then (E m (alterabomba (retiraPrUp d (c,l)) (c,l)) t , "0" )
                                                 else (E m (alterabomba d (c,l)) t, show ((read r:: Int) -1))

{- | Esta função averigua se existe um jogador em determinadas coordenadas

= Exemplos
 
>>> temJog ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 3","0 2 1"] ("2","1")
True

-}

temJog :: 
 Dados -- ^ Lista com os dados 
  -> (String,String) -- ^ Coordenadas para verificar
   -> Bool -- ^ Se existe ou não jogador naquelas coordenadas
temJog [] _ = False
temJog (h:t) (c,l) | (cp == c && lp == l) && isDigit(head h)  = True
                   | otherwise = temJog t (c,l)
       where (s,cp,lp,_,_) = retiraInfJog h

{- | Esta função passa o tempo das bombas para 1 instante

= Exemplos
 
>>> alterabomba ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 3","0 2 1"] ("1","2") ≃
 ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 3 1","0 2 1"] 

-}

alterabomba :: 
 Dados -- ^ Lista com os dados do jogo 
  -> (String,String) -- ^ Coordenadas da bomba q vai ser alterada
   -> Dados -- ^ Lista com os dados com as bombas alteradas
alterabomba [] _ = []
alterabomba (h:t) (c,l) | head h == '*' && c1 == c && l1==l = ("* " ++ c1 ++ " " ++l1 ++ " " ++ jg ++ " " ++ r ++ " " ++ "1") : alterabomba t (c,l)
                        | otherwise =  h : alterabomba t (c,l)
        where (_,c1,l1,jg,r,_) = retiraInfBomba h

{- | Esta função mata o jogador, retirando-o da lista de dados

= Exemplos
 
>>> killJog ["0 1 1"] ("1","1") = []

-}

killJog :: 
 [String] -- ^ dados do mapa 
  -> (String,String) -- ^ coordenadas do jogador
   -> Dados -- ^ dados sem a presença do jogador
killJog [] _ = []
killJog y (c,l) = aux y (c,l) []
    where aux [] (c,l) n = n
          aux (h:t) (c,l) n | veInf (drop 2 h) == c && veInf (drop (2 + length (veInf (drop 2 h)) + 1 ) h) == l && (isDigit (head h))  = killJog (n ++ t) (c,l)
                            | otherwise =  aux t (c,l) (n ++ [h])                                        

{- | Esta função averigua se numa determinada coordenada existe pedra

= Exemplos
 
>>> temParede ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] (2,2)
True
-}

temParede :: 
  Mapa -- ^ mapa recibido    
     -> (String,String) -- ^ Cordenadas para onde o jogador se vai mover.
        -> Bool -- ^ Resultado da verifcaçao
temParede [] _ = False
temParede l1 (c,l) | take 1 (drop (read c :: Int) (unlines (take 1 (drop (read l :: Int) l1)))) == "#" = True
                   | otherwise = False                                

{- | Esta função retira o tijolo do mapa.



-}

retiraTijolo :: 
 Mapa -- ^ mapa recebido
  -> (String,String) -- ^ coordenados onde e necessario retirar o tijolo
   -> Mapa -- ^ mapa sem o respetivo tijolo
retiraTijolo [] _ = []
retiraTijolo m (c,l) = retiraTijoloAux m (read c :: Int) (read l :: Int) 
       where retiraTijoloAux (h:t) c 0 = (retiraTijoloAux2 h c) : t  
             retiraTijoloAux (h:t) c l = h : (retiraTijoloAux t c (l-1))               
             retiraTijoloAux2 (x:xs) 0 = " " ++ xs
             retiraTijoloAux2 (x:xs) c = x : (retiraTijoloAux2 xs (c-1)) 
 
{- | Esta função faz a explosão das bombas

= Exemplos
 
-}

boom :: 
 Estado -- ^ Estado atual do jogo
  -> Estado -- ^ Estado do jogo com as bombas explodidas
boom (E m d n) = explosoesSerie (E m d n) (explosoes d)

{- | Esta função faz todas as explosoes necessarias


-}

explosoesSerie :: 
 Estado  -- ^ estado do jogo
  -> [(String,String,String)] -- ^ bombas para explodir (coluna,linha,raio) 
   -> Estado
explosoesSerie e [] = e
explosoesSerie e ((c,l,r):t) = explosoesSerie (explode e (c,l) r) t

{- | Esta função converte as bombas que estao prestes a explodir no formato (coluna,linha,raio)
  

-}

explosoes :: 
 Dados -- ^ dados do jogo
  -> [(String,String,String)] -- ^ lista das bombas que vao explodir no formato necessario
explosoes [] = []  
explosoes (h:t) | head h == 'e' = (c,l,r) : explosoes t
                | otherwise = explosoes t
            where (_,_,c,l,r,_) = retiraInfBomba h

{- | Esta função retira as bombas que explodiram

= Exemplos
 
>>> retiraExplosoes ["+ 5 3","explode 1 2 2"]
["+ 5 3"]
-}

retiraExplosoes :: 
 [String] -- ^ Lista com bombas que explodiram 
  -> [String] -- ^ Lista sem essas mesmas bombas
retiraExplosoes (h:t) = filter (\h -> head h /= 'e') (h:t)

{- | Esta função explode uma bomba

= Exemplos
 
>>> explode E ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 1","0 2 1"] 9 (1,2) 2
 ["#########","#       #","# #?#?# #","#  ?  ? #","# # # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 1","0 2 1"] 9
-}

explode :: 
 Estado -- ^ Estado atual do jogo
  -> (String,String) -- ^ Coordenadas da bomba
   -> String -- ^ Raio da bomba
    -> Estado -- ^ Estado do jogo depois da explosão
explode e (c,l) r = explodeDir (explodeEsq (explodeCima (explodeBaixo e (c,l) r) (c,l) r) (c,l) r) (c,l) r

{- | Esta função elimina aquilo que está no raio à direita da bomba

= Exemplos
 
>>> explodeDir ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 1","0 2 1"] 9 (1,2) 2

-}

explodeDir :: 
 Estado -- ^ Estado atual do jogo
  -> (String,String) -- ^ Coordenadas da bomba
   -> String -- ^ Raio da bomba
    -> Estado -- ^ Estado do jogo depois da explosão à direita
explodeDir e (c,l) r | r == "0" = e
                     | r/= "0" = explodeDir a (show((read c :: Int)+1),l) b
          where a = fst (explode_X e (show((read c ::  Int)+1),l) r)
                b = snd (explode_X e (show((read c ::  Int)+1),l) r)

{- | Esta função elimina aquilo que está no raio à esquerda da bomba

= Exemplos
 
>>> explodeEsq ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 1","0 2 1"] 9 (1,2) 2

-}

explodeEsq :: 
 Estado -- ^ Estado atual do jogo
  -> (String,String) -- ^ Coordenadas da bomba
   -> String -- ^ Raio da bomba
    -> Estado -- ^ Estado do jogo depois da explosão à esquerda
explodeEsq e (c,l) r | r== "0" = e
                     | r/= "0" = explodeEsq a (show((read c :: Int)-1),l) b
          where a = fst (explode_X e (show((read c ::  Int)-1),l) r)
                b = snd (explode_X e (show((read c ::  Int)-1),l) r)

{- | Esta função elimina aquilo que está no raio acima da bomba

= Exemplos
 
>>> explodeCima ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 1","0 2 1"] 9 (1,2) 2

-}                

explodeCima :: 
 Estado -- ^ Estado atual do jogo
  -> (String,String) -- ^ Coordenadas da bomba
   -> String -- ^ Raio da bomba
    -> Estado -- ^ Estado do jogo depois da explosão para cima
explodeCima e (c,l) r | r== "0" = e
                      | r/= "0" = explodeCima a (c,show((read l :: Int)-1)) b 
          where a = fst (explode_X e (c,show((read l :: Int)-1)) r)
                b = snd (explode_X e (c,show((read l :: Int)-1)) r)

{- | Esta função elimina aquilo que está no raio abaixo da bomba

= Exemplos
 
>>> explodeBaixo ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 1","0 2 1"] 9 (1,2) 2

-}

explodeBaixo :: 
 Estado -- ^ Estado atual do jogo
  -> (String,String) -- ^ Coordenadas da bomba
   -> String -- ^ Raio da bomba
    -> Estado -- ^ Estado do jogo depois da explosão para baixo
explodeBaixo e (c,l) r | r== "0" = e
                       | r/= "0" = explodeBaixo a (c,show((read l :: Int)+1)) b
          where a = fst (explode_X e (c,show((read l :: Int)+1)) r)
                b = snd (explode_X e (c,show((read l :: Int)+1)) r)

{- | Esta função fica apenas com as bombas que tenham um tempo inferior a 4

= Exemplos
 
>>> dangerbombs ["+ 5 2","+ 3 3","! 5 5","* 1 2 0 2 3","* 1 9 0 2 5","0 2 1"]
["* 1 2 0 2 3"]
-}

dangerbombs :: 
 Dados -- ^ Dados do estado de jogo atual
  -> Dados -- ^ Lista das bombas que faltam menos de 4 segundos para explodir
dangerbombs [] = []
dangerbombs (h:ts) | t <= "3" = h : dangerbombs ts
                   | otherwise = dangerbombs ts
    where (_,_,_,_,_,t) = retiraInfBomba h

{- | Esta função produz a lista de listas de zonas que vao explodir, onde cada lista tem as coordenadas de uma so bomba, e neste caso as coordenadas sao todo a cima do centro da bomba.


-}

zonesUp :: 
 [(Int,Int,Int)] -- ^ lista de bombas que vao explodir
  -> [[(Int,Int)]] -- ^ lista das listas das zonas que vao explodir , cada zona esta associada a uma bomba
zonesUp [] = []
zonesUp ((c,l,r):t) = reverse (aux r (c,l)) : zonesUp t                   
        where aux 0 _ = []
              aux r (c,l) = (c,l-r) : aux (r-1) (c,l)

{- |  Esta função produz a lista de listas de zonas que vao explodir, onde cada lista tem as coordenadas de uma so bomba, e neste caso as coordenadas sao todo a baixo do centro da bomba.


-}

zonesDown ::
 [(Int,Int,Int)] -- ^ lista de bombas que vao explodir
  -> [[(Int,Int)]] -- ^ lista das listas das zonas que vao explodir , cada zona esta associada a uma bomba
zonesDown [] = []
zonesDown ((c,l,r):t) =reverse (aux r (c,l)) : zonesDown t                   
           where aux 0 _ = []
                 aux r (c,l) = (c,l+r) : aux (r-1) (c,l)

{- |  Esta função produz a lista de listas de zonas que vao explodir, onde cada lista tem as coordenadas de uma so bomba, e neste caso as coordenadas sao todo a direita do centro da bomba.

-}

zonesRight :: 
 [(Int,Int,Int)] -- ^ lista de bombas que vao explodir
  -> [[(Int,Int)]] -- ^ lista das listas das zonas que vao explodir , cada zona esta associada a uma bomba
zonesRight [] = []
zonesRight ((c,l,r):t) = reverse (aux r (c,l)) : zonesRight t                   
           where aux 0 _ = []
                 aux r (c,l) = (c+r,l) : aux (r-1) (c,l)                 

{- |  Esta função produz a lista de listas de zonas que vao explodir, onde cada lista tem as coordenadas de uma so bomba, e neste caso as coordenadas sao todo a esquerda do centro da bomba.

-}

zonesLeft :: 
 [(Int,Int,Int)] -- ^ lista de bombas que vao explodir
  -> [[(Int,Int)]] -- ^ lista das listas das zonas que vao explodir , cada zona esta associada a uma bomba
zonesLeft [] = []
zonesLeft ((c,l,r):t) = reverse (aux r (c,l)) : zonesLeft t                   
           where aux 0 _ = []
                 aux r (c,l) = (c-r,l) : aux (r-1) (c,l) 

{- | Esta função da a lista de bombas que vao explodir.

-}

zone :: 
 Dados -- ^ dados do jogo
  -> [(Int,Int,Int)] -- ^ lista de bomba  no formato desejado
zone [] = []
zone (h:t) = (read c :: Int ,read l :: Int,read r :: Int) : zone t
    where (_,c,l,_,r,_) = retiraInfBomba h

{- | Esta função filtra as coordenadas das explosoes, casos esteja por exemplo um tijolo no caminho. 
= Exemplos
 
>>> tiraCoord ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] 
-}
    
tiraCoord :: 
 Mapa -- ^ mapa do jogo
  -> [(Int,Int)] -- ^ lista da zona que vai explodir
   ->[(Int,Int)] -- ^ lista da zona q vai explodir apos verificar o que está no mapa
tiraCoord _  [] = []
tiraCoord m ((c,l):t) | not(temPrUp m (show c,show l) == "+" || temPrUp m (show c,show l) == "!" ) && not(temParede m (show c,show l)) && not(temtijolo m (show c,show l)) = (c,l) : tiraCoord m t 
                      | otherwise = []

{- | Esta função dá o tamanho do mapa .

= Exemplos
 
>>> tamanhomp ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] 
9
-}

tamanhomp :: [String] -> Int
tamanhomp m = length (head m)                      