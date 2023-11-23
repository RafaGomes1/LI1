{- |
Module      : Tarefa2_2021li1g056
Description : Construção/Desconstrução do mapa
Copyright   : Afonso Antunes Martins <a96452@alunos.uminho.pt>;
            : Rafael João Ferreira Gomes <a96208@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g056 where

import LI12122

{- | Esta tarefa consiste em construir e desconstruir um mapa.

Construir o mapa está dividido em três principais funções:

1. A função 'acrescentaVazios' que cria uma lista de peças e coordenadas acrescentando todos os vazios à lista.
   Como auxiliares temos:

    - A função 'maiorXeY' que determina o maior X e Y. 
   
    - A função 'existeCoordenada' que verifica se existe uma coordenada numa lista.
   
    - A função 'buscaPeca' que através de umas coordenadas e de uma lista determina uma peça.

2. A função 'agrupa' que agrupa listas pelos seus Ys.

3. A função 'buscaMatriz' que seleciona as peças numa lista de [(Pecas,Coordenadas)].
   Como auxiliar temos: 

   - A função 'busca' que seleciona as pecas numa lista.


Desconstruir o mapa está dividido em duas principais funções:

1. A função 'medidas' que determina o valor máximo de X e Y.

2. A função 'desconstroiLinha' que descontroi uma linha de peças para uma lista de [(Peca,Coordenadas)].
-}

--1ªPARTE
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = let l1 = acrescentaVazios l
                 in buscaMatriz (agrupa l1)


--2ºPARTE
-- | função que desconstói um mapa para uma lista de peças e coordenadas
desconstroiMapa :: Mapa -> [(Peca,Coordenadas)]
desconstroiMapa l = ajuda l (0,0) (medidas l)
   where ajuda :: Mapa -> Coordenadas -> Coordenadas -> [(Peca,Coordenadas)]
         ajuda [] (m1,m2) (x1,y1) = []
         ajuda (p:ps) (m1,m2) (x1,y1)
            | m2 <= y1 = (desconstroiLinha p (m1,m2)) ++ ajuda ps (0,m2+1) (x1,y1)
            | otherwise = []

--------------------------------------------------------------1ªPARTE-----------------------------------------------------------------

-- | função que acrescenta vazios à lista recebida
acrescentaVazios :: [(Peca,Coordenadas)]             -- ^ recebe uma lista de peças e coordenadas
 -> [(Peca,Coordenadas)]                             -- ^ acrescenta vazios à lista recebida
acrescentaVazios l = ajuda l (0,0) (maiorXeY l)
   where ajuda :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas -> [(Peca,Coordenadas)]
         ajuda l (m1,m2) (x,y)
            | m2 > y = []
            | not (existeCoordenada (m1,m2) l) && (m1 <= x) = (Vazio,(m1,m2)) : ajuda l (m1+1,m2) (x,y)
            | existeCoordenada (m1,m2) l = ((buscaPeca (m1,m2) l),(m1,m2)) : ajuda l (m1+1,m2) (x,y)
            | m1 > x = ajuda l (0,m2+1) (x,y)

-- | função que determina o maior X e Y
maiorXeY :: [(Peca,Coordenadas)]                     -- ^ recebe uma lista de peças e coordenadas
 -> Coordenadas                                      -- ^ determina o maior X e Y
maiorXeY [(_,c)] = c
maiorXeY ((p1,(x1,y1)) : (p2,(x2,y2)) :t)
   | (x1 >= x2) && (y1 >= y2) = maiorXeY ((p1,(x1,y1)) : t)
   | (x1 <  x2) && (y1 >= y2) = maiorXeY ((p1,(x2,y1)) : t)
   | (x1 >= x2) && (y1 <  y2) = maiorXeY ((p1,(x1,y2)) : t)
   | otherwise = maiorXeY ((p1,(x2,y2)) : t)

-- | função que verifica se uma coordenada existe numa lista
existeCoordenada :: Coordenadas                      -- ^ recebe um par de coordenadas
 -> [(Peca,Coordenadas)]                             -- ^ recebe uma lista de peças e coordenadas
 -> Bool                                             -- ^ verifica se uma coordenada existe numa lista
existeCoordenada _ [] = False
existeCoordenada (x1,y1) ((_,(x2,y2)):t)
   | (x1==x2) && (y1==y2) = True
   | otherwise = existeCoordenada (x1,y1) t

-- | função que determina a peça que tem essas coordenadas
buscaPeca :: Coordenadas                             -- ^ recebe um par de coordenadas
 -> [(Peca,Coordenadas)]                             -- ^ recebe uma lista de peças e coordenadas
 -> Peca                                             -- ^ determina a peça que tem essas coordenadas
buscaPeca (x1,y1) ((p,(x2,y2)):t)
   | (x1==x2) && (y1==y2) = p
   | otherwise = buscaPeca (x1,y1) t

-------------------------------------------------------------------------------------------------------------------------------------

-- | função que constrói listas de listas guiando-se pelos Ys
agrupa :: [(Peca, Coordenadas)]                      -- ^ recebe uma lista de peças e coordenadas
 -> [[(Peca, Coordenadas)]]                          -- ^ constrói listas de listas guiando-se pelos Ys
agrupa [] = []
agrupa (x:xs) = ajuda [x] xs
   where ajuda :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
         ajuda l [] = [l]
         ajuda ((p1,(x1,y1)):t1) ((p2,(x2,y2)):t2)
            | (y1==y2) =  let pl = ((p1,(x1,y1)):t1) ++ [(p2,(x2,y2))]
                          in ajuda pl t2
            | (y1/=y2) = ((p1,(x1,y1)):t1) : ajuda [(p2,(x2,y2))] t2

-------------------------------------------------------------------------------------------------------------------------------------

-- | função que constrói uma lista de lista de peças
buscaMatriz :: [[(Peca, Coordenadas)]]               -- ^ recebe uma lista de peças e coordenadas
 -> Mapa                                             -- ^ constrói uma lista de lista de peças
buscaMatriz [] = []
buscaMatriz (x:xs) = busca x : buscaMatriz xs

-- | função que constrói uma lista de peças
busca :: [(Peca,Coordenadas)]                        -- ^ recebe uma lista de peças e coordenadas
 -> [Peca]                                           -- ^ constrói uma lista de peças
busca [] = []
busca ((p,_):t) = p : busca t




--------------------------------------------------------------2ªPARTE-----------------------------------------------------------------

-- | função que determina o máximo do X e Y
medidas :: Mapa                                      -- ^ recebe um mapa
 -> Coordenadas                                      -- ^ determina o máximo do X e do Y
medidas l = let x = length (head l)
                y = length l
            in (x-1,y-1)

-- | função que constrói uma lista de peças e coordenadas
desconstroiLinha :: [Peca]                           -- ^ recebe uma lista de peças
 -> Coordenadas                                      -- ^ um par de coordenadas que inicialmente vão ser (0,0)
 -> [(Peca, Coordenadas)]                            -- ^ constrói uma lista de peças e coordenadas
desconstroiLinha [] (m1,m2) = []
desconstroiLinha (Vazio:ps) (m1,m2) = desconstroiLinha ps (m1+1,m2)
desconstroiLinha (p:ps) (m1,m2) = (p,(m1,m2)) : desconstroiLinha ps (m1+1,m2)
