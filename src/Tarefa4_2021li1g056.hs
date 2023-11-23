{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa4_2021li1g056
Description : Movimentação do personagem
Copyright   : Afonso Antunes Martins <a96452@alunos.uminho.pt>;
            : Rafael João Ferreira Gomes <a96208@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g056 where

import LI12122

{- | Esta tarefa consiste em definir os movimentos do jogador.

1. As funções 'eVazio', 'eCaixa' e 'eCaixaOuBloco' são usadas em vários movimentos com ou sem o jogador estar a carregar a caixa.

2. A função 'trocaCaixa' é utilizada quando o jogador interage com uma caixa. Nas primeiras coordenadas que esta função recebe é inserida uma caixa no mapa através da função 'insereCaixa' e
nas segundas coordenadas que a função recebe é inserido um vazio no mapa através da função 'insereVazio'.

3. A função 'gravidade' determina até onde é possivel o jogador ou caixa caírem.
-}

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j
correrMovimentos j (x:xs) = let p = moveJogador j x
                            in correrMovimentos p xs

-- | função que recebe um jogo e um movimento e altera o jogo
moveJogador :: Jogo -> Movimento -> Jogo
--movimentos com o jogador sem este estar a carregar uma caixa
moveJogador (Jogo l (Jogador (x,y) _ False)) AndarEsquerda
   | eVazio (x-1,y) l = Jogo l (Jogador (gravidade (x-1,y) l (x-1,y)) Oeste False)
   | otherwise = Jogo l (Jogador (x,y) Oeste False)
moveJogador (Jogo l (Jogador (x,y) _ False)) AndarDireita
   | eVazio (x+1,y) l = Jogo l (Jogador (gravidade (x+1,y) l (x+1,y)) Este False)
   | otherwise = Jogo l (Jogador (x,y) Este False)
moveJogador j@(Jogo l (Jogador (x,y) Oeste False)) Trepar
   | eVazio (x-1,y-1) l && eCaixaOuBloco (x-1,y) l = Jogo l (Jogador (x-1,y-1) Oeste False)
   | otherwise = j
moveJogador j@(Jogo l (Jogador (x,y) Este False)) Trepar
   | eVazio (x+1,y-1) l && eCaixaOuBloco (x+1,y) l = Jogo l (Jogador (x+1,y-1) Este False)
   | otherwise = j
moveJogador j@(Jogo l (Jogador (x,y) Oeste False)) InterageCaixa
   | eCaixa (x-1,y) l && eVazio (x-1,y-1) l && eVazio (x,y-1) l = Jogo (trocaCaixa (x,y-1) (x-1,y) l) (Jogador (x,y) Oeste True)
   | otherwise = j
moveJogador j@(Jogo l (Jogador (x,y) Este False)) InterageCaixa
   | eCaixa (x+1,y) l && eVazio (x+1,y-1) l && eVazio (x,y-1) l = Jogo (trocaCaixa (x,y-1) (x+1,y) l) (Jogador (x,y) Este True)
   | otherwise = j
--movimentos com o jogador a carregar uma caixa
moveJogador (Jogo l (Jogador (x,y) _ True)) AndarEsquerda
   | eVazio (x-1,y) l && eVazio (x-1,y-1) l = let (m1,m2) = gravidade (x-1,y) l (x-1,y)
                                              in Jogo (trocaCaixa (m1,m2-1) (x,y-1) l) (Jogador (m1,m2) Oeste True)
   | otherwise = Jogo l (Jogador (x,y) Oeste True)
moveJogador (Jogo l (Jogador (x,y) _ True)) AndarDireita
   | eVazio (x+1,y) l && eVazio (x+1,y-1) l = let (m1,m2) = gravidade (x+1,y) l (x+1,y)
                                              in Jogo (trocaCaixa (m1,m2-1) (x,y-1) l) (Jogador (m1,m2) Este True)
   | otherwise = Jogo l (Jogador (x,y) Este True)
moveJogador j@(Jogo l (Jogador (x,y) Oeste True)) InterageCaixa
   | eVazio (x-1,y-1) l = Jogo (trocaCaixa (gravidade (x-1,y-1) l (x-1,y-1)) (x,y-1) l) (Jogador (x,y) Oeste False)
   | otherwise = j
moveJogador j@(Jogo l (Jogador (x,y) Este True)) InterageCaixa
   | eVazio (x+1,y-1) l = Jogo (trocaCaixa (gravidade (x+1,y-1) l (x+1,y-1)) (x,y-1) l) (Jogador (x,y) Este False)
   | otherwise = j
moveJogador j@(Jogo l (Jogador (x,y) Oeste True)) Trepar
   | eVazio (x-1,y-1) l && eVazio (x,y-2) l &&  eVazio (x-1,y-2) l && eCaixaOuBloco (x-1,y) l = Jogo (trocaCaixa (x-1,y-2) (x,y-1) l) (Jogador (x-1,y-1) Oeste True)
   | otherwise = j
moveJogador j@(Jogo l (Jogador (x,y) Este True)) Trepar
   | eVazio (x+1,y-1) l && eVazio (x,y-2) l &&  eVazio (x+1,y-2) l && eCaixaOuBloco (x+1,y) l = Jogo (trocaCaixa (x+1,y-2) (x,y-1) l) (Jogador (x+1,y-1) Este True)
   | otherwise = j

--------------------------------------------------------------------------------------------------------
-- | função que verifica se no mapa essas coordenadas pertencem a um vazio
eVazio :: Coordenadas                                             -- ^ recebe um par de coordenadas
 -> Mapa                                                          -- ^ recebe um mapa
 -> Bool                                                          -- ^ verifica se no mapa essas coordenadas pertencem a um vazio
eVazio (x,y) (h:t)
   | y == 0 = ajuda x h
   | y > 0  = eVazio (x,y-1) t
   | otherwise = False
   where ajuda :: Int -> [Peca] -> Bool
         ajuda _ [] = False
         ajuda x (h:t)
            | x == 0 && h == Vazio = True
            | x > 0 = ajuda (x-1) t
            | otherwise = False

-- | função que verifica se no mapa essas coordenadas pertencem a uma caixa
eCaixa :: Coordenadas                                             -- ^ recebe um par de coordenadas
 -> Mapa                                                          -- ^ recebe um mapa
 -> Bool                                                          -- ^ verifica se no mapa essas coordenadas pertencem a uma caixa
eCaixa (x,y) (h:t)
   | y == 0 = ajuda x h
   | y > 0  = eCaixa (x,y-1) t
   | otherwise = False
   where ajuda :: Int -> [Peca] -> Bool
         ajuda _ [] = False
         ajuda x (h:t)
            | x == 0 && h == Caixa = True
            | x > 0 = ajuda (x-1) t
            | otherwise = False

-- | função que verifica se no mapa essas coordenadas pertencem a uma bloco
eCaixaOuBloco :: Coordenadas                                     -- ^ recebe um par de coordenadas
 -> Mapa                                                         -- ^ recebe um mapa
 -> Bool                                                         -- ^ verifica se no mapa essas coordenadas pertencem a uma caixa ou um bloco
eCaixaOuBloco (x,y) (h:t)
   | y == 0 = ajuda x h
   | y > 0  = eCaixaOuBloco (x,y-1) t
   | otherwise = False
   where ajuda :: Int -> [Peca] -> Bool
         ajuda _ [] = False
         ajuda x (h:t)
            | x == 0 && (h == Caixa || h == Bloco) = True
            | x > 0 = ajuda (x-1) t
            | otherwise = False

--------------------------------------------------------------------------------------------------------
-- | função que constrói o mapa atualizado
trocaCaixa :: Coordenadas                                        -- ^ recebe um par de coordenadas onde vai ser inserida a caixa
 -> Coordenadas                                                  -- ^ recebe um par de coordenadas onde vai ser inserido o vazio
 -> Mapa                                                         -- ^ recebe um mapa onde vão ser trocadas as peças
 -> Mapa                                                         -- ^ constrói o mapa atualizado
trocaCaixa c1 c2 l = let p1 = insereCaixa c1 l
                     in insereVazio c2 p1

-- | função que insere a caixa nessas coordenadas do mapa
insereCaixa :: Coordenadas                                       -- ^ recebe um par de coordenadas onde vai ser inserida a caixa
 -> Mapa                                                         -- ^ recebe um mapa
 -> Mapa                                                         -- ^ insere a caixa nessas coordenadas do mapa
insereCaixa _ [] = []
insereCaixa (x,y) (h:t)
   | y == 0 = (ajuda x h) : t
   | y > 0 = h : insereCaixa (x,y-1) t
   where ajuda :: Int -> [Peca] -> [Peca]
         ajuda _ [] = []
         ajuda x (h:t)
            | x == 0 = Caixa : t
            | otherwise = h : ajuda (x-1) t

-- | função que insere o vazio nessas coordenadas do mapa
insereVazio :: Coordenadas                                       -- ^ recebe um par de coordenadas onde vai ser inserido o vazio
 -> Mapa                                                         -- ^ recebe um mapa
 -> Mapa                                                         -- ^ insere o vazio nessas coordenadas do mapa
insereVazio _ [] = []
insereVazio (x,y) (h:t)
   | y == 0 = (ajuda x h) : t
   | y > 0 = h : insereVazio (x,y-1) t
   where ajuda :: Int -> [Peca] -> [Peca]
         ajuda _ [] = []
         ajuda x (h:t)
            | x == 0 = Vazio : t
            | otherwise = h : ajuda (x-1) t

--------------------------------------------------------------------------------------------------------
-- | função que determina a posição que até onde foi possível descer
gravidade :: Coordenadas                                         -- ^ recebe as coordenadas que vão sendo testadas até descobrir um obstáculo
 -> Mapa                                                         -- ^ recebe um mapa
 -> Coordenadas                                                  -- ^ recebe as coordenadas que vão ser anteriores ao obstáculo
 -> Coordenadas                                                  -- ^ determina a posição que até onde foi possível descer
gravidade (x,y) l (m1,m2) = if eVazio (x,y) l
                       then gravidade (x,y+1) l (x,y)
                       else (m1,m2)
