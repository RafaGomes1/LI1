{- |
Module      : Tarefa3_2021li1g056
Description : Representação textual do jogo
Copyright   : Afonso Antunes Martins <a96452@alunos.uminho.pt>;
            : Rafael João Ferreira Gomes <a96208@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g056 where

import LI12122

{- | Esta tarefa consiste em representar o Jogo.

Para tal utiliza-se a função 'criaJogo' que recebe um jogador e um mapa no qual vai criar, linha a linha (separado por "/n") através do 'insereLinha'.
O 'insereLinha' consiste em pegar no jogador e pegar na primeira lista de peças do mapa e colocar o jogador na mesma string (linha) caso as coordenadas do jogador pertençam a essa linha se não avança para a próxima lista.
Através da função 'mostraPeca' e 'mostraJogador' é possivel transformar uma valor do tipo peça ou jogador numa string.
-}

instance Show Jogo where
   show (Jogo l j) = criaJogo j l

-- | função que introduz o jogador e o mapa na mesma string
criaJogo :: Jogador       -- ^ recebe um jogador
 -> Mapa                  -- ^ recebe um mapa
 -> String                -- ^ introduz o jogador e o mapa na mesma string
criaJogo j l = ajuda j l (0,0)
   where ajuda :: Jogador -> Mapa -> Coordenadas -> String
         ajuda j [] c = ""
         ajuda j [x] c = insereLinha j x c
         ajuda j (h:t) (m1,m2) = insereLinha j h (m1,m2) ++ "\n" ++ ajuda j t (0,m2+1)

-- | função que introduz se possível o jogador numa linha (string) juntamente com peças
insereLinha :: Jogador    -- ^ recebe um jogador
 -> [Peca]                -- ^ recebe uma lista de peças
 -> Coordenadas           -- ^ recebe um par de coordenadas que inicialmente vão ser (0,0)
 -> String                -- ^ introduz se possível o jogador numa linha (string) juntamente com peças
insereLinha j [] c = ""
insereLinha (Jogador (x,y) d b) (h:t) (m1,m2)
   | (m1 == x) && (m2 == y) = mostraJogador (Jogador (x,y) d b) ++ insereLinha (Jogador (x,y) d b) t (m1+1,m2)
   | otherwise = mostraPeca h ++ insereLinha (Jogador (x,y) d b) t (m1+1,m2)

-- | função que transforma uma peça numa string
mostraPeca :: Peca        -- ^ recebe uma peça
 -> String                -- ^ transforma numa string
mostraPeca Vazio = " "
mostraPeca Bloco = "X"
mostraPeca Caixa = "C"
mostraPeca Porta = "P"

-- | função que transforma um jogador numa string
mostraJogador :: Jogador  -- ^ recebe um jogador
 -> String                -- ^ transforma numa string
mostraJogador (Jogador c Oeste b) = "<"
mostraJogador (Jogador c Este b)  = ">"
