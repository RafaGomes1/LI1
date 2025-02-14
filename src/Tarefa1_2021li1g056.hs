{- |
Module      : Tarefa1_2021li1g056
Description : Validação de um potencial mapa
Copyright   : Afonso Antunes Martins <a96452@alunos.uminho.pt>;
            : Rafael João Ferreira Gomes <a96208@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}

module Tarefa1_2021li1g056 where

import LI12122
{- | Esta tarefa consiste em validar um potencial mapa. Seguindo os seguintes critérios:

1. Todas as coordenadas devem ser diferentes entre si.
2. Só deve existir exatamente uma porta.
3. Nenhuma caixa deve estar a "flutuar".
4. Existir no mínimo um espaço vazio.
5. Deve existir um chão ao longo do mapa.

No 1º tópico foi utilizada a função 'diferentesCoordenadas' para verificar se todas são diferentes.

No 2º tópico foi utilizada a função 'soUmaPorta' para verificar se apenas existe uma porta.
A função 'contaPortas' foi utilizada como auxiliar para determinar o número total de portas.

No 3º tópico foi utilizada a função 'posicaoCaixas' para verificar se todas as caixas estão bem posicionadas.
A função 'procuraCaixas' constrói uma lista apenas de caixas com as suas coordenadas.
A função 'deBaixoDaCaixa' verifica se existe um bloco ou uma caixa de baixo de uma caixa.

No 4º tópico foi utilizada a função 'haVazios' para verificar se o nº total de peças era diferente de o nº total da matriz.
A função 'maiorXeY' serve para tirar as dimensões da matriz.
A função 'conta' serve para enumerar o nº total de peças.

No 5º tópico foi utilizada a função 'validaChao' para verificar a partir do bloco mais à esquerda se existe uma ligação de blocos até ao bloco mais à direita.
A função 'procuraBlocos' constói uma lista só de blocos.
A função 'blocoMaisBaixoEsq' determina o bloco mais em baixo e mais à esquerda de todos.
A função 'blocoMaisBaixoDir' determina o bloco mais em baixo e mais à direita de todos.
A função 'haSeguinte' verifica se existe um bloco com uma determinada coordenada.
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa l = (diferentesCoordenadas l) &&
                        (soUmaPorta l)            &&
                        (posicaoCaixas l)         &&
                        (haVazios l)              &&
                        (validaChao l)

--------------------------------------------------------------------------------------------------------
-- | função que verifica se todas as peças têm diferentes coordenadas
diferentesCoordenadas :: [(Peca,Coordenadas)]  -- ^ recebe uma lista de peças e coordenadas
 -> Bool                                       -- ^ verifica se as peças são todas diferentes
diferentesCoordenadas [] = True
diferentesCoordenadas (x:xs) = if (ajuda x xs)
                               then diferentesCoordenadas xs
                               else False
   where ajuda :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
         ajuda (p,c) [] = True
         ajuda (p1,(x1,y1)) ((p2,(x2,y2)):t)
            | x1 == x2 && y1 == y2 = False
            | otherwise = ajuda (p1,(x1,y1)) t

--------------------------------------------------------------------------------------------------------
-- | função que verifica se apenas existe uma porta
soUmaPorta :: [(Peca,Coordenadas)]              -- ^ recebe uma lista de peças e coordenadas
 -> Bool                                        -- ^ verifica se existe apenas uma porta
soUmaPorta l = (contaPortas l) == 1

-- | função que conta o nº total de portas
contaPortas :: [(Peca,Coordenadas)]             -- ^ recebe uma lista de peças e coordenadas
 -> Int                                         -- ^ retorna o nº total de portas
contaPortas [] = 0
contaPortas ((Porta,c):t) = 1 + contaPortas t
contaPortas (_:t) = contaPortas t

--------------------------------------------------------------------------------------------------------
-- | função que verifica o posicionamento das caixas
posicaoCaixas :: [(Peca,Coordenadas)]           -- ^ recebe uma lista de peças e coordenadas
 -> Bool                                        -- ^ verifica se as caixas todas estão bem posicionadas
posicaoCaixas [] = True
posicaoCaixas l = ajuda l (procuraCaixas l)
   where ajuda :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Bool
         ajuda l [] = True
         ajuda l (x:xs) = if deBaixoDaCaixa x l 
                          then ajuda l xs
                          else False

-- | função que constroi uma lista de caixas
procuraCaixas :: [(Peca,Coordenadas)]           -- ^ recebe uma lista de peças e coordenadas
 -> [(Peca,Coordenadas)]                        -- ^ constrói uma lista só de caixas e as suas coordenadas
procuraCaixas [] = []
procuraCaixas ((Caixa,c):t) = (Caixa,c) : procuraCaixas t
procuraCaixas (_:t) = procuraCaixas t

-- | função que verifica se existe algo de baixo de uma caixa
deBaixoDaCaixa :: (Peca,Coordenadas)            -- ^ recebe uma caixa e as suas coordenadas
 -> [(Peca,Coordenadas)]                        -- ^ recebe uma lista de peças e coordenadas
 -> Bool                                        -- ^ verifica se existe algo de baixo da caixa
deBaixoDaCaixa (p,c) [] = False
deBaixoDaCaixa (Caixa,(x1,y1)) ((Bloco,(x2,y2)):t)
   | (x1 == x2) && (y2-1) == y1 = True
   | otherwise = deBaixoDaCaixa (Caixa,(x1,y1)) t
deBaixoDaCaixa (Caixa,(x1,y1)) ((Caixa,(x2,y2)):t)
   | (x1 == x2) && (y2-1) == y1 = True
   | otherwise = deBaixoDaCaixa (Caixa,(x1,y1)) t
deBaixoDaCaixa (Caixa,(x1,y1)) (_:t) = deBaixoDaCaixa (Caixa,(x1,y1)) t

--------------------------------------------------------------------------------------------------------
-- | função que verifica se existe pelo menos um vazio
haVazios :: [(Peca,Coordenadas)]                -- ^ recebe uma lista de peças e coordenadas
 ->  Bool                                       -- ^ verifica se existe pelo menos um espaço vazio
haVazios l = let (x,y) = maiorXeY l
                 m     = conta l
             in m /= (x+1)*(y+1)

-- | função que determina o maior X e Y
maiorXeY :: [(Peca,Coordenadas)]                -- ^ recebe uma lista de peças e coordenadas
 -> Coordenadas                                 -- ^ determina o maior X e o maior Y
maiorXeY [(_,c)] = c
maiorXeY ((p1,(x1,y1)) : (p2,(x2,y2)) :t)
   | (x1 >= x2) && (y1 >= y2) = maiorXeY ((p1,(x1,y1)) : t)
   | (x1 <  x2) && (y1 >= y2) = maiorXeY ((p1,(x2,y1)) : t)
   | (x1 >= x2) && (y1 <  y2) = maiorXeY ((p1,(x1,y2)) : t)
   | otherwise = maiorXeY ((p1,(x2,y2)) : t)

-- | função que determina o nº total de peças
conta :: [(Peca,Coordenadas)]                   -- ^ recebe uma lista de peças e coordenadas
 -> Int                                         -- ^ determina o nº total de peças
conta [] = 0
conta ((p,c):t) = 1 + conta t

--------------------------------------------------------------------------------------------------------
-- | função que verifica se o chão está corretamente construído
validaChao :: [(Peca,Coordenadas)]              -- ^ recebe uma lista de peças e coordenadas
 -> Bool                                        -- ^ verifica se o chão está corretamente construído
validaChao l = (x == 0) && ajuda (procuraBlocos l) (blocoMaisBaixoEsq l) 0 (blocoMaisBaixoDir l)
   where (x,y) = blocoMaisBaixoEsq l
         ajuda :: [Coordenadas] -> Coordenadas -> Int -> (Int,Int) -> Bool
         ajuda l (x1,y1) m (x2,y2)
            | (x1 == x2) && (y1==y2) = True
            | (haSeguinte (x1,y1+1) l) && (m /= (-1)) = ajuda l (x1,y1+1) 1 (x2,y2)
            | (haSeguinte (x1+1,y1) l) = ajuda l (x1+1,y1) 0 (x2,y2)
            | (haSeguinte (x1,y1-1) l) && (m /= 1)   = ajuda l (x1,y1-1) (-1) (x2,y2)
            | (haSeguinte (x1+1,y1+1) l) = ajuda l (x1+1,y1+1) 0 (x2,y2)
            | (haSeguinte (x1+1,y1-1) l) = ajuda l (x1+1,y1-1) 0 (x2,y2)
            | otherwise = False

-- | função que constrói uma lista apenas com as coordenadas dos blocos
procuraBlocos :: [(Peca,Coordenadas)]           -- ^ recebe uma lista de peças e coordenadas
 -> [Coordenadas]                               -- ^ constrói uma lista só de coordenadas de blocos
procuraBlocos [] = []
procuraBlocos ((Bloco,c):t) = c : procuraBlocos t
procuraBlocos (_ : t) = procuraBlocos t

-- | função que determina as coordenadas do bloco mais em baixo e à esquerda
blocoMaisBaixoEsq :: [(Peca,Coordenadas)]       -- ^ recebe uma lista de peças e coordenadas
 -> Coordenadas                                 -- ^ determina as coordenadas do bloco mais em baixo e à esquerda
blocoMaisBaixoEsq [(p,c)] = c
blocoMaisBaixoEsq ((p1,(x1,y1)):(p2,(x2,y2)):t)
   | (x1 < x2) = blocoMaisBaixoEsq ((p1,(x1,y1)):t)
   | (x1==x2) && (y1>y2) = blocoMaisBaixoEsq ((p1,(x1,y1)):t)
   | otherwise = blocoMaisBaixoEsq ((p2,(x2,y2)):t)

-- | função que determina as coordenadas do bloco mais em baixo e à direita
blocoMaisBaixoDir :: [(Peca,Coordenadas)]       -- ^ recebe uma lista de peças e coordenadas
 -> Coordenadas                                 -- ^ determina as coordenadas do bloco mais em baixo e à direita
blocoMaisBaixoDir [(p,c)] = c
blocoMaisBaixoDir ((p1,(x1,y1)):(p2,(x2,y2)):t)
   | (x1 > x2) = blocoMaisBaixoDir ((p1,(x1,y1)):t)
   | (x1==x2) && (y1>y2) = blocoMaisBaixoDir ((p1,(x1,y1)):t)
   | otherwise = blocoMaisBaixoDir ((p2,(x2,y2)):t)

-- | função que verifica se existe um bloco com uma determinada coordenada
haSeguinte :: Coordenadas                       -- ^ recebe um par de coordenadas
 -> [Coordenadas]                               -- ^ recebe uma lista com as coordenadas de todos os blocos
 -> Bool                                        -- ^ verifica se existe um bloco com uma determinada coordenada
haSeguinte _ [] = False
haSeguinte (x1,y1) ((x2,y2):t)
   | (x1==x2) && (y1==y2) = True
   | otherwise = haSeguinte (x1,y1) t
