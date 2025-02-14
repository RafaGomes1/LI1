{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12122
import Tarefa4_2021li1g056
import Data.Maybe
import System.Exit
import Data.Char
import System.Random

{- | Esta tarefa consiste em criar os gráficos e a interação do jogo.
     
     Neste Jogo inicialmente temos 3 opções.
     Jogar, Sair ou Instruções.
     Instruções - mostra quais as teclas que podemos usar e para que servem.
     Sair - abandonamos o jogo.
     Se selecionarmos Jogar vai levar nos para um sub Menu com duas opções.
     Níveis que está dividido em 3. Fácil, Médio e Díficil. Cada um deles tem 3 jogos, podemos fazer restart em qualquer um deles e também podemos salvar e continuar mais tarde. No final de concluir qualquer
     um dos modos de dificuldade aparece uma mensagem para terminar e sair.
     Contra-Relogio que existe um temporizador que vai decrementando. Por cada jogo que passamos ganhámos mais 60 segundos de cada vez. E quando o temporizador chegar a 0 aparece uma mensagem com a nossa pontuação e a melhor
     pontuação até ao momento e nesta mesma mensagem existe a opção de jogar de novo ou sair.
     Tanto na opção Níveis ou Contra-Relógio temos à escolha 8 jogadores diferentes e cada um deles com um fundo de jogo diferente.
-}

-- | estado global que estamos a usar
type Mundo = (Estado, Save, Imagens)

-- | estado mais particular
data Estado = Menu String          -- indica se estamos a selecionar "Jogar", "Sair" ou "Instruções"
             | Ins String          -- indica se estamos dentro das "Instruções"
             | Menu2 String        -- indica se estamos a selecionar "Níveis", "Contra-Relógio" ou a Seta
             | N String            -- indica se estamos a selecionar "Fácil", "Médio" ou "Difícil"
                 Int               -- indica o nível do jogo
             | J String            -- indica qual dos jogadores estamos a selecionar ou para regressar ao Menu2
                 Int               -- indica qual dos níveis vamos jogar ou se vamos jogar o CR
             | CR (Int             -- indica o tempo do CR
                  ,Jogo            -- indica o jogo que estamos a jogar de momento
                  ,Coordenadas     -- indica as coordenadas da porta do jogo que estamos a jogar de momento
                  ,[Jogo]          -- indica a lista de jogos que ainda faltam para termeninar
                  ,Int             -- indica o jogador que escolhemos
                  ,Int             -- indica o jogo em que estamos para poder fazer restart
                  ,Int             -- indica a pontuação final que fizemos no CR
                  )
             | Mensagem2 String    -- indica a pontuação que fizemos, a melhor até ao momento e estamos a selecionar "Sair" ou "Novo Jogo"
                         Int       -- indica a pontuação final que fizemos no CR
             | Game Jogo           -- indica o jogo que estamos a jogar de momento
                    Coordenadas    -- indica as coordenadas da porta do jogo que estamos a jogar de momento
                    [Jogo]         -- indica a lista de jogos que ainda faltam para termeninar
                    Int            -- indica o jogador que escolhemos
                    Int            -- indica o jogo em que estamos para poder fazer restart
             | Mensagem String     -- indica que chegamos ao fim do nível que estivemos a jogar e estamos a selecionar "Sair"

-- | para saber estados e valores acumulados
type Save = (Int -- Int caso seja diferente de 0 para saber se já foi dado o save no Fácil
            ,Int -- Int caso seja diferente de 0 para saber se já foi dado o save no Médio
            ,Int -- Int caso seja diferente de 0 para saber se já foi dado o save no Difícil
            ,Int -- valor mais alto alcançado no CR
            )

-- | imagens divididas por categorias
data Imagens = Imagens 
   { pecas :: [(Peca,Picture)]       -- imagens de peças
   , jogador :: [(Float,Picture)]    -- imagens de jogadores
   , fundo :: [(Int,Picture)]        -- imagens de fundo
   , palavras :: [(String,Picture)]  -- imagens de palavras
   , niveis :: [(String,Picture)]    -- imagens utilizadas no N
}

-- | função que fornece o estado inicial
estadoInicial :: Estado
estadoInicial = (Menu "Jogar1")

--Menu

-- | dimensões do retângulo exterior usado no Menu e no Menu2 nos "Níveis"
rectangulo1 :: Picture
rectangulo1 = polygon (rectanglePath (fromIntegral 250) (fromIntegral 125))

-- | dimensões do retângulo interior usado no usado no Menu e no Menu2 nos "Níveis"
rectangulo2 :: Picture
rectangulo2 = polygon (rectanglePath (fromIntegral 230) (fromIntegral 105))

-- | rectângulo vermelho usado como selecionador no Menu de "Jogar" e "Sair"
rectVermelho = Color red rectangulo1

-- | rectângulo branco usado como selecionador no Menu de "Jogar" e "Sair"
rectBranco = Color white rectangulo2

-- | rectângulo vermelho usado como selecionador no Menu de "Instruções"
rectVermelhoIns = Color red rectIns1

-- | rectângulo branco usado como selecionador no Menu de "Instruções"
rectBrancoIns = Color white rectIns2

-- | a selecionar a palavra "Jogar"
cimaJ1 = Translate (-650) (-400) $ Pictures [rectVermelho,rectBranco]

-- | a não selecionar a palavra "Jogar"
cimaJ2 = Translate (-650) (-400) $ rectBranco

-- | a selecionar a palavra "Sair"
baixoS1 = Translate (-350) (-400) $ Pictures [rectVermelho,rectBranco]

-- | a não selecionar a palavra "Sair"
baixoS2 = Translate (-350) (-400) $ rectBranco

-- | a selecionar a palavra "Instruções"
ladoS1 = Translate 650 (-400) $ Pictures [rectVermelhoIns,rectBrancoIns]

-- | a não selecionar a palavra "Instruções"
ladoS2 = Translate 650 (-400) $ rectBrancoIns



--Ins

-- | dimensões do retângulo exterior usado no Ins
rectIns1 :: Picture
rectIns1 = polygon (rectanglePath (fromIntegral 305) (fromIntegral 125))

-- | dimensões do retângulo interior usado no Ins
rectIns2 :: Picture
rectIns2 = polygon (rectanglePath (fromIntegral 285) (fromIntegral 105))

-- | rectângulo preto usado no Ins
rectPretoIns = Color black rectIns1

-- | rectângulo principal no Ins
ins1 = Translate 1050 (-147) $ Pictures [rectPretoIns,rectBrancoIns]



--Menu2

-- | dimensões do retângulo exterior usado no Menu2 no CR
rectangulo3 :: Picture
rectangulo3 = polygon (rectanglePath (fromIntegral 504) (fromIntegral 125))

-- | dimensões do retângulo interior usado no Menu2 no CR
rectangulo4 :: Picture
rectangulo4 = polygon (rectanglePath (fromIntegral 480) (fromIntegral 105))

-- | desenho preenchido da seta 
poligono :: Picture
poligono = polygon [(-200,-100) , (-100,-70) , (-100,-90) , (-20,-90) , (-20,-110) , (-100,-110) , (-100,-130)]

-- | desenho linha da seta 
contorno1 :: Picture
contorno1 = line [(-200,-100) , (-100,-70) , (-100,-90) , (-20,-90) , (-20,-110) , (-100,-110) , (-100,-130) , (-200,-100)]

-- | atribuição da cor vermelha ao rectângulo exterior no Menu2 de "Níveis"
rectVermelho2 = Color (withRed 0.6 black) rectangulo1

-- | atribuição da cor branco ao rectângulo interior no Menu2 de "Níveis"
rectBranco1 = Color white rectangulo2

-- | atribuição da cor vermelha ao rectângulo exterior no Menu2 de "Contra-Relógio"
rectVermelho3 = Color (withRed 0.6 black) rectangulo3

-- | atribuição da cor preto ao rectângulo interior no Menu2 de "Contra-Relógio"
rectPreto3 = Color black rectangulo3

-- | atribuição da cor branco ao rectângulo interior no Menu2 de "Contra-Relógio"
rectBranco2 = Color white rectangulo4

-- | atribuição da cor vermelha da seta preenchida
setaV = Color (withRed 0.6 black) poligono

-- | atribuição da cor vermelha da seta linha
contVermelho1 = Color (withRed 0.6 black) contorno1

-- | rectângulo vermelho usado como selecionador no Menu2 de "Níveis"
cimaN1 = Translate 0 (350) $ Pictures [rectVermelho2,rectBranco1]

-- | rectângulo branco usado como não selecionador no Menu2 de "Níveis"
cimaN2 = Translate 0 (350) $ rectBranco1

-- | rectângulo vermelho usado como selecionador no Menu2 de "Contra-Relógio"
meioC1 = Translate 0 0 $ Pictures [rectVermelho3,rectBranco2]

-- | rectângulo preto e branco usado como  não selecionador no Menu2 de "Contra-Relógio"
meioC2 = Translate 0 0 $ Pictures [rectPreto3,rectBranco2]

-- | seta preenchida como selecionador
seta1 = Scale 2 3.6 $ Translate (-241) 24 $ setaV

-- | seta linha como selecionador
seta2 = Scale 2 3.6 $ Translate (-241) 24 $ contVermelho1



--Niveis

-- | dimensões do retângulo exterior usado no N de "Fácil", "Médio" e "Difícil"
rectangulo5 :: Picture
rectangulo5 = polygon (rectanglePath (fromIntegral 292) (fromIntegral 116))

-- | dimensões do retângulo interior usado no N de "Fácil", "Médio" e "Difícil"
rectangulo6 :: Picture
rectangulo6 = polygon (rectanglePath (fromIntegral 268) (fromIntegral 96))

-- | atribuição da cor vermelha ao retângulo exterior usado no N de "Fácil", "Médio" e "Difícil"
rectVermelho4 = Color (withRed 0.6 black) rectangulo5

-- | atribuição da cor preto ao retângulo interior usado no N de "Fácil", "Médio" e "Difícil"
rectPreto4 = Color black rectangulo6

-- | rectângulo a dizer "Níveis"
rectN1 = Translate (-650) 430 $ Pictures [rectPreto3,rectBranco2]

-- | selecionador do nível "Fácil"
cimaN = Translate 0 (140) $ Pictures [rectVermelho4,rectPreto4]

-- | selecionador do nível "Médio"
meioN = Translate 0 (-90) $ Pictures [rectVermelho4,rectPreto4]

-- | selecionador do nível "Difícil"
baixoN = Translate 0 (-320) $ Pictures [rectVermelho4,rectPreto4]



--Mensagens

-- | dimensões do quadrado exterior usado na Mensagem e Mensagem2
quadrado1 :: Picture
quadrado1 = rectangleSolid 800 800

-- | dimensões do quadrado interior usado na Mensagem e Mensagem2
quadrado2 :: Picture
quadrado2 = rectangleSolid 780 780

-- | dimensões do selecionador usado na Mensagem e Mensagem2 de "Sair"
contorno4 :: Picture
contorno4 = line ((rectanglePath 75 50) ++ [(-38,-25)])

-- | dimensões do selecionador usado na Mensagem2 de "Novo Jogo"
contorno5 :: Picture
contorno5 = line ((rectanglePath 155 50) ++ [(-78,-25)])

-- | atribuição da cor preta ao quadrado exterior usado na Mensagem e Mensagem2
mPreto = Color black quadrado1

-- | atribuição da cor branco ao quadrado interior usado na Mensagem e Mensagem2
mBranco = Color white quadrado2

-- | atribuição da cor vermelha ao selecionador usado na Mensagem e Mensagem2 de "Sair"
contVermelho4 = Color red contorno4

-- | atribuição da cor vermelha ao selecionador usado na Mensagem2 de "Novo Jogo"
contVermelho5 = Color red contorno5

-- | selecionador de "Sair" usado na Mensagem e Mensagem2 de "Sair"
selecionadorSair = Translate (-338) (-350) $ contVermelho4

-- | selecionador de "Novo Jogo" usado na Mensagem2 de "Novo Jogo"
selecionadorNovo = Translate 292 (-350) $ contVermelho5



-- | função que converte segundos para uma string em minutos e segundos (m:s)
segundosPmin :: Int -> String
segundosPmin x = let y = div x 60
                     k = mod x 60
                 in show y ++ ":" ++ show k



-- | função que descobre as coordenadas de uma porta
encontraPorta :: Jogo -> Coordenadas
encontraPorta (Jogo m _) = ajuda m (0,0)
   where ajuda :: Mapa -> Coordenadas -> Coordenadas
         ajuda ((Porta:t):ts) (m1,m2) = (m1,m2)
         ajuda ((_:t):ts) (m1,m2)     = ajuda (t:ts) (m1+1,m2)
         ajuda ([]:ts) (m1,m2)        = ajuda ts (0,m2+1)



-- | função que transforma um Game e um CR para uma única imagem
mapaToPicture :: Mundo -> [Picture]
mapaToPicture (Game j c l n1 n2, s, imgs) = ajuda (Game j c l n1 n2, s, imgs) (0,0)
   where ajuda :: Mundo -> Coordenadas -> [Picture]
         ajuda (Game (Jogo [] j) c l n1 n2, s, imgs) _                 = []
         ajuda (Game (Jogo ((Bloco:t):ts) j) c l n1 n2, s, imgs) (x,y) = (Translate (fromIntegral x) (fromIntegral y) $ Scale 0.2 0.2 $ (fromJust $ lookup Bloco $ pecas imgs)) : ajuda (Game (Jogo (t:ts) j) c l n1 n2, s, imgs) (x+50,y)
         ajuda (Game (Jogo ((Caixa:t):ts) j) c l n1 n2, s, imgs) (x,y) = (Translate (fromIntegral x) (fromIntegral y) $ Scale 0.22 0.22 $ (fromJust $ lookup Caixa $ pecas imgs)) : ajuda (Game (Jogo (t:ts) j) c l n1 n2, s, imgs) (x+50,y)
         ajuda (Game (Jogo ((Vazio:t):ts) j) c l n1 n2, s, imgs) (x,y) = ajuda (Game (Jogo (t:ts) j) c l n1 n2, s, imgs) (x+50,y)
         ajuda (Game (Jogo ((Porta:t):ts) j) c l n1 n2, s, imgs) (x,y) = (Translate (fromIntegral x) (fromIntegral y) $ Scale 0.013 0.013 $ (fromJust $ lookup Porta $ pecas imgs)) : ajuda (Game (Jogo (t:ts) j) c l n1 n2, s, imgs) (x+50,y)
         ajuda (Game (Jogo ([]:ts) j) c l n1 n2, s, imgs)        (x,y) = ajuda (Game (Jogo ts j) c l n1 n2, s, imgs) (0,y-50)
mapaToPicture (CR (s1,j,c,l,n1,n2,n3), s, imgs) = ajuda (CR (s1,j,c,l,n1,n2,n3), s, imgs) (0,0)
   where ajuda :: Mundo -> Coordenadas -> [Picture]
         ajuda (CR (s1,(Jogo [] j),c,l,n1,n2,n3), s, imgs) _                 = []
         ajuda (CR (s1,(Jogo ((Bloco:t):ts) j),c,l,n1,n2,n3), s, imgs) (x,y) = (Translate (fromIntegral x) (fromIntegral y) $ Scale 0.2 0.2 $ (fromJust $ lookup Bloco $ pecas imgs)) : ajuda (CR (s1,(Jogo (t:ts) j),c,l,n1,n2,n3), s, imgs) (x+50,y)
         ajuda (CR (s1,(Jogo ((Caixa:t):ts) j),c,l,n1,n2,n3), s, imgs) (x,y) = (Translate (fromIntegral x) (fromIntegral y) $ Scale 0.22 0.22 $ (fromJust $ lookup Caixa $ pecas imgs)) : ajuda (CR (s1,(Jogo (t:ts) j),c,l,n1,n2,n3), s, imgs) (x+50,y)
         ajuda (CR (s1,(Jogo ((Vazio:t):ts) j),c,l,n1,n2,n3), s, imgs) (x,y) = ajuda (CR (s1,(Jogo (t:ts) j),c,l,n1,n2,n3), s,  imgs) (x+50,y)
         ajuda (CR (s1,(Jogo ((Porta:t):ts) j),c,l,n1,n2,n3), s, imgs) (x,y) = (Translate (fromIntegral x) (fromIntegral y) $ Scale 0.013 0.013 $ (fromJust $ lookup Porta $ pecas imgs)) : ajuda (CR (s1,(Jogo (t:ts) j),c,l,n1,n2,n3), s, imgs) (x+50,y)
         ajuda (CR (s1,(Jogo ([]:ts) j),c,l,n1,n2,n3), s, imgs)        (x,y) = ajuda (CR (s1,(Jogo ts j),c,l,n1,n2,n3), s, imgs) (0,y-50)



-- | função que descobre as coordenadas centrais de um mapa
descobreCentro :: Mapa -> Coordenadas
descobreCentro l = (div (length (head l)) 2 , - (div (length l) 2))



-- | função que verifica através dos inteiros s1, s2 e s3 se já foi dado o save nos Níveis Fácil, Médio e Difícil respetivamente e caso tenho dado retorna o estado onse se encontrava antes
saveOrNot :: Mundo -> IO Mundo
saveOrNot (J "deadpool" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 1 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 1 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 1 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "deadpool" n, s, imgs)
saveOrNot (J "wolverine" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 2 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 2 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 2 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "wolverine" n, s, imgs)
saveOrNot (J "hulk" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 3 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 3 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 3 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "hulk" n, s, imgs)
saveOrNot (J "spider" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 4 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 4 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 4 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "spider" n, s, imgs)
saveOrNot (J "ironman" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 5 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 5 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 5 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "ironman" n, s, imgs)
saveOrNot (J "america" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 6 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 6 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 6 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "america" n, s, imgs)
saveOrNot (J "thor" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 7 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 7 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 7 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "thor" n, s, imgs)
saveOrNot (J "pantera" n, s@(s1,s2,s3,s4), imgs)
   | n == 1 && s1 /= 0 = do m1 <- readFile "mapa1.text"
                            cd <- readFile "jcoord1.text"
                            d1 <- readFile "direcao1.text"
                            b1 <- readFile "bool1.text"
                            c1 <- readFile "coordporta1.text"
                            k1 <- readFile "nivel1.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 8 k2) ,s ,imgs)
   | n == 2 && s2 /= 0 = do m1 <- readFile "mapa2.text"
                            cd <- readFile "jcoord2.text"
                            d1 <- readFile "direcao2.text"
                            b1 <- readFile "bool2.text"
                            c1 <- readFile "coordporta2.text"
                            k1 <- readFile "nivel2.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 8 k2) ,s ,imgs)
   | n == 3 && s3 /= 0 = do m1 <- readFile "mapa3.text"
                            cd <- readFile "jcoord3.text"
                            d1 <- readFile "direcao3.text"
                            b1 <- readFile "bool3.text"
                            c1 <- readFile "coordporta3.text"
                            k1 <- readFile "nivel3.text"
                            let m2 = read m1
                                cd1 = read cd
                                d2 = read d1
                                b2 = read b1
                                c2 = read c1
                                k2 = read k1
                                l2 = descobreListaMapas (Jogo m2 (Jogador cd1 d2 b2))
                            return ((Game (Jogo m2 (Jogador cd1 d2 b2)) c2 l2 8 k2) ,s ,imgs)
   | otherwise = escolheNivel (J "pantera" n, s, imgs)



-- | função utilizada na função "saveOrNot" para descobrir a lista de mapas que restava quando este deu save
descobreListaMapas :: Jogo -> [Jogo]
descobreListaMapas (Jogo m j)
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo1) = [jogo2,jogo3]
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo2) = [jogo3]
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo3) = []
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo4) = [jogo5,jogo6]
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo5) = [jogo6]
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo6) = []
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo7) = [jogo8,jogo9]
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo8) = [jogo9]
   | (encontraPorta (Jogo m j)) == (encontraPorta jogo9) = []



-- | função que cria a 1ª vez que jogamos os Níveis e o CR
escolheNivel :: Mundo -> IO Mundo
escolheNivel (J "deadpool" n, s, imgs)
   | n == 0 = do 
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,1,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 1 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 1 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 1 7, s, imgs)
escolheNivel (J "wolverine" n, s, imgs)
   | n == 0 = do
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,2,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 2 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 2 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 2 7, s, imgs)
escolheNivel (J "hulk" n,  s, imgs)
   | n == 0 = do
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,3,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 3 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 3 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 3 7, s, imgs)
escolheNivel (J "spider" n, s, imgs)
   | n == 0 = do 
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,4,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 4 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 4 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 4 7, s, imgs)
escolheNivel (J "ironman" n, s, imgs)
   | n == 0 = do 
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,5,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 5 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 5 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 5 7, s, imgs)
escolheNivel (J "america" n, s, imgs)
   | n == 0 = do 
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,6,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 6 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 6 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 6 7, s, imgs)
escolheNivel (J "thor" n, s, imgs)
   | n == 0 = do 
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,7,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 7 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 7 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 7 7, s, imgs)
escolheNivel (J "pantera" n, s, imgs)
   | n == 0 = do 
                let j = [jogo5,jogo9,jogo6,jogo4,jogo2,jogo7,jogo3,jogo8]
                return $ (CR (300,jogo1,(encontraPorta jogo1),j,8,ajudaRestart jogo1,0), s, imgs)
   | n == 1 = return $ (Game jogo1 (encontraPorta jogo1) [jogo2,jogo3] 8 1, s, imgs)
   | n == 2 = return $ (Game jogo4 (encontraPorta jogo4) [jogo5,jogo6] 8 4, s, imgs)
   | n == 3 = return $ (Game jogo7 (encontraPorta jogo7) [jogo8,jogo9] 8 7, s, imgs)



-- | função que descobre o jogo em que se encontra para poder comecçar de novo (apenas no CR)
ajudaRestart :: Jogo -> Int
ajudaRestart j
   | encontraPorta j == encontraPorta jogo1 = 1
   | encontraPorta j == encontraPorta jogo2 = 2
   | encontraPorta j == encontraPorta jogo3 = 3
   | encontraPorta j == encontraPorta jogo4 = 4
   | encontraPorta j == encontraPorta jogo5 = 5
   | encontraPorta j == encontraPorta jogo6 = 6
   | encontraPorta j == encontraPorta jogo7 = 7
   | encontraPorta j == encontraPorta jogo8 = 8
   | encontraPorta j == encontraPorta jogo9 = 9



-- | função que fornece dependendo do jogo em que se encontra, o fim ou o próximo jogo (nos Níveis e no CR)
seguinte :: Mundo -> Mundo
seguinte (Game x _ [] n1 n2, (s1,s2,s3,s4), imgs)      | (encontraPorta x) == (encontraPorta jogo3) = (Mensagem "Fim", (0,s2,s3,s4), imgs)
                                                       | (encontraPorta x) == (encontraPorta jogo6) = (Mensagem "Fim", (s1,0,s3,s4), imgs)
                                                       | (encontraPorta x) == (encontraPorta jogo9) = (Mensagem "Fim", (s1,s2,0,s4), imgs)
seguinte (Game j c (x:xs) n1 n2, s, imgs) = (Game x (encontraPorta x) xs n1 (n2+1), s, imgs)
seguinte (CR (t,j,c,[],n1,n2,n3), (s1,s2,s3,s4), imgs) = if n3 >= s4
                                                         then (Mensagem2 "Novo Jogo" n3, (s1,s2,s3,n3), imgs)
                                                         else (Mensagem2 "Novo Jogo" n3, (s1,s2,s3,s4), imgs)
seguinte (CR (t,j,c,(x:xs),n1,n2,n3), s, imgs)         = (CR (t+60,x,(encontraPorta x),xs,n1,(ajudaRestart x),n3+1), s, imgs)



-- | função que através de um inteiro retorno as posiçoes iniciais de tudo no jogo
restart :: Mundo -> Mundo
restart (Game j c l n1 n2, s, imgs) = case n2 of
                                       1 -> (Game jogo1 (encontraPorta jogo1) l n1 n2, s, imgs)
                                       2 -> (Game jogo2 (encontraPorta jogo2) l n1 n2, s, imgs)
                                       3 -> (Game jogo3 (encontraPorta jogo3) l n1 n2, s, imgs)
                                       4 -> (Game jogo4 (encontraPorta jogo4) l n1 n2, s, imgs)
                                       5 -> (Game jogo5 (encontraPorta jogo5) l n1 n2, s, imgs)
                                       6 -> (Game jogo6 (encontraPorta jogo6) l n1 n2, s, imgs)
                                       7 -> (Game jogo7 (encontraPorta jogo7) l n1 n2, s, imgs)
                                       8 -> (Game jogo8 (encontraPorta jogo8) l n1 n2, s, imgs)
                                       9 -> (Game jogo8 (encontraPorta jogo8) l n1 n2, s, imgs)
restart (CR (t,j,c,l,n1,n2,n3), s, imgs) = case n2 of
                                            1 -> (CR (t,jogo1,(encontraPorta jogo1),l,n1,n2,n3), s, imgs)
                                            2 -> (CR (t,jogo2,(encontraPorta jogo2),l,n1,n2,n3), s, imgs)
                                            3 -> (CR (t,jogo3,(encontraPorta jogo3),l,n1,n2,n3), s, imgs)
                                            4 -> (CR (t,jogo4,(encontraPorta jogo4),l,n1,n2,n3), s, imgs)
                                            5 -> (CR (t,jogo5,(encontraPorta jogo5),l,n1,n2,n3), s, imgs)
                                            6 -> (CR (t,jogo6,(encontraPorta jogo6),l,n1,n2,n3), s, imgs)
                                            7 -> (CR (t,jogo7,(encontraPorta jogo7),l,n1,n2,n3), s, imgs)
                                            8 -> (CR (t,jogo8,(encontraPorta jogo8),l,n1,n2,n3), s, imgs)
                                            9 -> (CR (t,jogo9,(encontraPorta jogo9),l,n1,n2,n3), s, imgs)



-- | função que converte um estado para uma imagem
desenhaEstado :: Mundo -> IO Picture
-- Menu
desenhaEstado (Menu "Jogar1", s ,imgs)                       = return $ Pictures [Scale 1.12 1.12 $ (fromJust $ lookup 9 $ fundo imgs),
                                                                                  cimaJ1,baixoS2, ladoS2,
                                                                                  (Scale 1.5 1.5 $ Translate (-45) (-445) $ (fromJust $ lookup "jogar" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (123) (-306) $ (fromJust $ lookup "sair" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (897) (-400) $ (fromJust $ lookup "inst" $ palavras imgs))

                                                                                 ]
desenhaEstado (Menu "Jogar2", s ,imgs)                       = return $ Pictures [Scale 1.12 1.12 $ (fromJust $ lookup 9 $ fundo imgs),
                                                                                  cimaJ2,baixoS2, ladoS2,
                                                                                  (Scale 1.5 1.5 $ Translate (-45) (-445) $ (fromJust $ lookup "jogar" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (123) (-306) $ (fromJust $ lookup "sair" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (897) (-400) $ (fromJust $ lookup "inst" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu "Sair1", s ,imgs)                        = return $ Pictures [Scale 1.12 1.12 $ (fromJust $ lookup 9 $ fundo imgs),
                                                                                  cimaJ2,baixoS1, ladoS2,
                                                                                  (Scale 1.5 1.5 $ Translate (-45) (-445) $ (fromJust $ lookup "jogar" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (123) (-306) $ (fromJust $ lookup "sair" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (897) (-400) $ (fromJust $ lookup "inst" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu "Sair2", s ,imgs)                        = return $ Pictures [Scale 1.12 1.12 $ (fromJust $ lookup 9 $ fundo imgs),
                                                                                  cimaJ2,baixoS2, ladoS2,
                                                                                  (Scale 1.5 1.5 $ Translate (-45) (-445) $ (fromJust $ lookup "jogar" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (123) (-306) $ (fromJust $ lookup "sair" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (897) (-400) $ (fromJust $ lookup "inst" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu "Instrucoes1", s ,imgs)                  = return $ Pictures [Scale 1.12 1.12 $ (fromJust $ lookup 9 $ fundo imgs),
                                                                                  cimaJ2,baixoS2, ladoS1,
                                                                                  (Scale 1.5 1.5 $ Translate (-45) (-445) $ (fromJust $ lookup "jogar" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (123) (-306) $ (fromJust $ lookup "sair" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (897) (-400) $ (fromJust $ lookup "inst" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu "Instrucoes2", s ,imgs)                  = return $ Pictures [Scale 1.12 1.12 $ (fromJust $ lookup 9 $ fundo imgs),
                                                                                  cimaJ2,baixoS2, ladoS2,
                                                                                  (Scale 1.5 1.5 $ Translate (-45) (-445) $ (fromJust $ lookup "jogar" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (123) (-306) $ (fromJust $ lookup "sair" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate (897) (-400) $ (fromJust $ lookup "inst" $ palavras imgs))
                                                                                 ]
-- Ins
desenhaEstado (Ins "instrucoes1", s, imgs) = return $ Translate (-350) 0 $ Pictures [Scale 1.2 1.2 $ (fromJust $ lookup 11 $ fundo imgs),
                                                                                     Scale 0.75 0.75 $ Translate 70 (-320) $ (fromJust $ lookup "ae" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 190 (-440) $ (fromJust $ lookup "ad" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 165 (-442) $ (fromJust $ lookup "trepar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 170 (-477) $ (fromJust $ lookup "interage" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 172 (-510) $ (fromJust $ lookup "r" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (24) (-540) $ (fromJust $ lookup "salvar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (265) (-565) $ (fromJust $ lookup "1" $ palavras imgs),
                                                                                     ins1,
                                                                                     (Scale 1.25 1.25 $ Translate 1302 (-250) $ (fromJust $ lookup "inst" $ palavras imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 1500 0 $ (fromJust $ lookup 12 $ fundo imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 550 443 $ (fromJust $ lookup 13 $ fundo imgs)),
                                                                                     (Rotate (-15) $ Scale 0.6 0.6 $ Translate 380 (-440) $ (fromJust $ lookup 14 $ fundo imgs)),
                                                                                     (Rotate (-20) $ Scale 0.6 0.6 $ Translate 850 (-850) $ (fromJust $ lookup 15 $ fundo imgs))
                                                                                    ]
desenhaEstado (Ins "instrucoes2", s, imgs) = return $ Translate (-350) 0 $ Pictures [Scale 1.2 1.2 $ (fromJust $ lookup 11 $ fundo imgs),
                                                                                     Scale 0.75 0.75 $ Translate 70 (-320) $ (fromJust $ lookup "ae" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 190 (-440) $ (fromJust $ lookup "ad" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 165 (-442) $ (fromJust $ lookup "trepar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 170 (-477) $ (fromJust $ lookup "interage" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 172 (-510) $ (fromJust $ lookup "r" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (24) (-540) $ (fromJust $ lookup "salvar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (265) (-565) $ (fromJust $ lookup "1" $ palavras imgs),
                                                                                     ins1,
                                                                                     (Scale 1.25 1.25 $ Translate 1302 (-250) $ (fromJust $ lookup "inst" $ palavras imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 1500 0 $ (fromJust $ lookup 17 $ fundo imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 550 443 $ (fromJust $ lookup 13 $ fundo imgs)),
                                                                                     (Rotate (-15) $ Scale 0.6 0.6 $ Translate 380 (-440) $ (fromJust $ lookup 14 $ fundo imgs)),
                                                                                     (Rotate (-20) $ Scale 0.6 0.6 $ Translate 850 (-850) $ (fromJust $ lookup 15 $ fundo imgs)),
                                                                                     Rotate 30 $ (Scale 0.7 0.7 $ Translate 450 620 $ (fromJust $ lookup 16 $ fundo imgs))
                                                                                    ]
desenhaEstado (Ins "instrucoes3", s, imgs) = return $ Translate (-350) 0 $ Pictures [Scale 1.2 1.2 $ (fromJust $ lookup 11 $ fundo imgs),
                                                                                     Scale 0.75 0.75 $ Translate 70 (-320) $ (fromJust $ lookup "ae" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 190 (-440) $ (fromJust $ lookup "ad" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 165 (-442) $ (fromJust $ lookup "trepar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 170 (-477) $ (fromJust $ lookup "interage" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 172 (-510) $ (fromJust $ lookup "r" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (24) (-540) $ (fromJust $ lookup "salvar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (265) (-565) $ (fromJust $ lookup "1" $ palavras imgs),
                                                                                     ins1,
                                                                                     (Scale 1.25 1.25 $ Translate 1302 (-250) $ (fromJust $ lookup "inst" $ palavras imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 1500 0 $ (fromJust $ lookup 17 $ fundo imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 550 443 $ (fromJust $ lookup 13 $ fundo imgs)),
                                                                                     (Rotate (-15) $ Scale 0.6 0.6 $ Translate 380 (-440) $ (fromJust $ lookup 14 $ fundo imgs)),
                                                                                     (Rotate (-20) $ Scale 0.6 0.6 $ Translate 850 (-850) $ (fromJust $ lookup 15 $ fundo imgs)),
                                                                                     (Rotate (-30) $ Scale 0.7 0.7 $ Translate 490 (-490) $ (fromJust $ lookup 16 $ fundo imgs))
                                                                                    ]
desenhaEstado (Ins "instrucoes4",s , imgs) = return $ Translate (-350) 0 $ Pictures [Scale 1.2 1.2 $ (fromJust $ lookup 11 $ fundo imgs),
                                                                                     Scale 0.75 0.75 $ Translate 70 (-320) $ (fromJust $ lookup "ae" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 190 (-440) $ (fromJust $ lookup "ad" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 165 (-442) $ (fromJust $ lookup "trepar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 170 (-477) $ (fromJust $ lookup "interage" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate 172 (-510) $ (fromJust $ lookup "r" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (24) (-540) $ (fromJust $ lookup "salvar" $ palavras imgs),
                                                                                     Scale 0.75 0.75 $ Translate (265) (-565) $ (fromJust $ lookup "1" $ palavras imgs),
                                                                                     ins1,
                                                                                     (Scale 1.25 1.25 $ Translate 1302 (-250) $ (fromJust $ lookup "inst" $ palavras imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 1500 0 $ (fromJust $ lookup 17 $ fundo imgs)),
                                                                                     (Scale 0.7 0.7 $ Translate 550 443 $ (fromJust $ lookup 13 $ fundo imgs)),
                                                                                     (Rotate (-15) $ Scale 0.6 0.6 $ Translate 380 (-440) $ (fromJust $ lookup 14 $ fundo imgs)),
                                                                                     (Rotate (-20) $ Scale 0.6 0.6 $ Translate 850 (-850) $ (fromJust $ lookup 15 $ fundo imgs)),
                                                                                     (Rotate (-40) $ Scale 0.7 0.7 $ Translate 640 (-980) $ (fromJust $ lookup 16 $ fundo imgs))
                                                                                    ]
-- Menu2
desenhaEstado (Menu2 "Niveis1",s ,imgs)                      = return $ Pictures [Translate 0 50 $ Scale 1.5 1.5 $ (fromJust $ lookup 10 $ fundo imgs),
                                                                                  cimaN1,meioC2,seta1,
                                                                                  (Scale 1.5 1.5 $ Translate 257 83 $ (fromJust $ lookup "niveis" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate 288 (-160) $ (fromJust $ lookup "contra" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu2 "Niveis2",s ,imgs)                      = return $ Pictures [Translate 0 50 $ Scale 1.5 1.5 $ (fromJust $ lookup 10 $ fundo imgs),
                                                                                  cimaN2,meioC2,seta1,
                                                                                  (Scale 1.5 1.5 $ Translate 257 83 $ (fromJust $ lookup "niveis" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate 288 (-160) $ (fromJust $ lookup "contra" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu2 "Contra-Relogio1",s ,imgs)              = return $ Pictures [Translate 0 50 $ Scale 1.5 1.5 $ (fromJust $ lookup 10 $ fundo imgs),
                                                                                  cimaN2,meioC1,seta1,
                                                                                  (Scale 1.5 1.5 $ Translate 257 83 $ (fromJust $ lookup "niveis" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate 288 (-160) $ (fromJust $ lookup "contra" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu2 "Contra-Relogio2",s ,imgs)              = return $ Pictures [Translate 0 50 $ Scale 1.5 1.5 $ (fromJust $ lookup 10 $ fundo imgs),
                                                                                  cimaN2,meioC2,seta1,
                                                                                  (Scale 1.5 1.5 $ Translate 257 83 $ (fromJust $ lookup "niveis" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate 288 (-160) $ (fromJust $ lookup "contra" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu2 "Seta1",s ,imgs)                        = return $ Pictures [Translate 0 50 $ Scale 1.5 1.5 $ (fromJust $ lookup 10 $ fundo imgs),
                                                                                  cimaN2,meioC2,seta1,
                                                                                  (Scale 1.5 1.5 $ Translate 257 83 $ (fromJust $ lookup "niveis" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate 288 (-160) $ (fromJust $ lookup "contra" $ palavras imgs))
                                                                                 ]
desenhaEstado (Menu2 "Seta2",s ,imgs)                        = return $ Pictures [Translate 0 50 $ Scale 1.5 1.5 $ (fromJust $ lookup 10 $ fundo imgs),
                                                                                  cimaN2,meioC2,seta2,
                                                                                  (Scale 1.5 1.5 $ Translate 257 83 $ (fromJust $ lookup "niveis" $ palavras imgs)),
                                                                                  (Scale 1.5 1.5 $ Translate 288 (-160) $ (fromJust $ lookup "contra" $ palavras imgs))
                                                                                 ]
-- N
desenhaEstado (N "Faceis" n, s, imgs)                        = return $ Pictures [Scale 0.25 0.25 $ Translate (-3400) (-1790) $ (fromJust $ lookup "circP" $ niveis imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-8500) (-4500) $ (fromJust $ lookup "saidaV" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate (-383) 552 $ (fromJust $ lookup "pendura" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate 0 500 $ (fromJust $ lookup "tiro" $ niveis imgs),
                                                                                  cimaN,meioN,baixoN,rectN1,
                                                                                  Scale 1.25 1.25 $ Translate 450 (-15) $ (fromJust $ lookup "facil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 250 (-299) $ (fromJust $ lookup "medio" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 300 (-500) $ (fromJust $ lookup "dificil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate (-262) 194 $ (fromJust $ lookup "niveis" $ palavras imgs)
                                                                                 ]
desenhaEstado (N "Medios" n, s, imgs)                        = return $ Pictures [Scale 0.25 0.25 $ Translate (-3400) (-1790) $ (fromJust $ lookup "circP" $ niveis imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-8500) (-4500) $ (fromJust $ lookup "saidaV" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate (-383) 552 $ (fromJust $ lookup "pendura" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate 0 212 $ (fromJust $ lookup "tiro" $ niveis imgs),
                                                                                  cimaN,meioN,baixoN,rectN1,
                                                                                  Scale 1.25 1.25 $ Translate 450 (-15) $ (fromJust $ lookup "facil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 250 (-299) $ (fromJust $ lookup "medio" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 300 (-500) $ (fromJust $ lookup "dificil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate (-262) 194 $ (fromJust $ lookup "niveis" $ palavras imgs)
                                                                                 ]
desenhaEstado (N "Dificeis" n, s, imgs)                      = return $ Pictures [Scale 0.25 0.25 $ Translate (-3400) (-1790) $ (fromJust $ lookup "circP" $ niveis imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-8500) (-4500) $ (fromJust $ lookup "saidaV" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate (-383) 552 $ (fromJust $ lookup "pendura" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate 0 (-76) $ (fromJust $ lookup "tiro" $ niveis imgs),
                                                                                  cimaN,meioN,baixoN,rectN1,
                                                                                  Scale 1.25 1.25 $ Translate 450 (-15) $ (fromJust $ lookup "facil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 250 (-299) $ (fromJust $ lookup "medio" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 300 (-500) $ (fromJust $ lookup "dificil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate (-262) 194 $ (fromJust $ lookup "niveis" $ palavras imgs)
                                                                                 ]
desenhaEstado (N "SaidaV1" n, s, imgs)                       = return $ Pictures [Scale 0.25 0.25 $ Translate (-3400) (-1790) $ (fromJust $ lookup "circV" $ niveis imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-8500) (-4500) $ (fromJust $ lookup "saidaV" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate (-383) 552 $ (fromJust $ lookup "pendura" $ niveis imgs),
                                                                                  cimaN,meioN,baixoN,rectN1,
                                                                                  Scale 1.25 1.25 $ Translate 450 (-15) $ (fromJust $ lookup "facil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 250 (-299) $ (fromJust $ lookup "medio" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 300 (-500) $ (fromJust $ lookup "dificil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate (-262) 194 $ (fromJust $ lookup "niveis" $ palavras imgs)
                                                                                 ]
desenhaEstado (N "SaidaV2" n, s, imgs)                       = return $ Pictures [Scale 0.25 0.25 $ Translate (-3400) (-1790) $ (fromJust $ lookup "circP" $ niveis imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-8500) (-4500) $ (fromJust $ lookup "saidaV" $ niveis imgs),
                                                                                  Scale 0.8 0.8 $ Translate (-383) 552 $ (fromJust $ lookup "pendura" $ niveis imgs),
                                                                                  cimaN,meioN,baixoN,rectN1,
                                                                                  Scale 1.25 1.25 $ Translate 450 (-15) $ (fromJust $ lookup "facil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 250 (-299) $ (fromJust $ lookup "medio" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate 300 (-500) $ (fromJust $ lookup "dificil" $ palavras imgs),
                                                                                  Scale 1.25 1.25 $ Translate (-262) 194 $ (fromJust $ lookup "niveis" $ palavras imgs)
                                                                                 ]
-- J
desenhaEstado (J "deadpool" n, s, imgs)                      = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-7500) 600 $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "wolverine" n, s, imgs )                    = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-3200) 600 $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "hulk" n, s, imgs)                          = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate 1070 600 $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "spider" n, s, imgs)                        = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate 5100 600 $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "ironman" n, s, imgs)                       = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-7500) (-3020) $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "america" n, s, imgs)                       = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate (-3250) (-3050) $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "thor" n, s, imgs)                          = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate (1050) (-3000) $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "pantera" n, s, imgs)                       = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.1 0.1 $ Translate (5100) (-3000) $ (fromJust $ lookup 9.9 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "saidaSeta1" n, s, imgs)                    = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs),
                                                                                  Rotate 180 $ Scale 0.2 0.2 $ Translate (4100) (2200) $ (fromJust $ lookup 9.9 $ jogador imgs)
                                                                                 ]
desenhaEstado (J "saidaSeta2" n, s, imgs)                    = return $ Pictures [Scale 1.6 1.6 $ Translate (-400) 40 $ (fromJust $ lookup 1.1 $ jogador imgs),
                                                                                  Scale 0.45 0.45 $ Translate (-450) 150 $ (fromJust $ lookup 2.1 $ jogador imgs),
                                                                                  Scale 0.37 0.37 $ Translate 600 165 $ (fromJust $ lookup 3.1 $ jogador imgs),
                                                                                  Scale 0.46 0.46 $ Translate 1400 140 $ (fromJust $ lookup 4.1 $ jogador imgs),
                                                                                  Scale 0.23 0.23 $ Translate (-2850) (-1275) $ (fromJust $ lookup 5.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate (-770) (-1087) $ (fromJust $ lookup 6.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 830 (-1085) $ (fromJust $ lookup 7.1 $ jogador imgs),
                                                                                  Scale 0.27 0.27 $ Translate 2400 (-1080) $ (fromJust $ lookup 8.1 $ jogador imgs),
                                                                                  Scale 0.9 0.9 $ Translate (-220) 450 $ (fromJust $ lookup 100 $ jogador imgs)
                                                                                 ]
-- Game
desenhaEstado j@(Game (Jogo m (Jogador (x,y) Oeste b)) _ _ n _ , s, imgs) = 
   let (m1,m2) = descobreCentro m
   in case n of
       1 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 1 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.8 0.8 $ fromJust $ lookup 1.1 $ jogador imgs]
       2 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 2 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.22 0.22 $ fromJust $ lookup 2.1 $ jogador imgs]
       3 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 3 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.19 0.19 $ fromJust $ lookup 3.1 $ jogador imgs]
       4 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 4 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.22 0.22 $ fromJust $ lookup 4.1 $ jogador imgs]
       5 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 5 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.112 0.112 $ fromJust $ lookup 5.1 $ jogador imgs]
       6 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 6 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.13 0.13 $ fromJust $ lookup 6.1 $ jogador imgs]
       7 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 7 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.13 0.13 $ fromJust $ lookup 7.1 $ jogador imgs]
       8 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 2.8 2.8 (fromJust $ lookup 8 $ fundo imgs))] ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.13 0.13 $ fromJust $ lookup 8.1 $ jogador imgs]
desenhaEstado j@(Game (Jogo m (Jogador (x,y) Este b))  _ _ n _, s, imgs)  = 
   let (m1,m2) = descobreCentro m
   in case n of 
       1 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 1 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.8 0.8 $ fromJust $ lookup 1.2 $ jogador imgs]
       2 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 2 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.22 0.22 $ fromJust $ lookup 2.2 $ jogador imgs]
       3 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 3 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.19 0.19 $ fromJust $ lookup 3.2 $ jogador imgs]
       4 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 4 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.22 0.22 $ fromJust $ lookup 4.2 $ jogador imgs]
       5 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 5 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.112 0.112 $ fromJust $ lookup 5.2 $ jogador imgs]
       6 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 6 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.13 0.13 $ fromJust $ lookup 6.2 $ jogador imgs]
       7 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 7 $ fundo imgs))]     ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.13 0.13 $ fromJust $ lookup 7.2 $ jogador imgs]
       8 -> return $ Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 2.8 2.8 (fromJust $ lookup 8 $ fundo imgs))] ++ (mapaToPicture j) ++ [ Translate (fromIntegral x*50) (fromIntegral (-y*50))     $ Scale 0.13 0.13 $ fromJust $ lookup 8.2 $ jogador imgs]
-- CR
desenhaEstado j@(CR (s1,(Jogo m (Jogador (x,y) Oeste b)),c,l,n1,n2,n3), s, imgs) =
   let (m1,m2) = descobreCentro m
       cr = [Translate 850 430 $ Scale 0.15 0.15 $ fromJust $ lookup 18 $ fundo imgs, Scale 0.28 0.28 $ Translate 2910 1458 $ (text (segundosPmin s1))]
   in case n1 of
       1 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 1 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.8 0.8 $ fromJust $ lookup 1.1 $ jogador imgs]    ] ++ cr
       2 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 2 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.22 0.22 $ fromJust $ lookup 2.1 $ jogador imgs]  ] ++ cr
       3 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 3 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.19 0.19 $ fromJust $ lookup 3.1 $ jogador imgs]  ] ++ cr
       4 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 4 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.22 0.22 $ fromJust $ lookup 4.1 $ jogador imgs]  ] ++ cr
       5 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 5 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.112 0.112 $ fromJust $ lookup 5.1 $ jogador imgs]] ++ cr
       6 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 6 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.13 0.13 $ fromJust $ lookup 6.1 $ jogador imgs]  ] ++ cr
       7 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 7 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.13 0.13 $ fromJust $ lookup 7.1 $ jogador imgs]  ] ++ cr
       8 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 2.8 2.8 (fromJust $ lookup 8 $ fundo imgs))] ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.13 0.13 $ fromJust $ lookup 8.1 $ jogador imgs]  ] ++ cr
desenhaEstado j@(CR (s1,(Jogo m (Jogador (x,y) Este b)),c,l,n1,n2,n3), s, imgs) =
   let (m1,m2) = descobreCentro m
       cr = [Translate 850 430 $ Scale 0.15 0.15 $ fromJust $ lookup 18 $ fundo imgs, Scale 0.28 0.28 $ Translate 2910 1458 $ (text (segundosPmin s1))]
   in case n1 of
       1 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 1 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.8 0.8 $ fromJust $ lookup 1.2 $ jogador imgs]    ] ++ cr
       2 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 2 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.22 0.22 $ fromJust $ lookup 2.2 $ jogador imgs]  ] ++ cr
       3 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 3 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.19 0.19 $ fromJust $ lookup 3.2 $ jogador imgs]  ] ++ cr
       4 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 4 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.22 0.22 $ fromJust $ lookup 4.2 $ jogador imgs]  ] ++ cr
       5 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 5 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.112 0.112 $ fromJust $ lookup 5.2 $ jogador imgs]] ++ cr
       6 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 6 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.13 0.13 $ fromJust $ lookup 6.2 $ jogador imgs]  ] ++ cr
       7 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 1 1 (fromJust $ lookup 7 $ fundo imgs))]     ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.13 0.13 $ fromJust $ lookup 7.2 $ jogador imgs]  ] ++ cr
       8 -> return $ Pictures $ [Translate (fromIntegral (-m1*50)) (fromIntegral (-m2*50)) $ Pictures $ [Translate (fromIntegral (m1*50)) (fromIntegral (m2*50)) (Scale 2.8 2.8 (fromJust $ lookup 8 $ fundo imgs))] ++ (mapaToPicture j) ++ [Translate (fromIntegral x*50) (fromIntegral (-y*50)) $ Scale 0.13 0.13 $ fromJust $ lookup 8.2 $ jogador imgs]  ] ++ cr  
-- Mensagem
desenhaEstado (Mensagem "Fim", s, imgs)                     = return $ Pictures [Scale 0.7 0.7 $ Translate (-680) (-430) $ (fromJust $ lookup 21 $ fundo imgs),
                                                                                  mPreto,mBranco,selecionadorSair,
                                                                                  Scale 0.7 0.7 $ Translate 515 300 $ (fromJust $ lookup 19 $ fundo imgs),
                                                                                  Scale 0.4 0.4 $ Translate 243 (-490) $ (fromJust $ lookup 20 $ fundo imgs),
                                                                                  Scale 0.7 0.7 $ Translate (-128) (-540) $ (fromJust $ lookup "sair" $ palavras imgs)
                                                                                 ]
-- Mensagem2
desenhaEstado (Mensagem2 "Novo Jogo" n1, (s1,s2,s3,s4), imgs) = return $ Pictures [Scale 1 1 $ Translate 420 (-120) $ (fromJust $ lookup 22 $ fundo imgs),
                                                                                   mPreto,mBranco,selecionadorNovo,
                                                                                   Scale 0.7 0.7 $ Translate (-193) 18 $ (fromJust $ lookup 23 $ fundo imgs),
                                                                                   Scale 0.7 0.7 $ Translate 515 300 $ (fromJust $ lookup 19 $ fundo imgs),
                                                                                   Scale 1 1 $ Translate 670 (-120) $ (fromJust $ lookup "melhor" $ palavras imgs),
                                                                                   Scale 1 1 $ Translate 300 (-250) $ (fromJust $ lookup "tua" $ palavras imgs),
                                                                                   Scale 0.7 0.7 $ Translate (-128) (-540) $ (fromJust $ lookup "sair" $ palavras imgs),
                                                                                   Scale 0.7 0.7 $ Translate (945) (-586) $ (fromJust $ lookup "novo" $ palavras imgs),
                                                                                   Scale 0.5 0.5 $ Translate (-395) (-445) $ text (show n1),
                                                                                   Scale 0.5 0.5 $ Translate 200 (-150) $ text (show s4)
                                                                                  ]
desenhaEstado (Mensagem2 "Sair" n1, (s1,s2,s3,s4), imgs)      = return $ Pictures [Scale 1 1 $ Translate 420 (-120) $ (fromJust $ lookup 22 $ fundo imgs),
                                                                                   mPreto,mBranco,selecionadorSair,
                                                                                   Scale 0.7 0.7 $ Translate (-193) 18 $ (fromJust $ lookup 23 $ fundo imgs),
                                                                                   Scale 0.7 0.7 $ Translate 515 300 $ (fromJust $ lookup 19 $ fundo imgs),
                                                                                   Scale 1 1 $ Translate 670 (-120) $ (fromJust $ lookup "melhor" $ palavras imgs),
                                                                                   Scale 1 1 $ Translate 300 (-250) $ (fromJust $ lookup "tua" $ palavras imgs),
                                                                                   Scale 0.7 0.7 $ Translate (-128) (-540) $ (fromJust $ lookup "sair" $ palavras imgs),
                                                                                   Scale 0.7 0.7 $ Translate (945) (-586) $ (fromJust $ lookup "novo" $ palavras imgs),
                                                                                   Scale 0.5 0.5 $ Translate (-395) (-445) $ text (show n1),
                                                                                   Scale 0.5 0.5 $ Translate 200 (-150) $ text (show s4)
                                                                                  ]



-- | função que através de um evento gera ou não um novo estado
reageEvento :: Event -> Mundo -> IO Mundo
-- Menu
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (Menu "Jogar1", s, imgs)                                                 = return $ (Menu "Sair1", s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (Menu "Jogar2", s, imgs)                                                 = return $ (Menu "Sair1", s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (Menu "Sair1", s, imgs)                                                  = return $ (Menu "Instrucoes1", s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (Menu "Sair2", s, imgs)                                                  = return $ (Menu "Instrucoes1", s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (Menu "Instrucoes1", s, imgs)                                            = return $ (Menu "Sair1", s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (Menu "Instrucoes2", s, imgs)                                            = return $ (Menu "Sair1", s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (Menu "Sair1", s, imgs)                                                  = return $ (Menu "Jogar1", s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (Menu "Sair2", s, imgs)                                                  = return $ (Menu "Jogar1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu "Jogar1", s, imgs)                                                 = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu "Jogar2", s, imgs)                                                 = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu "Instrucoes1", s, imgs)                                            = return $ (Ins "instrucoes1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu "Instrucoes2", s, imgs)                                            = return $ (Ins "instrucoes1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu "Sair1", s, imgs)                                                  = do putStrLn "OBRIGADO POR TER JOGADO!!!"
                                                                                                                                     exitSuccess                                                                                                                     
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu "Sair2", s, imgs)                                                  = do putStrLn "OBRIGADO POR TER JOGADO!!!"
                                                                                                                                     exitSuccess
-- Ins
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Ins _, s, imgs)                                                         = return $ (Menu "Jogar1", s, imgs)
-- Menu2
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (Menu2 "Niveis1", s, imgs)                                               = return $ (Menu2 "Contra-Relogio1", s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (Menu2 "Niveis2", s, imgs)                                               = return $ (Menu2 "Contra-Relogio1", s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (Menu2 "Contra-Relogio1", s, imgs)                                       = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (Menu2 "Contra-Relogio2", s, imgs)                                       = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (Menu2 "Contra-Relogio1", s, imgs)                                       = return $ (Menu2 "Seta1", s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (Menu2 "Contra-Relogio2", s, imgs)                                       = return $ (Menu2 "Seta1", s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (Menu2 "Seta1", s, imgs)                                                 = return $ (Menu2 "Contra-Relogio1", s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (Menu2 "Seta2", s, imgs)                                                 = return $ (Menu2 "Contra-Relogio1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu2 "Seta1", s, imgs)                                                 = return $ (Menu "Jogar1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu2 "Seta2", s, imgs)                                                 = return $ (Menu "Jogar1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu2 "Contra-Relogio1", s, imgs)                                       = return $ (J "deadpool" 0, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu2 "Contra-Relogio2", s, imgs)                                       = return $ (J "deadpool" 0, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu2 "Niveis1", s, imgs)                                               = return $ (N "Faceis" 1, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Menu2 "Niveis2", s, imgs)                                               = return $ (N "Faceis" 1, s, imgs)
-- N
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (N "Faceis" n, s, imgs)                                                  = return $ (N "Medios" (n+1), s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (N "Medios" n, s, imgs)                                                  = return $ (N "Dificeis" (n+1), s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (N "Dificeis" n, s, imgs)                                                = return $ (N "SaidaV1" 0, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (N "SaidaV1" 0, s, imgs)                                                 = return $ (N "Dificeis" 3, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (N "SaidaV2" 0, s, imgs)                                                 = return $ (N "Dificeis" 3, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (N "Dificeis" n, s, imgs)                                                = return $ (N "Medios" (n-1), s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (N "Medios" n, s, imgs)                                                  = return $ (N "Faceis" (n-1), s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (N "Faceis" n, s, imgs)                                                  = return $ (J "deadpool" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (N "Medios" n, s, imgs)                                                  = return $ (J "deadpool" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (N "Dificeis" n, s, imgs)                                                = return $ (J "deadpool" n , s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (N "SaidaV1" 0, s, imgs)                                                 = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (N "SaidaV2" 0, s, imgs)                                                 = return $ (Menu2 "Niveis1", s, imgs)
-- J
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (J "deadpool" n, s, imgs)                                                = return $ (J "wolverine" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (J "wolverine" n, s, imgs)                                               = return $ (J "hulk" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (J "hulk" n, s, imgs)                                                    = return $ (J "spider" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (J "spider" n, s, imgs)                                                  = return $ (J "hulk" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (J "hulk" n, s, imgs)                                                    = return $ (J "wolverine" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (J "wolverine" n, s, imgs)                                               = return $ (J "deadpool" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "deadpool" n, s, imgs)                                                = return $ (J "ironman" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (J "ironman" n, s, imgs)                                                 = return $ (J "deadpool" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "wolverine" n, s, imgs)                                               = return $ (J "america" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (J "america" n, s, imgs)                                                 = return $ (J "wolverine" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "hulk" n, s, imgs)                                                    = return $ (J "thor" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (J "thor" n, s, imgs)                                                    = return $ (J "hulk" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "spider" n, s, imgs)                                                  = return $ (J "pantera" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (J "pantera" n, s, imgs)                                                 = return $ (J "spider" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (J "ironman" n, s, imgs)                                                 = return $ (J "america" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (J "america" n, s, imgs)                                                 = return $ (J "thor" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  (J "thor" n, s, imgs)                                                    = return $ (J "pantera" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (J "pantera" n, s, imgs)                                                 = return $ (J "thor" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (J "thor" n, s, imgs)                                                    = return $ (J "america" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   (J "america" n, s, imgs)                                                 = return $ (J "ironman" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "ironman" n, s, imgs)                                                 = return $ (J "saidaSeta1" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "america" n, s, imgs)                                                 = return $ (J "saidaSeta1" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "thor" n, s, imgs)                                                    = return $ (J "saidaSeta1" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)   (J "pantera" n, s, imgs)                                                 = return $ (J "saidaSeta1" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (J "saidaSeta1" n, s, imgs)                                              = return $ (J "ironman" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     (J "saidaSeta2" n, s, imgs)                                              = return $ (J "ironman" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "saidaSeta1" n, s, imgs)                                              = return $ (N "Faceis" 1, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "saidaSeta2" n, s, imgs)                                              = return $ (N "Faceis" 1, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "deadpool" n, s, imgs)                                                = saveOrNot (J "deadpool" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "wolverine" n, s, imgs)                                               = saveOrNot (J "wolverine" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "hulk" n, s, imgs)                                                    = saveOrNot (J "hulk" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "spider" n, s, imgs)                                                  = saveOrNot (J "spider" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "ironman" n, s, imgs)                                                 = saveOrNot (J "ironman" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "america" n, s, imgs)                                                 = saveOrNot (J "america" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "thor" n, s, imgs)                                                    = saveOrNot (J "thor" n, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (J "pantera" n, s, imgs)                                                 = saveOrNot (J "pantera" n, s, imgs)
-- CR    
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     j@(CR (s1,(Jogo m (Jogador (x,y) d b)),c,l,n1,n2,n3), s, imgs)
   | d == Oeste && (c == (x-1,y-1))                                                                                             = return $ seguinte j
   | d == Este  && (c == (x+1,y-1))                                                                                             = return $ seguinte j
   | otherwise                                                                                                                  = return $ (CR (s1,moveJogador (Jogo m (Jogador (x,y) d b)) Trepar, c,l,n1,n2,n3), s, imgs)
reageEvento (EventKey (Char 'c') Down _ _)             (CR (s1,(Jogo m (Jogador (x,y) d b)),c,l,n1,n2,n3), s, imgs)             = return $ (CR (s1,moveJogador (Jogo m (Jogador (x,y) d b)) InterageCaixa, c,l,n1,n2,n3), s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   j@(CR (s1,(Jogo m (Jogador (x,y) d b)),c,l,n1,n2,n3), s, imgs)                 
   | d == Oeste && (c == (x-1,y) || let (m1,m2) = gravidade (x-1,y) m (x-1,y) in c == (m1,m2+1))                                = return $ seguinte j
   | otherwise                                                                                                                  = return $ (CR (s1,moveJogador (Jogo m (Jogador (x,y) d b)) AndarEsquerda, c,l,n1,n2,n3), s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  j@(CR (s1,(Jogo m (Jogador (x,y) d b)),c,l,n1,n2,n3), s, imgs)                 
   | d == Este && (c == (x+1,y) || let (m1,m2) = gravidade (x-1,y) m (x+1,y) in c == (m1,m2+1))                                 = return $ seguinte j
   | otherwise                                                                                                                  = return $ (CR (s1,moveJogador (Jogo m (Jogador (x,y) d b)) AndarDireita, c,l,n1,n2,n3), s, imgs)
reageEvento (EventKey (Char '1') Down _ _)             (CR (s1,(Jogo m (Jogador (x,y) d b)),c,l,n1,n2,n3), s, imgs)             = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (Char 'r') Down _ _)             j@(CR (s1,(Jogo m (Jogador (x,y) d b)),c,l,n1,n2,n3), s, imgs)           = return $ restart j
-- Mensagem2
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   ((Mensagem2 "Novo Jogo" n1), s, imgs)                                    = return $ (Mensagem2 "Sair" n1, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  ((Mensagem2 "Sair" n1), s, imgs)                                         = return $ (Menu2 "Niveis1", s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  ((Mensagem2 "Sair" n1), s, imgs)                                         = return $ (Mensagem2 "Novo Jogo" n1, s, imgs)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  (Mensagem2 "Novo Jogo" n1, s, imgs)                                      = return $ (J "deadpool" 0, s, imgs)
-- Game
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)     j@((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , s, imgs)
   | d == Oeste && (c == (x-1,y-1))                                                                                             = return $ seguinte j
   | d == Este  && (c == (x+1,y-1))                                                                                             = return $ seguinte j
   | otherwise                                                                                                                  = return $ (Game (moveJogador (Jogo m (Jogador (x,y) d b)) Trepar) c l n1 n2, s, imgs)
reageEvento (EventKey (Char 'c') Down _ _)             ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , s, imgs)                = return $ (Game (moveJogador (Jogo m (Jogador (x,y) d b)) InterageCaixa) c l n1 n2, s, imgs)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)   j@((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , s, imgs)                 
   | d == Oeste && (c == (x-1,y) || let (m1,m2) = gravidade (x-1,y) m (x-1,y) in c == (m1,m2+1))                                = return $ seguinte j
   | otherwise                                                                                                                  = return $ (Game (moveJogador (Jogo m (Jogador (x,y) d b)) AndarEsquerda) c l n1 n2, s, imgs)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  j@((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , s, imgs)                 
   | d == Este && (c == (x+1,y) || let (m1,m2) = gravidade (x-1,y) m (x+1,y) in c == (m1,m2+1))                                 = return $ seguinte j
   | otherwise                                                                                                                  = return $ (Game (moveJogador (Jogo m (Jogador (x,y) d b)) AndarDireita) c l n1 n2, s, imgs)
reageEvento (EventKey (Char 'r') Down _ _)             j@((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , s, imgs)              = return $ restart j
reageEvento (EventKey (Char 's') Down _ _)             ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , (s1,s2,s3,s4) , imgs)
   | n2 >= 1 && n2 <=3                                                                                                          = do writeFile "mapa1.text" (show m)
                                                                                                                                     writeFile "jcoord1.text" (show (x,y))
                                                                                                                                     writeFile "nivel1.text" (show n2)
                                                                                                                                     writeFile "direcao1.text" (show d)
                                                                                                                                     writeFile "bool1.text" (show b)
                                                                                                                                     writeFile "coordporta1.text" (show c)
                                                                                                                                     return ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , (s1+1,s2,s3,s4) , imgs)
reageEvento (EventKey (Char 's') Down _ _)             ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , (s1,s2,s3,s4) , imgs)
   | n2 >= 4 && n2 <=6                                                                                                          = do writeFile "mapa2.text" (show m)
                                                                                                                                     writeFile "jcoord2.text" (show (x,y))
                                                                                                                                     writeFile "nivel2.text" (show n2)
                                                                                                                                     writeFile "direcao2.text" (show d)
                                                                                                                                     writeFile "bool2.text" (show b)
                                                                                                                                     writeFile "coordporta2.text" (show c)
                                                                                                                                     return ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , (s1,s2+1,s3,s4) , imgs)
reageEvento (EventKey (Char 's') Down _ _)             ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , (s1,s2,s3,s4) , imgs)
   | n2 >= 7 && n2 <=9                                                                                                          = do writeFile "mapa3.text" (show m)
                                                                                                                                     writeFile "jcoord3.text" (show (x,y))
                                                                                                                                     writeFile "nivel3.text" (show n2)
                                                                                                                                     writeFile "direcao3.text" (show d)
                                                                                                                                     writeFile "bool3.text" (show b)
                                                                                                                                     writeFile "coordporta3.text" (show c)
                                                                                                                                     return ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , (s1,s2,s3+1,s4) , imgs)
reageEvento (EventKey (Char '1') Down _ _)             ((Game (Jogo m (Jogador (x,y) d b)) c l n1 n2) , s, imgs)                = return $ (Menu2 "Niveis1", s, imgs)
-- Mensagem
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _)  ((Mensagem "Fim"), s, imgs)                                              = return $ (Menu2 "Niveis1", s, imgs)
-- ignora qualquer outro evento
reageEvento _ s                                                                                                                 = return s



-- | função que através de um fr e gera ou não um novo estado
reageTempo :: Float -> Mundo -> IO Mundo
-- Menu
reageTempo n (Menu "Jogar1", s, imgs)                      = return $ (Menu "Jogar2", s, imgs)
reageTempo n (Menu "Jogar2", s, imgs)                      = return $ (Menu "Jogar1", s, imgs)
reageTempo n (Menu "Sair1", s, imgs)                       = return $ (Menu "Sair2", s, imgs)
reageTempo n (Menu "Sair2", s, imgs)                       = return $ (Menu "Sair1", s, imgs)
reageTempo n (Menu "Instrucoes1", s, imgs)                 = return $ (Menu "Instrucoes2", s, imgs)
reageTempo n (Menu "Instrucoes2", s, imgs)                 = return $ (Menu "Instrucoes1", s, imgs)
-- Ins
reageTempo n (Ins "instrucoes1", s, imgs)                  = return $ (Ins "instrucoes2", s, imgs)
reageTempo n (Ins "instrucoes2", s, imgs)                  = return $ (Ins "instrucoes3", s, imgs)
reageTempo n (Ins "instrucoes3", s, imgs)                  = return $ (Ins "instrucoes4", s, imgs)
reageTempo n (Ins "instrucoes4", s, imgs)                  = return $ (Ins "instrucoes1", s, imgs)
-- Menu2
reageTempo n (Menu2 "Niveis1", s, imgs)                    = return $ (Menu2 "Niveis2", s, imgs)
reageTempo n (Menu2 "Niveis2", s, imgs)                    = return $ (Menu2 "Niveis1", s, imgs)
reageTempo n (Menu2 "Contra-Relogio1", s, imgs)            = return $ (Menu2 "Contra-Relogio2", s, imgs)
reageTempo n (Menu2 "Contra-Relogio2", s, imgs)            = return $ (Menu2 "Contra-Relogio1", s, imgs)
reageTempo n (Menu2 "Seta1", s, imgs)                      = return $ (Menu2 "Seta2", s, imgs)
reageTempo n (Menu2 "Seta2", s, imgs)                      = return $ (Menu2 "Seta1", s, imgs)
-- N
reageTempo n (N "SaidaV1" n1, s, imgs)                     = return $ (N "SaidaV2" n1, s, imgs)
reageTempo n (N "SaidaV2" n1, s, imgs)                     = return $ (N "SaidaV1" n1, s, imgs)
-- J
reageTempo n (J "saidaSeta1" n1,  s, imgs)                 = return $ (J "saidaSeta2" n1,  s, imgs) 
reageTempo n (J "saidaSeta2" n1,  s, imgs)                 = return $ (J "saidaSeta1" n1,  s, imgs) 
-- CR
reageTempo n (CR (x,j,c,l,n1,n2,n3), (s1,s2,s3,s4), imgs)  = if x > 0
                                                             then return $ (CR ((x-1),j,c,l,n1,n2,n3), (s1,s2,s3,s4), imgs)
                                                             else if n3 >= s4
                                                                  then return $ (Mensagem2 "Novo Jogo" n3, (s1,s2,s3,n3), imgs)
                                                                  else return $ (Mensagem2 "Novo Jogo" n3, (s1,s2,s3,s4), imgs)
-- ignora qualquer outro evento
reageTempo _ e                                             = return e



-- | função recebe um frame
fr :: Int
fr = 1



-- | função que fornece o tamanho da janela
dm :: Display
dm = FullScreen



-- | main
main :: IO ()
main = do 
   --heróis
   p1   <- loadBMP "deadPoolO.bmp"
   p2   <- loadBMP "deadPoolE.bmp"
   p3   <- loadBMP "wolverineO.bmp"
   p4   <- loadBMP "wolverineE.bmp"
   p5   <- loadBMP "hulkO.bmp"
   p6   <- loadBMP "hulkE.bmp"
   p7   <- loadBMP "spiderO.bmp"
   p8   <- loadBMP "spiderE.bmp"
   p9   <- loadBMP "ironmanO.bmp"
   p10  <- loadBMP "ironmanE.bmp"
   p11  <- loadBMP "cAmericaO.bmp"
   p12  <- loadBMP "cAmericaE.bmp"
   p13  <- loadBMP "thorO.bmp"
   p14  <- loadBMP "thorE.bmp"
   p15  <- loadBMP "panteraO.bmp"
   p16  <- loadBMP "panteraE.bmp"
   p99  <- loadBMP "setaH.bmp"
   p100 <- loadBMP "escolhe.bmp"
   
   --caixa
   c1 <- loadBMP "box.bmp"
   
   --bloco
   b1 <- loadBMP "black.bmp"

   --porta
   s1 <- loadBMP "avengers.bmp"

   --fundo
   f1  <- loadBMP "bg1.bmp"
   f2  <- loadBMP "bg2.bmp"
   f3  <- loadBMP "bg3.bmp"
   f4  <- loadBMP "bg4.bmp"
   f5  <- loadBMP "bg5.bmp"
   f6  <- loadBMP "bg6.bmp"
   f7  <- loadBMP "bg7.bmp"
   f8  <- loadBMP "bg8.bmp"
   f9  <- loadBMP "marvel1.bmp"
   f10 <- loadBMP "venom.bmp"
   f11 <- loadBMP "infinity.bmp"
   f12 <- loadBMP "thanos.bmp"
   f13 <- loadBMP "a1.bmp"
   f14 <- loadBMP "i1.bmp"
   f15 <- loadBMP "t1.bmp"
   f16 <- loadBMP "luva.bmp"
   f17 <- loadBMP "semhand.bmp"
   f18 <- loadBMP "relogio.bmp"
   f19 <- loadBMP "end.bmp"
   f20 <- loadBMP "hammer.bmp"
   f21 <- loadBMP "shield.bmp"
   f22 <- loadBMP "face.bmp"
   f23 <- loadBMP "web.bmp"

   --palavras
   w1 <- loadBMP "jogar.bmp"
   w2 <- loadBMP "sair.bmp"
   w3 <- loadBMP "niveis.bmp"
   w4 <- loadBMP "contra.bmp"
   w5 <- loadBMP "facil.bmp"
   w6 <- loadBMP "medio.bmp"
   w7 <- loadBMP "dificil.bmp"
   w8 <- loadBMP "ae.bmp"
   w9 <- loadBMP "ad.bmp"
   w10 <- loadBMP "trepar.bmp"
   w11 <- loadBMP "interage.bmp"
   w12 <- loadBMP "r.bmp"
   w13 <- loadBMP "salvar.bmp"
   w14 <- loadBMP "1.bmp"
   w15 <- loadBMP "inst.bmp"
   w16 <- loadBMP "melhor.bmp"
   w17 <- loadBMP "tua.bmp"
   w18 <- loadBMP "novo.bmp"

   --niveis
   n1 <- loadBMP "tiro.bmp"
   n2 <- loadBMP "pendura.bmp"
   n3 <- loadBMP "saidaV.bmp"
   n4 <- loadBMP "circP.bmp"
   n5 <- loadBMP "circV.bmp"

   let estado = (estadoInicial, (0,0,0,0), Imagens [(Caixa,c1),(Bloco,b1),(Porta,s1)]
                                                   [(1.1, p1),(1.2, p2),(2.1, p3),(2.2, p4),(3.1,p5),(3.2,p6),(4.1,p7),(4.2,p8),(5.1,p9),(5.2,p10),(6.1,p11),(6.2,p12),(7.1,p13),(7.2,p14),(8.1,p15),(8.2,p16),(9.9,p99),(100,p100)]
                                                   [(1,f1),(2,f2),(3,f3),(4,f4),(5,f5),(6,f6),(7,f7),(8,f8),(9,f9),(10,f10),(11,f11),(12,f12),(13,f13),(14,f14),(15,f15),(16,f16),(17,f17),(18,f18),(19,f19),(20,f20),(21,f21),(22,f22),(23,f23)]
                                                   [("jogar",w1),("sair",w2),("niveis",w3),("contra",w4),("facil",w5),("medio",w6),("dificil",w7),("ae",w8),("ad",w9),("trepar",w10),("interage",w11),("r",w12),("salvar",w13),("1",w14),("inst",w15),("melhor",w16),("tua",w17),("novo",w18)]
                                                   [("tiro",n1),("pendura",n2),("saidaV",n3),("circP",n4),("circV",n5)]
                )
   playIO dm
          (greyN 0.4)
          fr
          estado
          desenhaEstado
          reageEvento
          reageTempo

-- MAPAS
-- Fácil
-- | Jogo 1
jogo1 = Jogo [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio],
              [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Caixa,Caixa,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
             
             (Jogador (18,6) Oeste False)

-- | Jogo 2
jogo2 = Jogo [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
              [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],
              [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
              [Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Bloco,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]

             (Jogador (9,6) Oeste False)

-- | Jogo 3
jogo3 = Jogo [[Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Vazio,Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco,Bloco,Bloco],
              [Vazio,Bloco,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Caixa,Caixa,Vazio,Bloco,Vazio,Vazio],
              [Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio],
              [Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
             
             (Jogador (13,7) Oeste False)

-- Médio
-- | Jogo 4
jogo4 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],
              [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
              [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
              [Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Bloco,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
             
             (Jogador (17,9) Oeste False)

-- | Jogo 5
jogo5 = Jogo [[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio],
              [Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio],
              [Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Bloco,Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Caixa,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
              [Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Caixa,Caixa,Caixa,Vazio,Vazio,Bloco,Bloco,Vazio,Caixa,Caixa,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
             
             (Jogador (17,9) Oeste False)

-- | Jogo 6
jogo6 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco],
              [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Caixa,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio],
              [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
             
             (Jogador (14,7) Oeste False)

--Difícil
-- | Jogo 7
jogo7 = Jogo [[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
              [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco],
              [Bloco,Caixa,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Caixa,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Porta,Vazio,Bloco,Vazio,Bloco],
              [Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco],
              [Bloco,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco],
              [Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Bloco],
              [Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
              [Bloco,Bloco,Bloco,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Caixa,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Caixa,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Caixa,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco],
              [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco,Vazio,Bloco],
              [Bloco,Caixa,Caixa,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco],
              [Bloco,Caixa,Caixa,Caixa,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Caixa,Caixa,Caixa,Caixa,Caixa,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
 
             (Jogador (13,4) Oeste False)

-- | Jogo 8
jogo8 = Jogo [[Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio],
              [Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
              [Bloco,Bloco,Bloco,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco,Caixa,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Vazio,Caixa,Bloco,Bloco,Vazio],
              [Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Caixa,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco],
              [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
              [Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Caixa,Caixa,Caixa,Bloco,Vazio],
              [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio]]
             
             (Jogador (14,11) Oeste False)

-- | Jogo 9
jogo9 = Jogo [[Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
              [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco],
              [Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Bloco],
              [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Porta,Vazio,Bloco],
              [Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco],
              [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
              [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
              [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
             
             (Jogador (20,15) Oeste False)

-- ghc Tarefa5_2021li1g056.hs
-- ./Tarefa5_2021li1g056
