module Library where
import PdePreludat

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--punto 1

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{velocidad=10, precision=(precisionJugador habilidad)*2, altura=0}

madera :: Palo
madera habilidad = UnTiro{velocidad=100, precision=div (precisionJugador habilidad) 2, altura=5}

hierros :: Number -> Palo
hierros n habilidad = UnTiro{velocidad=(fuerzaJugador habilidad)*n, precision=div (precisionJugador habilidad) n, altura=max 0 (n-3)}

type Palos = [Palo]

palos :: Palos
palos = (putter: madera: generadorPalosHierro [1..10])

generadorPalosHierro :: [Number] -> [Palo]
generadorPalosHierro (n:ns) = (hierros n:generadorPalosHierro ns)
generadorPalosHierro [] = []

--punto 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo . habilidad) jugador

--punto 3

type Obstaculo = Tiro -> Tiro
type LargoLaguna = Number

tunel :: Obstaculo
tunel tiro
    | ( (>90) . precision) tiro = UnTiro{velocidad=velocidad tiro*2, precision=100, altura=0}
    | otherwise = tiroSeDetiene tiro

laguna :: LargoLaguna -> Obstaculo
laguna largoLaguna tiro 
    | velocidad tiro > 80 && between 1 5 (altura tiro) = UnTiro{velocidad=velocidad tiro, precision=precision tiro, altura=div (altura tiro) largoLaguna}
    | otherwise = tiroSeDetiene tiro

hoyo :: Obstaculo 
hoyo tiro 
    | between 5 20 (velocidad tiro) && precision tiro > 95 = tiroSeDetiene tiro
    | otherwise = tiro

tiroSeDetiene :: Obstaculo
tiroSeDetiene tiro = UnTiro{velocidad=0, precision=0, altura=0}

--punto 4a

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculo jugador obstaculo) palos

superaObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
superaObstaculo  jugador obstaculo palo = obstaculo (golpe jugador palo) /= UnTiro{velocidad=0, precision=0, altura=0}

--punto 4b

cuantosObstaculos :: [Obstaculo] -> Tiro -> Number
cuantosObstaculos obstaculos tiro = undefined