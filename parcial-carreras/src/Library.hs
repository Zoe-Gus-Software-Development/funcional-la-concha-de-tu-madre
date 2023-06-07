module Library where
import PdePreludat

type Color = String
type Tiempo = Number
type Velocidad = Number

data Auto = UnAuto{
    color :: Color,
    velocidad :: Velocidad,
    distancia :: Number
}

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


--punto 1

estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = abs (distancia auto1 - distancia auto2) > 10 && sonAutosDistintos auto1 auto2

sonAutosDistintos :: Auto -> Auto -> Bool
sonAutosDistintos auto1 auto2 = color auto1 /= color auto2

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo auto autos = (length . filter (estanCerca auto)) autos == 0

puesto :: Auto -> [Auto] -> Number
puesto auto autos = length (filter (vaPorDelante auto) autos) + 1

vaPorDelante :: Auto -> Auto -> Bool  -- veo si el segundo auto recibido va por delante del primero recibido    
vaPorDelante auto1 auto2 = distancia auto2 > distancia auto1

--punto 2

correrPorTiempo :: Auto -> Tiempo -> Velocidad -> Auto
correrPorTiempo auto tiempo velocidad = auto{distancia = distancia auto + tiempo*velocidad}

                    --modificador ???????

bajarVelocidad :: Velocidad -> Auto -> Auto
bajarVelocidad velocidadABajar auto = auto{velocidad = max (velocidad auto - velocidadABajar) 0}

--punto 3

terremoto :: Auto -> [Auto] -> [Auto]
terremoto auto autos = afectarALosQueCumplen (estanCerca auto) (bajarVelocidad 50) autos

miguelitos :: Auto -> Velocidad -> [Auto] -> [Auto]
miguelitos auto velocidadABajar autos = afectarALosQueCumplen (vaPorDelante auto) (bajarVelocidad velocidadABajar) autos

jetPack :: Auto -> Tiempo -> Auto
jetPack auto tiempo = correrPorTiempo auto tiempo (velocidad auto *2)

--punto 4


