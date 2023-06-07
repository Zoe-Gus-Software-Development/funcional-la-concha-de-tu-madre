module Library where
import PdePreludat

type Gema = Personaje -> Personaje
type Habilidad = String
type Planeta = String
type Edad = Int

data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema]
}

data Personaje = Personaje{
    edad :: Edad,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: Planeta
}

data Universo = Universo{
    habitantes :: [Personaje]
}

{-modelos

magui :: Personaje
magui = Personaje{
    edad = 20,
    energia = 100,
    habilidades = ["jugar a la compu", "tocar el piano"],
    nombre = "Super Magui",
    planeta = "Planeta Magui"
}

tin :: Personaje
tin = Personaje{
    edad = 19,
    energia = 150,
    habilidades = ["ayudar en pdep", "tocar el violin", "pelarse"],
    nombre = "Super Tin",
    planeta = "Planeta Magui"
}

gusuniverso :: Universo
gusuniverso = gusuniverso{
    habitantes = [magui, tin]
}
-}


--punto 2

aptoParaPendex :: Universo -> Bool
aptoParaPendex = ( any (< 45) . map edad . habitantes) 

energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso = ( foldr (+) 0 . map energia . filter masDeUnaHabilidad . habitantes)

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad = ((>1) . length . habilidades)

--las gemas

laMente :: Int -> Gema
laMente debilitacion personaje = personaje {energia = energia personaje - debilitacion} 

elAlma :: Habilidad -> Gema
elAlma habilidadADeletear personaje = personaje {energia = energia personaje - 10, habilidades = filter (/= habilidadADeletear) (habilidades personaje)} 

elEspacio :: Planeta -> Gema
elEspacio planetaATransportar personaje = personaje {energia = energia personaje - 20, planeta = planetaATransportar}

elPoder :: Gema 
elPoder personaje = personaje {energia = 0, habilidades = modificarHabilidades (habilidades personaje)}

menosDeDosHabilidades :: [Habilidad] -> Bool
menosDeDosHabilidades = ((<=1) . length)

modificarHabilidades :: [Habilidad] -> [Habilidad]
modificarHabilidades habilidades
        | menosDeDosHabilidades habilidades = []
        | otherwise = habilidades

elTiempo :: Gema
elTiempo personaje = personaje {energia = energia personaje - 50, edad = modificarEdad (edad personaje)}

modificarEdad :: Edad -> Edad
modificarEdad edad
        | edad > 36 = div edad 2
        | otherwise = 18

gemaLoca :: Gema -> Gema
gemaLoca gema personaje = (gema . gema) personaje

--punto 4 el guantelete de goma

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete{
    material = "Goma",
    gemas = [elTiempo, elAlma "usar Mjolnir", gemaLoca (elAlma "programación en Haskell")]
}

--punto 5 utilizar

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldl ($) personaje gemas

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = undefined

