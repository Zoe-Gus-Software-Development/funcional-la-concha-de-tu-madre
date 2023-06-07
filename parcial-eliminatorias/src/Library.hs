module Library where
import PdePreludat

----------------------------------------------
-- Código base provisto en el enunciado
----------------------------------------------

maximoSegun :: Ord a => (b -> a) -> [b] -> b
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Descomentar luego de definir los tipos Rol y Participante
-- de acuerdo al modelo elegido en el punto 1:


data Desafio = Desafio {
    rolesDisponibles :: [Rol],
    pruebaASuperar :: Participante -> Bool
  }


----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
-- abajo, indicando a qué punto pertenecen
----------------------------------------------

--punto 1a

data Participante = UnParticipante{
    nombre :: String,
    experiencia :: Number,
    inteligencia :: Number,
    destrezaFisica :: Number,
    rol :: Rol
}deriving (Show, Eq)

data Arma = UnArma{
    valorCombate :: Number,
    experienciaMinima :: Number
}

type Potencia = Number

calculoPotenciaArma :: Arma -> Participante -> Potencia
calculoPotenciaArma arma participante
        | experiencia participante >= experienciaMinima arma = valorCombate arma
        | otherwise = valorCombate arma /2

type Aptitud = Number
type Rol = Participante -> Aptitud

calculoAptitud :: Participante -> Rol -> Aptitud
calculoAptitud participante rol = rol participante

indeterminado :: Rol
indeterminado participante = inteligencia participante + destrezaFisica participante

soporte :: Rol
soporte participante = (inteligencia participante) *7 + experiencia participante

primeraLinea :: Arma -> Rol
primeraLinea arma participante = (destrezaFisica participante + calculoPotenciaArma arma participante) *experiencia participante /100 

--punto 1b

magui = UnParticipante "Magui" 1000 20 12 indeterminado

--punto 1c

type Poder = Number

calculoPoder :: Participante -> Poder
calculoPoder participante = experiencia participante * calculoAptitud participante (rol participante) 

--punto 2a

rolMasApto :: Participante -> [Rol] -> Rol
rolMasApto participante [rol] = rol
rolMasApto participante (rol:roles)
        | calculoAptitud participante rol > calculoAptitud participante (head roles) = rolMasApto participante (rol:tail roles)
        | otherwise = rolMasApto participante roles

elegirRolMasApto :: Participante -> Participante
elegirRolMasApto participante = participante{rol= rolMasApto participante roles}

--punto 2b

armaPrueba = UnArma 20 750

roles :: [Rol]
roles = [indeterminado, soporte, primeraLinea armaPrueba]

consultaPoder :: Participante -> Poder
consultaPoder participante = (calculoPoder . elegirRolMasApto) participante

--punto 2c

maestroDeArmas :: [Arma] -> Rol
maestroDeArmas armas participante = sum (map (flip calculoPotenciaArma participante) (take 3 (filter (usoApropiadoArma participante) armas)))

usoApropiadoArma :: Participante -> Arma -> Bool
usoApropiadoArma  participante arma = calculoPotenciaArma arma participante == valorCombate arma 

--punto 3a

estaParticipando :: [Participante] -> Participante -> Bool
estaParticipando participantes participante = elem (nombre participante) (map nombre participantes)

--punto 3b


