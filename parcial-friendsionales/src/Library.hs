module Library where
import PdePreludat

type Yulius = Number
type Alegronio = Number
type Nerviofrinas = Number
type Tarea = Persona -> Persona
type Estres = Number

data Persona = UnaPersona{
    nombre :: String,
    edad :: Number,
    energia :: Yulius,
    alegria :: Alegronio,
    ansiedad :: Nerviofrinas,
    tareas :: [Tarea]
}deriving (Show)

obtenerEnergia :: Tarea
obtenerEnergia persona 
        | alegria persona > ansiedad persona = persona{energia = min 340 (alegria persona *2)}
        | alegria persona < ansiedad persona && edad persona < 40 = persona{energia= 300 - (estres persona)}
        | otherwise = persona{energia=energia persona +10}

estres :: Persona -> Estres
estres persona = ansiedad persona * ((div (length (tareas persona)) 5) * 1.5)

--punto 1 modelo de una persona

-- nombre edad energia alegria ansiedad [tareas] 
magui = UnaPersona "Magui" 20 15 20 15 []
juan = UnaPersona "Juan" 15 20 100 250 []
cecilia = UnaPersona "Cecilia" 35 0 90 65 []

--punto 2

cuantoDueleVerLasBuenas :: [Persona] -> Bool
cuantoDueleVerLasBuenas jovatos = all esVital jovatos

esVital :: Persona -> Bool
esVital persona = energia persona > 100

nivelTotalDeAnsiedad :: [Persona] -> Nerviofrinas
nivelTotalDeAnsiedad jovatos = foldr (+) 0 (map ansiedad jovatos)

losMasCriticados :: (Persona -> Bool) -> [Persona] -> [Persona]
losMasCriticados criterio personas = take 2 (filter criterio personas)

mas50Ansiedad :: Persona -> Bool
mas50Ansiedad persona = ansiedad persona > 50

energiaPar :: Persona -> Bool
energiaPar persona = div (energia persona) 2 == 0

--punto 3 tareas

realizarTarea :: Persona -> Tarea -> Persona 
realizarTarea persona tarea = (tarea persona) {ansiedad = max 0 (ansiedad (tarea persona) - 10)}

codearUnProyectoNuevo :: Tarea 
codearUnProyectoNuevo persona = persona{ansiedad = ansiedad persona +50, alegria = alegria persona +110}

type CantDeTramites = Number

hacerTramitesEnAfip :: CantDeTramites -> Tarea
hacerTramitesEnAfip cantDeTramites persona = persona{ansiedad= max 300 (ansiedad persona * cantDeTramites)}

type KilometrosRecorridos = Number

andarEnBici :: KilometrosRecorridos -> Tarea
andarEnBici kilometros persona = persona{ansiedad = 0, alegria= alegria persona + 50*kilometros}

escucharMusica :: Tarea
escucharMusica persona = persona{ansiedad = ansiedad persona -10}

--punto 4

energiaResultante :: Persona -> [Tarea] -> Yulius
energiaResultante persona tareas = energia (foldr ($) persona tareas)

--punto 5

hiceLoQuePude :: Persona -> [Tarea] -> Persona
hiceLoQuePude persona [] = persona
hiceLoQuePude persona (tarea:tareas)
        | (mas100Energia . realizarTarea persona) tarea = hiceLoQuePude persona tareas
        | otherwise = persona

mas100Energia :: Persona -> Bool
mas100Energia persona = energia persona > 100