module Library where
import PdePreludat

data Turista = UnTurista {
    cansancio :: Number,
    stress :: Number,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show,Eq)

data Marea = Tranquila | Moderada | Fuerte

-------- Tipos de Datos --------

type Excursion = Turista -> Turista
type Tour = [Excursion]

-------- Ejemplos de Turistas --------

ana = UnTurista {cansancio = 0 , stress = 20, viajaSolo = False, idiomas = ["español"]}
beto = UnTurista {cansancio = 15, stress = 15, viajaSolo = True, idiomas = ["aleman"]}
cathi = UnTurista {cansancio = 15, stress = 15, viajaSolo = True, idiomas = ["aleman", "catalan"]}

------------------------------------------------------ Punto 1 ------------------------------------------------------

cambiarStress :: Number -> Turista -> Turista
cambiarStress x turista = turista {stress = stress turista + x}

cambiarStressPorcentual :: Number -> Turista -> Turista
cambiarStressPorcentual porciento turista = cambiarStress (div (porciento * stress turista) 100) turista

cambiarCansancio :: Number -> Turista -> Turista
cambiarCansancio x turista = turista {cansancio = cansancio turista + x}

aprenderIdioma :: String -> Turista -> Turista
aprenderIdioma idioma turista = turista {idiomas = idioma : idiomas turista}

acompañado :: Turista -> Turista
acompañado turista = turista {viajaSolo = False}

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

------------------------------------------------ Lista de Excursiones -----------------------------------------------

irPlaya :: Excursion
irPlaya turista | viajaSolo turista = cambiarCansancio (-5) turista
                | otherwise = cambiarStress (-1) turista

apreciarAlgunElemento ::  String -> Excursion
apreciarAlgunElemento elemento = cambiarStress (-length elemento)

salirHablarIdioma :: String -> Excursion
salirHablarIdioma idioma = acompañado . (aprenderIdioma idioma)

caminarCiertosMinutos :: Number -> Excursion
caminarCiertosMinutos tiempo = cambiarStress (- intensidad tiempo) . cambiarCansancio (intensidad tiempo)
intensidad :: Number -> Number
intensidad tiempo = div tiempo 4

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila = caminarCiertosMinutos 10 . apreciarAlgunElemento "mar" . salirHablarIdioma "aleman"
paseoEnBarco Moderada = id
paseoEnBarco Fuerte = cambiarStress 6 . cambiarCansancio 10

------------------------------------------------------ Punto 2 ------------------------------------------------------

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion = cambiarStressPorcentual (-10) . excursion

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerUnaExcursion excursion turista) turista

excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista excursiones = filter (esDesestresante turista) excursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (>=3) . deltaExcursionSegun stress turista

------------------------------------------------------ Punto 3 ------------------------------------------------------

------------------------------------------------- Listado de Tours --------------------------------------------------

completo :: Tour
completo = [salirHablarIdioma "melmacquiano",caminarCiertosMinutos 40,apreciarAlgunElemento "cascada",irPlaya,caminarCiertosMinutos 20]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminarCiertosMinutos 120]

islaVecina :: Marea -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionEnIslaVecina mareaVecina, paseoEnBarco mareaVecina]

excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina Fuerte = apreciarAlgunElemento "lago"
excursionEnIslaVecina _  = irPlaya

-------- Punto A --------

turistaHaceTour :: Turista -> Tour -> Turista
turistaHaceTour turista tour = foldl (flip hacerUnaExcursion) (cambiarStress (length tour) turista) tour

-------- Punto B --------

propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompañado turista) . (excursionesDesestresantes turista)

dejaAcompañado :: Turista -> Excursion -> Bool
dejaAcompañado turista = not . (viajaSolo) . (flip hacerUnaExcursion turista)

-------- Punto C --------

efectividadTour :: Tour -> [Turista] -> Number
efectividadTour tour = sum . map (espiritualidad tour) . filter (flip esConvincente tour)

espiritualidad :: Tour -> Turista -> Number
espiritualidad tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Number
deltaRutina tour turista = deltaSegun nivelDeRutina (turistaHaceTour turista tour) turista

nivelDeRutina :: Turista -> Number
nivelDeRutina turista = cansancio turista + stress turista

------------------------------------------------------ Punto 4 ------------------------------------------------------

-------- Punto A --------

playasEternas :: Tour
playasEternas = repeat irPlaya

-------- Punto B --------

-- Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
-- Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.

-------- Punto C --------

-- No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.