module Library where
import PdePreludat

type Edad = Number
type Nombre = String
type Felicidonios = Number
type Suenio = String

data Persona = UnaPersona {
    edad :: Edad,
    nombre :: Nombre,
    felicidonios :: Felicidonios,
    suenios :: [Suenio]
} deriving Show

--Coeficiente de satisfaccion
coeficienteSatisfaccion :: Persona -> Number
coeficienteSatisfaccion persona
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios persona <= 100 && felicidonios persona > 50 = felicidonios persona * cantidadSuenios persona
    | otherwise = 40

cantidadSuenios :: Persona -> Number
cantidadSuenios persona = length (suenios persona)


--Nombre largo
tieneNombreLargo :: Persona -> Bool
tieneNombreLargo persona = longitudNombre persona > 10

longitudNombre :: Persona -> Number
longitudNombre persona = length (nombre persona)


--Persona suertuda
esSuertuda :: Persona -> Bool
esSuertuda persona = even (coeficienteSatisfaccion persona * 3)


--Nombre lindo
tieneNombreLindo :: Persona -> Bool
tieneNombreLindo persona = primeraLetraDelNombre persona == 'L'

primeraLetraDelNombre :: Persona -> Char
primeraLetraDelNombre persona = head (nombre persona)


--Cumplir sueños
cumplirSuenios :: Persona -> Persona
cumplirSuenios persona = quedarseSinSuenios (ganarFelicidoniosPorCumplirSuenios persona)

ganarFelicidoniosPorCumplirSuenios :: Persona -> Persona
ganarFelicidoniosPorCumplirSuenios persona = ganarFelicidonios (felicidoniosAlCumplirSuenios persona) persona

felicidoniosAlCumplirSuenios :: Persona -> Felicidonios
felicidoniosAlCumplirSuenios persona = cantidadSuenios persona * coeficienteSatisfaccion persona

ganarFelicidonios :: Felicidonios -> Persona -> Persona
ganarFelicidonios felicidoniosAGanar persona = persona{felicidonios = felicidonios persona + felicidoniosAGanar}

quedarseSinSuenios :: Persona -> Persona
quedarseSinSuenios persona = persona {suenios = []}


--Fuente de los deseos
tirarMoneda :: Persona -> Persona
tirarMoneda persona = cumplirDeseosSegunFuente (ganarFelicidonios 10 persona)

cumplirDeseosSegunFuente :: Persona -> Persona
cumplirDeseosSegunFuente persona
    | esSuertuda persona = cumplirSuenios persona
    | otherwise = persona 