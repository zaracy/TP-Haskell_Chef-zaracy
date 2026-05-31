-- ======= PARTE A =======

type Nombre = String
type Truco = Plato -> Plato
type Especialidad = Plato
type Dificultad = Int
type Peso = Int
type Ingrediente = String
type Componente = (Ingrediente, Peso)

data Plato = UnPlato {
  dificultad :: Dificultad,
  componentes :: [Componente]    -- lista de componentes
} deriving (Show, Eq)

data Participante = UnParticipante {
  nombre :: Nombre,
  trucos :: [Truco],             -- lista de trucos
  especialidad :: Especialidad
}

-- TRUCOS FAMOSOS

endulzar :: Peso -> Truco
endulzar gramos unPlato = UnPlato (dificultad unPlato) (("azucar", gramos) : componentes unPlato)

salar :: Peso -> Truco
salar gramos unPlato = UnPlato (dificultad unPlato) (("sal", gramos) : componentes unPlato)

-- darSabor agrega sal y azucar a un mismo plato
darSabor :: Peso -> Peso -> Truco
darSabor gramosDeSal gramosDeAzucar unPlato = salar gramosDeSal (endulzar gramosDeAzucar unPlato)

-- duplicarPorcion 
duplicarPorcion :: Truco
duplicarPorcion unPlato = UnPlato (dificultad unPlato) (map duplicarComponente (componentes unPlato))

duplicarComponente :: Componente -> Componente
duplicarComponente (ingrediente, peso) = (ingrediente, peso * 2) -- funcion auxiliar para duplicarPorcion


simplificar :: Truco  -- porque recibo un plato y devuelvo un otro plato
simplificar unPlato
   | dificultad unPlato > 7 && length (componentes unPlato) > 5 = 
        UnPlato 5 (filter (tieneAlMenos10Gramos) (componentes unPlato))
   | otherwise = unPlato

tieneAlMenos10Gramos :: Componente -> Bool
tieneAlMenos10Gramos (_, peso) = peso >= 10

-- DATOS DE LOS PLATOS

esVegano :: Plato -> Bool
esVegano unPlato = not ( any esIngredienteNoVegano (ingredientesDelPlato unPlato) )

ingredientesDelPlato :: Plato -> [Ingrediente]

ingredientesDelPlato unPlato = map ingredienteDelComponente (componentes unPlato)

ingredienteDelComponente :: Componente -> Ingrediente
ingredienteDelComponente (ingrediente, _) = ingrediente

esIngredienteNoVegano :: Ingrediente -> Bool
esIngredienteNoVegano ingrediente = ingrediente == "carne" || ingrediente == "huevos" || ingrediente == "lacteos"

-- esSinTacc
esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (any tieneHarina (ingredientesDelPlato unPlato))

tieneHarina :: Ingrediente -> Bool
tieneHarina ingrediente = ingrediente == "harina"

-- esComplejo
esComplejo :: Plato -> Bool
esComplejo unPlato = length (componentes unPlato) > 5 && dificultad unPlato > 7

-- noAptoHipertension
noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = cantidadDeSal unPlato > 2

cantidadDeSal :: Plato -> Peso
cantidadDeSal unPlato = sum ( map pesoDelComponente (filter esSal (componentes unPlato) ))

pesoDelComponente :: Componente -> Peso
pesoDelComponente (_, peso) = peso

esSal :: Componente -> Bool
esSal (ingrediente, _) = ingrediente == "sal"


-- ======= PARTE B =======

pepe :: Participante
pepe = UnParticipante {
  nombre = "Pepe Ronccino",
  trucos = [darSabor 2 5, simplificar, duplicarPorcion],
  especialidad = platoDePepe
}

platoDePepe :: Plato
platoDePepe = UnPlato {
  dificultad = 8,   -- mayor a 7, y tiene 6 (>5) componentes entonces es complejo
  componentes = [("carne", 100), ("sal", 3), ("azucar", 5), ("harina", 50), ("huevos", 3), ("lacteos", 4)]
}



-- ======= PARTE C =======

-- FUNCIONALIDADES
--cocinar
cocinar :: Participante -> Plato
cocinar unParticipante = foldl aplicarTruco (especialidad unParticipante) (trucos unParticipante)

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

-- esMejorQue
esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = dificultad plato1 > dificultad plato2 && pesoTotalDelPlato plato1 < pesoTotalDelPlato plato2

pesoTotalDelPlato :: Plato -> Peso
pesoTotalDelPlato unPlato = sum (map pesoDelComponente (componentes unPlato))

-- participanteEstrella
participanteEstrella :: [Participante] -> Participante
participanteEstrella participantes = foldl1 mejorParticipante participantes

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante participanteGanador participanteNuevo
  | esMejorQue (cocinar participanteGanador) (cocinar participanteNuevo) = participanteGanador
  | otherwise = participanteNuevo


-- ======= PARTE D =======

{- Para finalizar, vamos a modelar el plato definitivo, el platinum. Este plato tiene de especial que tiene infinitos
componentes misteriosos con cantidades incrementales y dificultad 10.
-}
