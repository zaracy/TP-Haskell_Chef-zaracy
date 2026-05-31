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

-- Trucos famosos

endulzar :: Peso -> Truco
endulzar gramos unPlato = UnPlato (dificultad unPlato) (("azucar", gramos) : componentes unPlato)

salar :: Peso -> Truco
salar gramos unPlato = UnPlato (dificultad unPlato) (("sal", gramos) : componentes unPlato)

-- darSabor agrega sal y azucar a un mismo plato

darSabor :: Peso -> Peso -> Truco
darSabor gramosDeSal gramosDeAzucar unPlato = salar gramosDeSal (endulzar gramosDeAzucar unPlato)

-- duplicarPorcion duplica la cantidad de cada componente en un plato
duplicarPorcion :: Truco
duplicarPorcion unPlato = UnPlato (dificultad unPlato) (map duplicarComponente (componentes unPlato))

duplicarComponente :: Componente -> Componente
duplicarComponente (ingrediente, peso) = (ingrediente, peso * 2) -- funcion auxiliar para duplicarPorcion

{-simplificar: sin un plato tiene mas de 5 componentes y una dificultad mayor a 7, lo vamos a simplificar, 
sino lo dejamos igual. Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes
de los que hayamos agregado menos de 10 gramos -}
simplificar :: Truco  -- porque recibo un plato y devuelvo un otro plato
simplificar unPlato
   | dificultad unPlato > 7 && length (componentes unPlato) > 5 = 
        UnPlato 5 (filter (tieneAlMenos10Gramos) (componentes unPlato))
   | otherwise = unPlato

tieneAlMenos10Gramos :: Componente -> Bool
tieneAlMenos10Gramos (_, peso) = peso >= 10

-- DATOS DE LOS PLATOS
-- esVegano: si un plato no tiene carne, huevos o alimentos lacteos
esVegano :: Plato -> Bool
esVegano unPlato = not ( any esIngredienteNoVegano (ingredientesDelPlato unPlato) )

ingredientesDelPlato :: Plato -> [Ingrediente]
--ingredientesDelPlato unPlato = map fst (componentes unPlato) -- no quiero usar fst
ingredientesDelPlato unPlato = map ingredienteDelComponente (componentes unPlato)

ingredienteDelComponente :: Componente -> Ingrediente
ingredienteDelComponente (ingrediente, _) = ingrediente

esIngredienteNoVegano :: Ingrediente -> Bool
esIngredienteNoVegano ingrediente = ingrediente == "carne" || ingrediente == "huevos" || ingrediente == "lacteos"

-- esSinTacc: si no tiene harina
esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (any tieneHarina (ingredientesDelPlato unPlato))

tieneHarina :: Ingrediente -> Bool
tieneHarina ingrediente = ingrediente == "harina"

-- esComplejo : cuando tiene mas de 5 componentes y una dificultad mayor a 7
esComplejo :: Plato -> Bool
esComplejo componentes UnPlato = length (componentes unPlato) > 5 && dificultad unPlato > 7

-- noAptoHipertension : si tiene mas de 2 gramos de sal. coviene crear mas funciones auxiliares
noAptoHipertension :: Plato -> Bool
noAptoHiperTension unPlato = cantidadDeSal unPlato > 2

cantidadeDeSal :: Plato -> Peso
cantidadDeSal unPlato = sum ( map pesoDeSal (componentes unPlato) )

pesoDeSal :: Componente -> Peso
pesoDeSal (_, peso) = peso

esSal :: Componente -> Bool
esSal (ingrediente, _) = ingrediente == "sal"


-- ======= PARTE B =======
{-
en la prueba piloto del programa va a estar participando Pepe Ronccino, quien tiene unos trucazos bajo 
la manga, como darle sabor a un plato con 2 gramos de sal y 5 gramos de azucar, simplificarlo y 
duplicar su porcion. Su especialidad es un plano complejo y no apto para personas hipertensas. Modelar
a pepe y a su plato
-}
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

{-
pepe = UnParticipante {
  nombre = "Pepe Ronccino",
  trucos = [darSabor 2 5, simplificar, duplicarPorcion],
  especialidad = UnPlato {
    dificultad = 8,   -- mayor a 7, entonces es complejo (tiene mas de 5 componentes)
    componentes = [("carne", 100), ("sal", 3), ("azucar", 5), ("harina", 50), ("huevos", 3), ("lacteos", 4)]
  }
}  creo que tambien sirve hacerlo asi, pero separar ambos record syntax me parece mas claro -}

-- ======= PARTE C =======

-- FUNCIONALIDADES
--cocinar: vemos como queda in plato de un participante luego de aplicar todos sus trucos a su especialidad
cocinar :: Participante -> Plato
