Tree Stock = [(String, Int)]

consolidar :: [Stock] -> Stock
consolidar [] = (,)
consolidar (stock:stocks) = combinarStock stock (consolidar stocks)

combinarStock :: Stock -> Stock -> Stock
combinarStock (stock1, int1) (stock2, int2) = (stock1, int1+int2)
--al ser una tupla el stock no se repite por eso pongo stock 1 solamente



[(arroz,50),(arroz,60)] = (arroz,110)


--
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs  

--
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = (n+1) : sucesores ns

--
conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (b:bs) = b && conjuncion bs

--
disyuncion :: [Bool] -> Bool
disyuncion [] = True
disyuncion (b:bs) = b || disyuncion bs

--
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

--
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (x==e) || pertenece e xs 

--
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = unoSi (e==x) + apariciones e xs

--
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []
losMenoresA n (x:xs) = if x<n 
                        then x : losMenoresA n xs
                        else losMenoresA n xs

--
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n 
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss

--hacer
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = 
agregarAlFinal (x:xs) e = 

--hacer
reversa :: [a] -> [a]
reversa [] = 
reversa (x:xs) =

--hacer
zipMaximos :: [Int] -> [Int] -> [Int]

--
elMinimo :: Ord a => [a] -> a
elMinimo [] = error"lista vacía no tiene mínimo"
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs
                        then x 
                        else elMinimo xs

--
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

--
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 

--
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

--
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if edad p > n 
                    then p : mayoresA n ps
                    else mayoresA n ps 

edad :: Persona -> Int
edad (P _ e) = e

--hacer
promedioEdad :: [Persona] -> Int

--
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error"lista vacía"
elMasViejo (p:ps) = if edad p > edad (elMasViejo ps)
                        then p
                        else elMasViejo ps

--
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pokemones) = longitud pokemones

--
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador _ pokemones) = longitud (losQueSonDeTipo tipo pokemones)

losQueSonDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
losQueSonDeTipo t [] = []
losQueSonDeTipo t (pok:poks) = if sonMismoTipo t (tipo pok) 
                                then pok : losQueSonDeTipo t poks
                                else losQueSonDeTipo t poks
                                
sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Planta Planta = True
sonMismoTipo Fuego Fuego = True 
sonMismoTipo _ _ = False

--hacer
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = 

--hacer
esMaestroPokemon :: Entrenador -> Bool

--hacer
proyectos :: Empresa -> [Proyecto]

--hacer
losDevSenior :: Empresa -> [Proyecto] -> Int

--hacer
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int

--hacer
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]

--
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro Cofre objs camino = poseeTesoro objs || hayTesoro camino  
hayTesoro Nada camino = hayTesoro camino

poseeTesoro :: [Objeto] -> Bool
poseeTesoro [] = False
poseeTesoro (obj:objs) = esTesoro obj || poseeTesoro objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro Cofre objs camino = if poseeTesoro objs 
                                        then 0
                                        else 1 + pasosHastaTesoro camino
pasosHastaTesoro Nada camino = 1 + pasosHastaTesoro camino

-- hacer
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n 

-- hacer
alMenosNTesoros :: Int -> Camino -> Bool


--
unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

--
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

--
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ing:ings) = (Capa ing (armarPizza ings)) 

--
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing p) = if esJamon ing 
                            then (sacarJamon p)
                            else (Capa ing (sacarJamon p)) 

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

--
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = esSoloSalsaOQueso ing && (tieneSoloSalsaYQueso p)

esSoloSalsaOQueso :: Ingrediente -> Bool
esSoloSalsaOQueso Salsa = True
esSoloSalsaOQueso Queso = True
esSoloSalsaOQueso _ = False

--
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = (Capa (aceitunasDuplicadas ing) (duplicarAceitunas p))

aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas n) = (Aceitunas (n*2))
aceitunasDuplicadas ing = ing

--
cantidadDeCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantidadDeCapasPorPizza [] = []
cantidadDeCapasPorPizza (pz:pzs) = capasYCantidadDe pz : cantidadDeCapasPorPizza pzs

capasYCantidadDe :: Pizza -> (Int, Pizza)
capasYCantidadDe p = (cantidadDeCapas p, p)

--
hayTesoro :: Mapa -> Bool
hayTesoro Fin c = tieneTesoro c
hayTesoro Bifurcacion c m1 m2 = tieneTesoro c || (hayTesoro m1) || (hayTesoro m2)  
 
tieneTesoro :: Cofre -> Bool
tieneTesoro Cofre [] = False
tieneTesoro Cofre (obj:objs) = esTesoro obj || tieneTesoro objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

-- hacer
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] _ = False
hayTesoroEn [] Fin c = tieneTesoro c
hayTesoroEn (d:ds) Fin c = tieneTesoro c
hayTesoroEn (d:ds) Bifurcacion c m1 m2 =  if esIzq d 
                                            then hayTesoroEn ds m1 
                                            else hayTesoroEn ds m2

--hacer
caminoAlTesoro :: Mapa -> [Dir]

--hacer
caminoDeLaMasLarga :: Mapa -> [Dir]

--hacer
tesorosPorNivel :: Mapa -> [[Objeto]]

--hacer
todosLosCaminos :: Mapa -> [[Dir]]

--
sectores :: Nave -> [SectorId]
sectores (N EmptyT) = []
sectores (N (NodeT (S sectorId _ _) t1 t2)) = sectorId : sectores (N t1) ++ sectores (N t2)

--
poderDePropulsion :: Nave -> Int
poderDePropulsion (N EmptyT) = 0
poderDePropulsion (N (NodeT (S _ componentes _) t1 t2)) = poderDeComponentes componentes + poderDePropulsion (N t1) + poderDePropulsion (N t2)

poderDeComponentes :: [Componente] -> Int
poderDeComponentes [] = 0
poderDeComponentes (c:cs) = poder c + poderDeComponentes cs

poder :: Componente -> Int
poder (Motor n) = n 
poder _ = 0

--
barriles :: Nave -> [Barril]
barriles (N EmptyT) = []
barriles (N (NodeT (S _ componentes _) t1 t2)) = barrilesDeComponentes componentes ++ barriles (N t1) ++ barriles (N t2)

barrilesDeComponentes :: [Componente] -> [Barril]
barrilesDeComponentes [] = []
barrilesDeComponentes (c:cs) = barrilesDe c : barrilesDeComponentes cs

barrilesDe :: Componente -> [Barril]
barrilesDe (Almacen b) = b
barrilesDe _ = []

--mal
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector [] s nave = nave
agregarASector (c:cs) s nave = agregarComponenteASector c s (agregarASector cs s nave)

agregarComponenteASector :: Componente -> SectorId -> Nave -> Nave
agregarComponenteASector c s (N EmptyT) = EmptyT
agregarComponenteASector c s (N (NodeT (S sector componentes _) t1 t2)) = if esElMismoSector s sector then (agregarComponenteASector c s t1) (agregarComponenteASector c s t2)

--
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t [] nave = nave
asignarTripulanteA t (sector:sectores) nave = asignarTripulanteASector t sector (asignarTripulanteA t sectores nave)

asignarTripulanteASector :: Tripulante -> SectorId -> Nave -> Nave 
asignarTripulanteASector t s (N (EmptyT)) = 
asignarTripulanteASector t s (N (NodeT (S sector _ tripulantes) t1 t2)) = if s == sector then (asignarTripulanteASector t s (N t1)) (asignarTripulanteASector (N t2))

--está mal
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N EmptyT) = []
sectoresAsignados t N (NodeT (S sector _ tripulantes) t1 t2) = if pertenece t tripulantes 
                                                                then sectorId : sectoresAsignados t (N t1)  ++ sectoresAsignados t (N t2)
                                                                else sectoresAsignados t (N t1)  ++ sectoresAsignados t (N t2)

--
tripulantes :: Nave -> [Tripulante]
tripulantes (N EmptyT)
tripulantes (N (NodeT (S _ _ tripulantes) t1 t2)) = tripulantes

--
data Manada = M Lobo

manada :: Manada
manada =
    M (Cazador "Pepe" ["ciervo"]
        (Explorador "Pedro" ["bosque"]
            (Cría "Juan")
            (Cría "Luis"))
        (Explorador "Hernan" ["rio"]
            (Cría "Ana")
            (Cría "Maria"))
        (Cría "Jose"))


--hacer
buenaCaza :: Manada -> Bool
buenaCaza manada = cantidadDeAlimento manada > cantidadDeCrias manada

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M Lobo) = 

--hacer
elAlfa :: Manada -> (Nombre, Int)

--hacer
losQueExploraron :: Territorio -> Manada -> [Nombre]


--
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas c (Bolita color celda) = unoSi (esMismoColor c color) + nroBolitas c celda

esMismoColor :: Color -> Color -> Bool
esMismoColor Rojo Rojo = True
esMismoColor Azul Azul = True
esMismoColor _ _ = False

--
poner :: Color -> Celda -> Celda
poner c CeldaVacia = (Bolita c CeldaVacia)
poner c (Bolita color celda) = (Bolita color (poner c celda))

--
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x t1 t2) = x + (sumarT t1) + (sumarT t2)

--
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + (sizeT t1) +(sizeT t2)

--
mapDoubleT :: Tree Int -> Tree Int
mapDoubleT EmptyT = EmptyT
mapDoubleT (NodeT x t1 t2) = (NodeT (x*2) (mapDoubleT t1) (mapDoubleT t2))

--
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e (EmptyT) = False 
perteneceT e (NodeT x t1 t2) = (e==x) || (perteneceT e t1) || (perteneceT e t2)

--
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e (EmptyT) = 0
aparicionesT e (NodeT x t1 t2) = unoSi (e==x) + (aparicionesT e t1) + (aparicionesT e t2) 

--
leaves :: Tree a -> [a]
leaves (EmptyT) = [] 
leaves (NodeT x EmptyT EmptyT) = x : []
leaves (NodeT x t1 t2) = leaves t1 ++ leaves t2

--
heightT :: Tree a -> Int
heightT (EmptyT) = 0
heightT (NodeT x t1 t2) = 1 + elMasGrande (heightT t1) (heightT t2)

elMasGrande :: Int -> Int -> Int
elMasGrande a b = if a>b
                    then a
                    else b

--
mirrorT :: Tree a -> Tree a 
mirrorT (EmptyT) = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

--
toList :: Tree a -> [a]
toList (EmptyT) = []
toList (NodeT x t1 t2) = x : ((toList t1) ++ (toList t2))

--
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT x t1 t2) = x : []
levelN n (NodeT x t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2)

--mal
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) =  [x] : ((listPerLevel t1) ++ (listPerLevel t2))

--hacer
ramaMasLarga :: Tree a -> [a]

--hacer
todosLosCaminos :: Tree a -> [[a]]

