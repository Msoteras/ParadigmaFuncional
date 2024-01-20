module Library where
import PdePreludat

-- Parte 1

data Personaje = UnPersonaje {
    vida :: Number,
    armadura :: Armadura
    --arma ::
} deriving (Show, Eq)
type Armadura = [(Number, Number)]

poderDeDefensa :: Personaje -> Number
poderDeDefensa personaje = vida personaje + sum (map fst (filter ((/=0).snd) (armadura personaje)))
-- Parte 2 (El ataque)

--poderDeAtaque :: Personaje -> Number
--poderDeAtaque 

--Parte 3 (Los "buffs")
type Buffs =  Personaje -> Personaje
frenesi :: Buffs
frenesi personaje = personaje{armadura = map (\(defensa, x) -> (1.2* defensa,x)) (armadura personaje)}
-- o puedo hacer una funcion aumentardef factor def =  map (\(defensa, x) -> (factor defensa,x)) armadura
-- factor d


mantoEtereo :: Buffs
mantoEtereo personaje = personaje{armadura = map (\(defensa, x) -> (3+ defensa,x)) (armadura personaje), vida = vida personaje - 100}


--berserker :: Buffs
--berserker personaje =

espejoDeKarma :: Buffs -> Buffs
espejoDeKarma buff  = buff.buff

sucesionDeBuffs :: [Buffs] -> Buffs
sucesionDeBuffs buffs personaje = foldl (\personaje buff -> buff personaje) personaje buffs

buffCreativo :: Buffs
buffCreativo personaje 
        | poderDeDefensa personaje > 20 = personaje {vida = vida personaje * 1.5}
        | otherwise = agregarDurabilidadArmadura personaje


agregarDurabilidadArmadura personaje 
    | (length (armadura personaje)) >= 10 = personaje
    | otherwise = personaje {armadura = filter (\(x, durabilidad) -> durabilidad /= 0) (armadura personaje)}


-- 1

potenciarBuff :: Buffs -> Personaje -> Personaje
potenciarBuff buff personaje  = buff personaje

-- 2

buffInofensivo :: [Personaje] -> Buffs -> Bool
buffInofensivo personajes buff = or (zipWith (==) (map (potenciarBuff buff) personajes) personajes)

-- 3
{- es importante el uso de orden superior ya que me permite dividir las tareas a realizar (comparar o aplicar elemento a elemento de una lista)
y a su vez implementar otra funcion que me permite hacer lo que busco (comprar personajes antes y dsps) y aplicar un buff a cada personaje.
Puedo hacer un codigo mas declarativo con funciones que utilizan otras funciones, siendo estas reutilizables en distintos contextos y 
-}

-- Parte 4
desgaste personaje intensidad = debilitarArmadura (armadura personaje) intensidad

debilitarArmadura ((defensa,durabilidad):partes) intensidad 
        | intensidad >=1  =(defensa, max 0 durabilidad-intensidad):(debilitarArmadura partes (div intensidad 2)) 
        | otherwise = partes

{- Si la armadura tiene infinitas partes no habria problema ya que una vez que la intensidad baje de 0 devolverla la lista 7
infinita de partes solo con las primeras partes modificadas hasta que la intensidad alla bajado a 0. Agregar concepto de lazy evalution

Con otras funciones si habria conflictos, ya que por ejemplo una de ellas filtra partes de armadura que cumplen una condicion. 
Esto generaria un conflicto ya que la funcion filtraria infinitamente partes de armaduras sin nunca llegar a un fin-}