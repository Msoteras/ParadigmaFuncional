module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Hechicero = UnHechicero {
        --nombre :: String, no se pide luego en los ejercicos el mostrar el nombre
        antiguedad :: Number,
       -- experiencia :: String,
        clan :: String,
        grado :: Number
}deriving (Show)

-- Hechiceros
nobara = UnHechicero {antiguedad = 1, clan = "Kugisaki", grado = 3}
satoru = UnHechicero {antiguedad = 15, clan = "Gajo", grado = 0}
maki = UnHechicero {antiguedad = 3, clan = "Zenin", grado = 4}
yuji = UnHechicero {antiguedad = 0, clan = "Kugisaki", grado = 1}


tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = grado hechicero > 1

type Equipo = [Hechicero]
equipoPreparado :: Equipo -> Bool
equipoPreparado equipo = length equipo > 3

subirGrado :: Hechicero -> Hechicero
subirGrado hechicero | esEspecial hechicero = hechicero
                     | otherwise = hechicero {grado = grado hechicero - 1} 

esEspecial :: Hechicero -> Bool
esEspecial hechicero = grado hechicero == 0


clanesPrestigiosos = ["Zenin", "Gojo", "Kamo"]
esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero = elem (clan hechicero) clanesPrestigiosos 

-- Punto 6
esInvencible :: Equipo -> Bool
esInvencible equipo = any esEspecial equipo

--Punto 7
esFavorito :: Equipo -> Bool
esFavorito equipo = all esPrestigioso equipo

--Punto 8
losExpertos :: Equipo -> Equipo
losExpertos equipo = filter tieneExperiencia equipo

--Punto 9
powerUp :: Equipo -> Equipo
powerUp equipo = map subirGrado equipo