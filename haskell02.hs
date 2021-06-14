-- Prática 02 de Haskell
-- Nome: Robson Daniel Marchesan 

febreT :: Float -> Bool
febreT graus = graus > 37.8

comFebre :: [Float] -> [Float]
comFebre temF = filter febreT temF

comFebre' :: [Float] -> [Float]
comFebre' cl = filter (\g -> g>37.80) cl

itemize :: [String] -> [String]
itemize lstr = map (\lr -> "<li>"++ lr ++ "</li>") lstr

circleArea :: Float -> Float
circleArea r = pi*(r^2)

bigCircles :: Float -> [Float] -> [Float]
bigCircles n ls = filter (\x -> circleArea x> n)ls

quarentena :: [(String,Float)] -> [(String,Float)]
quarentena tmp = filter (\(_,n) -> febreT n)tmp 

idadesEm :: [Int] -> Int -> [Int]
idadesEm x y = map (\p -> p - y)x 

headA :: String -> String
headA sng | head sng == 'A' = "Super " ++ sng
          | otherwise = sng

changeNames :: [String] -> [String]
changeNames nomes = map headA nomes

onlyShorts :: [String] -> [String]
onlyShorts shrt = filter (\t -> length t <5)shrt
