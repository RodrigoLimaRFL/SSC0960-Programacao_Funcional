isPrimo :: Int -> Int -> Bool
isPrimo x i
    | x <= 1 = False
    | x `mod` i == 0 = False
    | i * i > x = True
    | otherwise = isPrimo x (i + 1)

listaPrimos :: [Int] -> Int -> Int -> [Int]
listaPrimos lista x y
    | x > y = lista
    | isPrimo x 2 = listaPrimos (lista ++ [x]) (x + 1) y
    | otherwise = listaPrimos lista (x + 1) y

maiorIntervaloEntrePrimos :: [Int] -> Int -> Int
maiorIntervaloEntrePrimos lista intervalo
    | null (tail lista) = intervalo
    | head (tail lista) - head lista > intervalo = maiorIntervaloEntrePrimos (tail lista) (head (tail lista) - head lista)
    | otherwise = maiorIntervaloEntrePrimos (tail lista) intervalo

maiorIntervaloEntrePrimosMaiorQueXMenorQueY :: Int -> Int -> Int
maiorIntervaloEntrePrimosMaiorQueXMenorQueY x y = maiorIntervaloEntrePrimos (listaPrimos [] x y) 0

main :: IO ()
main = do
    xStr <- getLine
    yStr <- getLine
    let x = read xStr :: Int
        y = read yStr :: Int
    print $ maiorIntervaloEntrePrimosMaiorQueXMenorQueY x y
