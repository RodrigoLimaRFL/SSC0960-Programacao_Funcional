secondTail :: [a] -> [a]
secondTail (_:_:xs) = xs
secondTail _        = []

separarRodadasBoliche :: [Int] -> Int -> String
separarRodadasBoliche lista rodada
    | null lista = "" -- lista vazia
    | rodada == 10 && head lista == 10 && null (tail lista) = "X | " -- ultima bola da 10 rodada foi strike
    | null (tail lista) = show (head lista) ++ " | " -- ultima bola da 10 rodada foi normal
    | rodada == 10 && head lista == 10 = "X " ++ separarRodadasBoliche (tail lista) rodada -- primeira ou segunda bola da 10 rodada foi strike
    | rodada == 10 && null (secondTail lista) && head lista + head (tail lista) == 10 = show (head lista) ++ " / | " -- ultima bola da 10 rodada foi spare
    | rodada == 10 && head lista + head (tail lista) == 10 = show (head lista) ++ " / " ++ separarRodadasBoliche (secondTail lista) rodada -- segunda bola da 10 rodada foi spare
    | head lista == 10 = "X _ | " ++ separarRodadasBoliche (tail lista) (rodada + 1) -- primeira bola da rodada foi strike
    | head lista + head (tail lista) == 10 = show (head lista) ++ " / | " ++ separarRodadasBoliche (secondTail lista) (rodada + 1) -- segunda bola da rodada foi spare
    | otherwise = show (head lista) ++ " " ++ show (head (tail lista)) ++ " | " ++ separarRodadasBoliche (secondTail lista) (rodada + 1) -- rodada normal

contarPontosBoliche :: [Int] -> Int -> Int
contarPontosBoliche lista rodada
    | null lista = 0 -- lista vazia
    | null (tail lista) = head lista -- ultima bola
    | rodada == 10 && head lista == 10 = head lista + head (tail lista) + head (secondTail lista) -- primeira bola da 10 rodada foi strike
    | rodada == 10 && head lista + head (tail lista) == 10 = head lista + head (tail lista) + head (secondTail lista) -- segunda bola da 10 rodada foi spare
    | head lista == 10 = head lista + head (tail lista) + head (secondTail lista) + contarPontosBoliche (tail lista) (rodada + 1) -- primeira bola da rodada foi strike
    | head lista + head (tail lista) == 10 = head lista + head (tail lista) + head (secondTail lista) + contarPontosBoliche (secondTail lista) (rodada + 1) -- segunda bola da rodada foi spare
    | otherwise = head lista + head (tail lista) + contarPontosBoliche (secondTail lista) (rodada + 1) -- rodada normal

main :: IO()
main = do
    lista <- getLine
    let x = map read (words lista)
    let rodadas = separarRodadasBoliche x 1
    let pontos = contarPontosBoliche x 1
    putStrLn $ rodadas ++ show pontos