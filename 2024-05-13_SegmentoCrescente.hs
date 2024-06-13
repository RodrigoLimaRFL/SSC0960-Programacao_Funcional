-- comprimento(int *v) -> int comprimento
comprimentoSegmentoCrescente :: (Num a, Ord a) => [a] -> a
comprimentoSegmentoCrescente [] = 0 -- lista vazia = 0
comprimentoSegmentoCrescente [x] = 1 -- lista com 1 elemento = 1
comprimentoSegmentoCrescente (x:y:xs) -- lista com 2 ou mais elementos
    | x < y = 1 + comprimentoSegmentoCrescente (y:xs) -- if(v[i] < v[i+1]) comprimento++
    | otherwise = 1 -- else comprimento = 1

-- maiorComprimento(int *v) -> int maiorComprimento
maiorComprimento :: (Num a, Ord a) => [a] -> a
maiorComprimento [] = 0 -- lista vazia = 0
maiorComprimento [x] = 1 -- lista com 1 elemento = 1
maiorComprimento (x:xs) = max (comprimentoSegmentoCrescente (x:xs)) (maiorComprimento xs) -- lista com 2 ou mais elementos = MAX(comprimento do segmento atual, maior comprimento)

main :: IO ()
main = do
    input <- getLine
    let lista = map read (words input)
    let comprimento = maiorComprimento lista
    print comprimento
