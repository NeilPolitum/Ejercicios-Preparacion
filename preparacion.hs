primero :: [Int] -> [Int]
primero ln = [ mod n 10 | n <- ln]

segundo1 :: Int -> [Int]
segundo1 0 = []
segundo1 x = segundo1 (div x 10) ++ [mod x 10]

segundo2 :: [Int] -> [[Int]]
segundo2 ln = [segundo1 l | l <- ln, mod l 3 == 0]

tercero1 :: [Int] -> Int
tercero1 [ ] = 0
tercero1 (x:xs)
	|x > tercero1(xs) = x
	|otherwise = tercero1(xs)
	
tercero2 :: [Int] -> Int
tercero2 [ ] = 99
tercero2 (x:xs)
	|x < tercero2(xs) = x
	|otherwise = tercero2(xs)
	
