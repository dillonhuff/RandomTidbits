euclid :: Int -> Int -> (Int, Int, [Int])
euclid a b = euc (a, b, [])

euc :: (Int, Int, [Int]) -> (Int, Int, [Int])
euc (a, b, qs) = if r == 0
	then (a, b, qs)
	else euc (b, r, q:qs)
	where
		r = mod a b
		q = div a b