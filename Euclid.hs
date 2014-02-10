euclid :: Int -> Int -> (Int, Int, [Int])
euclid a b = euc (a, b, [])

euc :: (Int, Int, [Int]) -> (Int, Int, [Int])
euc (a, b, qs) = if r == 0
	then (a, b, qs)
	else euc (b, r, q:qs)
	where
		r = mod a b
		q = div a b
		
sSeq :: Int -> [Int] -> Int
sSeq 0 _ = 1
sSeq 1 _ = 0
sSeq n q = (sSeq (n-2) q) - (q !! n)*(sSeq (n-1) q)

rSeq :: Int -> [Int] -> Int
rSeq 0 _ = 0
rSeq 1 _ = 1
rSeq n q = (rSeq (n-2) q) - (q !! n)*(rSeq (n-1) q)

altEuc :: Int -> Int -> Int
altEuc x y = if x == y
	then x
	else if x > y
		then altEuc (x-y) y
		else altEuc x (y-x)