import Data.List

isPrime :: Int -> Bool
isPrime num = (length divs) == 2 && elem num divs
	where
		divs = divisors num

divisors :: Int -> [Int]
divisors num = filter (isDivisor num) [1..num]

isDivisor :: Int -> Int -> Bool
isDivisor num d = (mod num d) == 0