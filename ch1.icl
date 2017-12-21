module ch1

import StdEnv

Start = isum 1234


/** Exercises **/

// #3

isum :: Int -> Int
isum 0 = 0
isum x = x rem 10 + isum(x/10)


/** Play **/

over :: Int Int -> Int
over n k = fac n / (fac k * fac(n-k))
	where
		fac :: Int -> Int
		fac n = prod [1..n]


roots a b c = [
		(~b + x)/denom,
		(~b - x)/denom
	]
	where
		x = sqrt (b*b - 4.0*a*c)
		denom = 2.0 * a

abs x
	| x < 0     = ~x
	| otherwise = x


length :: [t] -> Int
length []       = 0
length [_:tail] = 1 + length tail

		