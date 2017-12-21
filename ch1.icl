module ch1

import StdEnv

Start = MaxOfList [2, 4, 0, 2, 8, -8, 3, 1332, 482]


/** Exercises **/

// #3

isum :: Int -> Int
isum 0 = 0
isum x = x rem 10 + isum(x/10)

// #4

canDivideByNine x = isum x == 9

// #5, 6

Max x y
    | x < y = y
    | x > y = x
            = x

Min x y
    | x < y = x
    | x > y = y
            = x

// #7

// TODO: How do I keep this with an empty list?
// MaxOfList :: [t] -> t
MaxOfList [] = 0 // This should be the minimum possible int/real value in case
                 // the list has all negative numbers.
MaxOfList [x:xs] = Max x (MaxOfList xs)

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

