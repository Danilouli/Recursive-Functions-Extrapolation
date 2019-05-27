-- C-c C-l loads buffer into ghci

module Recursion where

-- primitive recursion: basic functions

-- zero function $\zeta$
zero :: [Int] -> [Int]
zero [] = [0]

-- successor function $\sigma$
suc :: [Int] -> [Int]
suc [x] = [x+1]

-- projection functions $\pi^n_m$
proj :: Int -> Int -> [Int] -> [Int]
proj n m l | n >= m && m > 0 = [l !! (m-1)]
	   | otherwise       = []

-- primitive recursive building blocks

-- combination $f \times g$
comb :: ([Int]->[Int]) -> ([Int]->[Int]) -> [Int]->[Int]
comb f g l = f l ++ g l

-- an alternative infix notation for comb
infixl 7 @*
f @* g = comb f g

-- composition $f \circ g$
comp :: ([Int]->[Int]) -> ([Int]->[Int]) -> [Int]->[Int]
comp f g l = f(g l) -- f . g

-- primitive recursion

-- primrec' uses first parameter for recursion
primrec' :: ([Int]->[Int]) -> ([Int]->[Int]) -> [Int]->[Int]
primrec' g h (x:l) | x==0      = g l
		   | otherwise 
  = h ((primrec' g h (x-1:l)) ++ [x-1] ++ l)

-- primrec uses last parameter for recursion
-- this is the official semantics of p.r.

-- (note code is much less efficient as it needs
--  repeated list traversals)
primrec :: ([Int]->[Int]) -> ([Int]->[Int]) -> [Int]->[Int]
primrec g h l 
    | x == 0    = g l'
    | otherwise
	= h (l'++[x']++(primrec g h (l'++[x'])))
    where x  = last l
	  x' = x-1
	  l' = front l

-- minimalisation, $\mu$
mu :: ([Int]->[Int]) -> [Int]->[Int]
mu g l = mu' 0
    where mu' x
	      | g (l++[x]) == [0] = [x]
	      | otherwise         = mu' (x+1)

-- combinators built from primitive recursive operations

-- addition: [x,y] |-> [x+y]
plus :: [Int]->[Int]
plus = primrec (proj 1 1) (suc . (proj 3 3))

-- constant functions $K^n_m$
kay :: Int -> Int -> [Int]->[Int]
kay 0 0 = zero
kay 0 m = suc . (kay 0 (m-1))

kay n m = primrec (kay (n-1) m) (proj (n+1) (n+1))
-- note we are only allowing ourselves the basic
-- building blocks and function building operations

-- multiplication
mult :: [Int]->[Int]
mult = primrec (kay 1 0) (plus . (proj 3 1 @* proj 3 3))

-- exponentiation
expo :: [Int]->[Int]
expo = primrec (kay 1 1) (mult . (proj 3 1 @* proj 3 3))

-- predecessor
pre :: [Int]->[Int]
pre = primrec zero (proj 2 1)

-- monus, or cut-off subtraction
monus :: [Int]->[Int]
monus = primrec (proj 1 1) (pre . (proj 3 3))

-- switch arguments
switch :: [Int]->[Int]
switch = proj 2 2 @* proj 2 1

-- test for inequality
neq1 :: [Int]->[Int]
neq1 = plus . (monus . switch @* monus)

-- test for equality
eq :: [Int]->[Int]
eq = monus . (kay 2 1 @* plus . (monus . switch @* monus))

-- negation (note that eq is now just neg neq)
neg :: ([Int]->[Int]) -> [Int]->[Int]
neg f = monus . (kay 2 1 @* f)

-- true inequality (neq[x,y] returns 1 if x and y differ)
neq :: [Int]->[Int]
neq = neg eq

-- quotient
quo :: [Int]->[Int]
quo = quo' . switch
    where
    quo' = primrec (kay 1 0) h
    h    = plus . (g @* proj 3 3)
    g    = eq . (suc . (proj 3 2) @* plus . (k @* proj 3 1))
    k    = mult . (proj 3 1 @* proj 3 3)

-- auxiliary functions

ror l = last l :  front l   -- ROtate list to the Right
rol l = tail l ++ [head l]  -- ROtate list to the Left

front []    = []
front [x]   = []
front (x:l) = x:front l

-- primrec g h l = rol(primrec' g h (ror l)) -- RUBBISH




