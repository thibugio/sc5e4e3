factorial n =
  if n == 0
  then 1
  else n * (factorial (n - 1))

{- by declaring the type of factorial2, we are restricting the input and output values -}
factorial2 :: Int -> Int

factorial2 0 = 1
factorial2 n = n * (factorial2 (n - 1))

{- Two ways to create a function in Scheme: explicit binding or implicit binding
   (define (factorial n) ...  
   (define factorial (lambda (n) ...
  We can do the same in Haskell -}
factorial3 = 
  \n -> if n == 0
        then
          1
        else
          n * (factorial3 (n - 1))

fact_cps 0 return = return 1
fact_cps n return = fact_cps (n - 1) (\v -> return (n * v)) 

{- subsequence tail recursive form -}
{- subsequence [2,4] [4,1,2,2,3,4,5]  -> [4,1,2,3,5] -}

subsetsequence_cps n [] return = return []
subsetsequence_cps [] m return = return m
subsetsequence_cps n m return =
  if (head m) == (head n)
   then
     subsetsequence_cps (tail n) (tail m) return
   else
     subsetsequence_cps n (tail m) (\v -> return ((head m):v))


{- create a type -}
data Coordinate = Coord Double Double  | 
                  Coord3D Double Double Double deriving(Eq, Show)

getx (Coord x y) = x
gety (Coord x y) = y

distance (Coord x y) = sqrt(x*x + y*y)

distance2 v = sqrt ((getx v) * (getx v) + (gety v) * (gety v))
