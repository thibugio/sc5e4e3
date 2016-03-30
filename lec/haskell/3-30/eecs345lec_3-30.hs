--factorial function written 3 ways:

--long form (then and else must be in the same column)
factorial n =
    if n == 0
    then 1
    else n * (factorial (n - 1))

--short form: do each of the cases separately; 
-- haskell does pattern-matching on the input
factorial2 0 = 1
factorial2 n = n * (factorial2 (n - 1))

{- scheme: 
(define factorial (lambda (n) ...)) => separate the function from the binding -}
-- haskell: '\' is lambda
--          '->' is output
-- can't do Y-combinator because it doesn't have an obvious type

factorial3 = 
 \n -> if n == 0
       then 1
       else n * (factorial3 (n - 1))

-- continuation passing style
-- call it like this: fact_cps 5 (\v -> v)
fact_cps 0 return = return 1
fact_cps n return = fact_cps (n - 1) (\v -> return (n * v))

-- another tail-recursive function: subsequence
{- subsequence [2,4] [1,2,3,4,5] -> [1,3,5] -}
{- subsequence_cps :: Eq a => [a] -> [a] -> ([a] -> t) -> t
    -considered to be composition of 3 functions each with a single input -}

subsequence_cps seq [] return = return []
subsequence_cps [] l return = return l 
subsequence_cps seq l return = 
    if (head seq) == (head l)
    then subsequence_cps (tail seq) (tail l) return
    else subsequence_cps seq (tail l) (\v -> return ((head l) : v))

{- Haskell is strongly typed and uses Loose Name Equivalence:
    -types are equivalent iff they have the same name, or are aliases 
    - no type coercion, automatic widening, etc
    - no method overloading 
-}
-- use ':t <thing>' in the interactive interpreter to ask the type of something

-- define a type for a function:
--factorial :: (Eq a, Num a) => a -> a

--factorial4 6 -> 720
--factorial4 6.0 -> Error
--factorial4 1000 -> 0
factorial4 :: Int -> Int
factorial4 0 = 1
factorial4 n = n * (factorial4 (n - 1))

{- create a type: data typename = 
                    ValueConstructorName
                    typeComponents... 
   
   creating values of a type:
   5 :: Num :: Int
   Coord 4.0 5.0

   (==) Eq Eq = ...
-}

data Coordinate = Coord Double Double deriving (Eq, Show) --Show: has toString()
-- 'or' type: multiple constructors for a single type
data Coordinate2 = Coord2D Double Double | 
                   Coord3D Double Double Double deriving (Eq, Show)

getx (Coord x y) = x
gety (Coord x y) = y

-- functions take a single input: need ()s to group the parameters
distance (Coord x y) = sqrt (x*x + y*y)

-- example with type inference: getx is defined on Coordinates
distance2 v = sqrt ((getx v)*(getx v) + ((gety v)*(gety v))) 




-- quicksort cps style:
{-quicksort_cps [] return = return []
quicksort_cps l return = 
    if null (tail l)
    then return l
    else quicksort_cps (left_half l) (\v1 -> quicksort_cps (pivot l) (\v2 -> quicksort_cps (right_half l) (\v3 -> return (append v1 (append v2 v3)))))

left_half [] p return = return []
left_half l p return = 
    if null (tail l)
    then return l
    else if (head l) < p
         then left_half
    else left_half (tail l) p (\v ->  -}
