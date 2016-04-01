-- types can extend other type classes using 'deriving'
-- Coord is the constructor
-- a type can be the OR of more than one structure
-- same type, but different constructor names
-- can place getter/setter methods within the type declaration
data Coordinate = Coord Double Double |
                  Coord3D {c3d_getx :: Double, c3d_gety :: Double, c3d_getz :: Double} |
                  Coord4D Double Double Double Double deriving (Show, Eq)
-- use _ to indicate 'dont care' value, since interpreter/compiler does pattern matching on the inputs
c_getx (Coord x _) = x
c_gety (Coord _ y) = y

distance (Coord x y) = sqrt(x*x + y*y)
distance (Coord3D x y z) = sqrt(x*x + y*y + z*z)
distance (Coord4D x y z w) = sqrt(x*x + y*y + z*z + w*w)

-- add two coordinate values
-- add (Coord 3 4) (Coord 5 6)
-- add (Coord 3 4) (Coord3D 5 6 7) => Coord3D {c3d_getx = 8.0, c3d_gety = 10.0, c3d_getz = 7.0} 
add (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)
add (Coord x1 y1) (Coord3D x2 y2 z2) = Coord3D (x1 + x2) (y1 + y2) z2
add (Coord3D x2 y2 z2) (Coord x1 y1) = Coord3D (x1 + x2) (y1 + y2) z2

{-
-- specify the type of a function:
add :: Coordinate -> Coordinate -> Coordinate
add c1 c2 = Coord ((c_getx c1) + (c_getx c2)) ((c_gety c1) + (c_gety c2))
-}

-- generic types: checks that the parameterized types 'a' match in the constructor
data GenericCoordinate a = GCoord a a deriving (Show, Eq)

-- number division
-- mydivide :: Fractional a => a -> a -> a
-- mydivide (5 :: Rational) (0 :: Rational) => *** Exception: Ratio has zero denominator
-- mydivide (5 :: Rational) (2 :: Rational) => 5 % 2
-- mydivide (5 :: Int) (2 :: Int) => Error: Haskell does not allow integer division
mydivide x y = x / y

-- take a single input x and produces a function that takes an input y and returns x / y
-- always groups left-to-right, so don't need ()s: (mydivide2 5) 2 => 2.5
-- mydivide2 :: Fractional a => a -> a -> a
-- All of Haskell's functions are a single input: facilitates Modus Ponens
-- called 'currying' (Finkle): take a function with multiple arguments and turn it into a composition of single-argument functions
-- mydivide is a shortcut for this 
mydivide2 x = (\y -> x / y) 

-- mysub :: Num a => a -> a -> a -> a
mysub x y z = (x - y) - z
-- mysub2 :: Num a => a -> a -> a -> a
mysub2 x = (\y -> (\z -> (x - y) - z))

-- Not-a-Number or a single rational number
-- Value 4 => Value (4 % 1)
data Value = NaN | Value Rational deriving (Show, Eq) 

mydivide3 NaN _ = NaN
mydivide3 _ NaN = NaN
mydivide3 (Value x) (Value y) = if (y == 0) then NaN else Value (x / y)

-- a symbol for division: ((Value 5) // (Value 0)) // (Value 10) => NaN
(//) NaN _ = NaN
(//) _ NaN = NaN
(//) (Value x) (Value y) = if (y == 0) then NaN else Value (x / y)

