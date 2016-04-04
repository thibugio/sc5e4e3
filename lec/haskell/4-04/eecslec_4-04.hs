-- Not-a-Number or a single rational number
-- Value 4 => Value (4 % 1)
-- Value takes a type parameter, "a"
data Value a = NaN | Value a deriving (Show, Eq) 

mydivide3 NaN _ = NaN
mydivide3 _ NaN = NaN
mydivide3 (Value x) (Value y) = if (y == 0) then NaN else Value (x / y)

-- division for the Value type: ((Value 5) // (Value 0)) // (Value 10) => NaN
--(//) NaN _ = NaN
--(//) _ NaN = NaN
--(//) (Value x) (Value y) = if (y == 0) then NaN else Value (x / y)

-- "apply" function (haskell calls it "bind")
-- "a" is a type parameter: Value<a>
-- takes a Value a, and a function a->Value b, and applies the function to the internal type of Value, returning a new Value of type b
-- example: bind (Value 2) (\v -> vreturn (10 * v))
bind :: Value a -> (a -> Value b) -> Value b
bind NaN f = NaN 
bind (Value x) f = f x 

-- "return" function
-- creates a Value type given an element
vreturn :: a -> Value a
vreturn x = Value x

-- Monad (single parameter): pass along functions for parameter that can have multiple type values
-- for a Monad to work, need a "bind", and need a "return"
-- bind (Value a) vreturn => needs to return (Value a)
-- rewrite "divide" using "bind" and "vreturn"
--(//) :: (Value Rational) -> (Value Rational) -> (Value Rational) --internal types are the same
--(//) :: (Value a) -> (Value a) -> (Value a) --doesn't work because "a"'s type might not allow division
-- :t (//) is (//) :: (Eq b, Fractional b) => Value b -> Value b -> Value b
(//) vx vy = bind vx (\x -> 
                bind vy (\y -> if y == 0 then NaN else vreturn (x / y))) 

-- :t Nothing is Nothing :: Maybe a
-- :t (Just 10) is (Just 10) :: Num a => Maybe a
-- redefine "bind" using Nothing and Maybe
bind2 :: Maybe a -> (a -> Maybe b) -> Maybe b
bind2 Nothing f = Nothing
bind2 (Just x) f = f x

vreturn2 :: a -> Maybe a
vreturn2 x = Just x

(///) vx vy = bind2 vx (\x -> 
                bind2 vy (\y -> if y == 0 then Nothing else vreturn2 (x / y))) 

{- 
Haskell's Maybe monad:
-- (Maybe is the name for the type; Just is the type constructor name)
data Maybe a = Just a | Nothing
bind: >>=
vreturn: return

the Monad must have a single parameter type, but can have many cases for the different types the type parameter can assume
-}

-- "divide" using Haskell's built-in bind and return
(////) vx vy = 
    (>>=) vx (\x -> 
        (>>=) vy (\y -> if y == 0 then Nothing else return (x / y)))


-- "divide" another way: "do" operator for Monad programming
-- extract values from 2 monads and apply the function
-- example: (Just 10) ///// (Just 4) => (Just 2.5)
(/////) vx vy = do
    x <- vx -- shortcut for bind operator: 'x' is the thing extracted out of the Monad
    y <- vy
    if y == 0 then Nothing else return (x / y) -- the function

(/*/) vx vy = do
    x <- vx
    y <- vy
    return (x * y)


