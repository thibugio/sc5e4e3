{- Create a data type that is a rational but also keeps track of whether we ever tried
   to make the denominator 0
 -}
data Value a = NaN | Value a deriving (Show, Eq)

-- the divide for the Value type
-- (//) NaN _ = NaN
-- (//) _ NaN = NaN
-- (//) (Value x) (Value y) = if (y == 0) then NaN else Value (x / y)

-- bind takes a value and a function, applies the function to the internal type
--  of value and returns a new value
bind NaN f = NaN
bind (Value x) f = f x

-- vreturn creates a Value type given an element
vreturn x = Value x

{--- the divide using bind and vreturn
(//) vx vy =
   bind vx (\x ->
      bind vy (\y -> if y == 0 then NaN else vreturn (x / y)))
-}


{--- the divide using the built in monad Maybe.  Maybe is either Nothing or Just x
(//) vx vy =
   vx >>= (\x ->
      vy >>= (\y -> if y == 0 then Nothing else vreturn (x / y)))
-}

-- the same divide, but this time using the do operation to shorten the monad syntax
(//) vx vy = do
  x <- vx
  y <- vy
  if y == 0 then Nothing else return (x / y)

-- now for a multiplication that uses the same Maybe monad
(/*/) vx vy = do
  x <- vx
  y <- vy
  return (x * y)

