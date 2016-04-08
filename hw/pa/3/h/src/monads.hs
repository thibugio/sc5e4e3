-- Step 1

--test_append :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
test_append v (Just lv) test = if (test v) then (Just (v:lv)) else Nothing 
{-Example Usage: 
> test_append 'h' (Just "ello") (\v -> if (elem v "hats") then True else False)
> Just "hello"
> test_append 'h' (Just "ello") (\v -> if (elem v "bats") then True else False)
> Nothing
-}


-- Step 2

--checklist :: [a] -> (a -> Bool) -> Maybe [a]
checklist [] test = Nothing 
checklist l test = if (foldl (&&) True (fmap test l)) then (Just l) else Nothing

-- this version of the implementation is un-necessarily complicated, but I wanted to play around with bindings.
checklist2 [] test = Nothing
checklist2 l test = (>>=) ((>>=) (Just (fmap test l)) (\x -> (Just (foldl (&&) True x)))) (\t -> if t then (Just l) else Nothing)


-- Step 3

--checkappend :: Eq a => Maybe [a] -> Maybe [a] -> (a -> Bool) -> Maybe [a]
append [] l = l
append l1 l2 = head l1 : append (tail l1) l2 

checkappend ml1 ml2 test = if ((>>=) ml1 (\l -> checklist l test)) == Nothing
                           then Nothing
                           else (>>=) ml1 (\l1 -> (>>=) ml2 (\l2 -> (Just (append l1 l2))))
