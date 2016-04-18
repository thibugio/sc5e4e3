-- hw answers 

-- list that contains elements and sublists
data NestedList a = Element a | SubList [NestedList a] deriving (Show, Eq)

flatten [] = []
flatten ((Element x):xs) = (Element x) : (flatten xs)  
flatten ((SubList x):xs) = (flatten x) ++ (flatten xs) -- ++ is the append operator 

myreverse [] = []
myreverse ((Element x):xs) = (myreverse xs) ++ ((Element x):[]) 
myreverse ((SubList x):xs) = (myreverse xs) ++ ((SubList (myreverse x)):[])

-- Monad
filteradd x ml f = do
    l <- ml --pull the list out of the monad
    if (f x) then return (x:l) else Nothing

checklist [] f = (Just [])
checklist (x:xs) f = filteradd x (checklist xs f) f

checkappend ml1 ml2 f = do
    l1 <- ml1
    v1 <- checklist l1 f --checklist returns a monad
    l2 <- ml2
    return (v1 ++ l2)
