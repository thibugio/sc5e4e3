
data NestedList a = Element a | SubList [NestedList a] deriving (Show)

flatten [] = []
flatten ((Element x):xs) = (Element x) : (flatten xs)
flatten ((SubList x):xs) = (flatten x) ++ (flatten xs)

myreverse [] = []
myreverse ((Element x):xs) = (myreverse xs) ++ ((Element x):[])
myreverse ((SubList x):xs) = (myreverse xs) ++ ((SubList (myreverse x)):[])

filteradd x ml f = do
 l <- ml
 if (f x) then return (x:l) else Nothing

checklist [] f = Just []
checklist (x:xs) f = filteradd x (checklist xs f) f

checkappend mx my f = do
 x <- mx
 v <- checklist x f
 y <- my
 return (v ++ y)
   
