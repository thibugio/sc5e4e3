data ListElement a = Element a |
                     SubList [ListElement a] deriving (Eq, Show, Read)

append [] l = l
append l1 l2 = head l1 : append (tail l1) l2 

-- Usage: flatten [Element 1,Element 3,SubList [Element 4,SubList [SubList [Element 5],SubList []]],Element 6] id
--          => [Element 1,Element 3,Element 4,Element 5,Element 6]
flatten [] ret = ret []
flatten ((Element a):rest) ret = flatten rest (\v -> (ret ((Element a):v)))
flatten ((SubList a):rest) ret = flatten rest (\v1 -> (flatten a (\v2 -> (ret (append v2 v1)))))

-- Usage: myreverse [Element 1,Element 3,SubList [Element 4,SubList [SubList [Element 5],SubList []]],Element 6] []
--          => [Element 6,SubList [SubList [SubList [],SubList [Element 5]],Element 4],Element 3,Element 1]
myreverse [] acc = acc
myreverse ((Element a):rest) acc = myreverse rest ((Element a):acc)
myreverse ((SubList a):rest) acc = myreverse rest (append ((SubList (myreverse a [])):[]) acc)
