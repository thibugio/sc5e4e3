{- calculate the factorial of a number -}
factorial n = 
    if n == 0
        then 
            1
        else 
            n * factorial (n - 1)

myappend l1 l2 =
    if l1 == []
        then
            l2
        else
            -- (cons (car l1) (myappend (cdr l1) l2))
            -- cons is :
            -- car is head
            -- cdr is tail
            (head l1) : (myappend (tail l1) l2)

myappend2 [] l2 = l2
myappend2 l1 l2 = (head l1) : (myappend2 (tail l1) l2)

{- do newton's method for squareroot using k iterations -}
squareroot n 0 = n
squareroot n k =
    let old = squareroot n (k - 1)
    in
        old - (old * old - n) / (2 * old)

{- removesubsequence [1,3,5] [1,2,3,4,5,6] => [2,4,6] -}
removesubsequence seq [] = []
removesubsequence [] l = l
removesubsequence seq l = 
    if (head seq) == (head l)
        then
            removesubsequence (tail seq) (tail l)
        else 
            (head l) : (removesubsequence seq (tail l))
    

{- inorder [1,2,5,10] => T 
   inorder [1,5,2,10] => F -}
inorder l = 
    if (null l) || ((null . tail) l) --function composition: (null (tail l))
        then
            True
        else if (head l) > ((head . tail) l) --function composition: (head (tail l))
            then
                False
            else
                inorder (tail l)

{- dotproduct of two vectors of arbitrary length -}
dotproduct v [] = 0
dotproduct [] v = 0
dotproduct v1 v2 = 
    (((head v1) * (head v2)) + (dotproduct (tail v1) (tail v2)))

{- multiply a vector and a matrix -}
firstcolumn [] = []
firstcolumn m = myappend ((head (head m)) : []) (firstcolumn (tail m))

restcolumns [] = []
restcolumns m = (tail (head m)) : (restcolumns (tail m))

vectormult v [] = []
vectormult v m = 
    myappend ((dotproduct v (firstcolumn m)) : []) (vectormult v (restcolumns m))

{- matrixmultiply -}


