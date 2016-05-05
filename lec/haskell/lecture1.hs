{- calculate the factorial of a number -}
factorial n = 
  if n == 0
    then
      1
    else
      n * factorial (n - 1)

{- append two lists with the function body defined as an if statment -}
myappend l1 l2 =
  if l1 == []
    then
      l2
    else
      -- cons (car l1) (append (cdr l1) l2)
      -- cons is :
      -- car is head
      -- cdr is tail
      (head l1) : (myappend (tail l1) l2)

{- another way to write append as different input conditions -}
myappend2 [] l2 = l2
myappend2 l1 l2 = (head l1) : (myappend2 (tail l1) l2)

{- Do newton:s method for square root using k iterations.
   Demonstrates the use of let in haskell  -}
squareroot n 0 = n
squareroot n k =
  let old = squareroot n (k - 1)
  in
    old - (old * old - n) / (2 * old)

{- remove subsequence -}
removesubsequence [] l = l
removesubsequence s [] = []
removesubsequence s l =
  if (head s) == (head l)
    then
      removesubsequence (tail s) (tail l)
    else
      (head l) : (removesubsequence s (tail l))

{- inorder returns True if and only if the list is in numeric order.
   Demonstrates composing two functions -}
inorder lis =
  if (null lis) || ((null . tail) lis)
    then
      True
    else if (head lis) > ((head . tail) lis)
      then
        False
      else
        inorder (tail lis)
  
