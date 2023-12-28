module Stack (
    Stack,
    push, pop, top,
    empty, isEmpty
) where

data Stack a = Stk [a] deriving Show -- using lists

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Run-time error"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Run-time error"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False
