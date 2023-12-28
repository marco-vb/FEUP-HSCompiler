module Stack (
    Stack,
    push, pop, top,
    empty, isEmpty
) where

data Stack a = Stk [a] deriving Show -- implementation with lists

-- Add a new element to the top of the stack
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

-- Remove the top element from the stack
pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Run-time error"

-- Return the top element of the stack
top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Run-time error"

-- Create an empty stack
empty :: Stack a
empty = Stk []

-- Check if the stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False
