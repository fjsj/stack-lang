{-
 - Functional Programming course test.
 - Author: flaviojuvenal@gmail.com
 -}
data Stack = Stack [Int] deriving Show

push x (Stack xs) = Stack (x:xs)

pop (Stack []) = error "Empty Stack!"
pop (Stack (x:xs)) = (x, Stack xs)

data Instruction = Push Int       -- pushes an integer to the top of the Stack
                 | Add            -- adds the two values on the top of the Stack, popping both and pushing the result
                 | Mul            -- multiplies the two values on the top of the Stack, popping both and pushing the result
                 | Neg            -- inverts the sign (i.e. +5 to -5) of the value on the top of the Stack
                 deriving (Show)

type Program = [Instruction]

example1 = [Push 100, Push 5, Push 2, Mul, Push 2, Push 3, Add, Mul, Mul, Neg] --example program in stack-lang

{-
 - interp is the function that is the interpreter of the stack-lang.
 - It recieves a Program and returns the value on the top of the Stack memory.
 -}
eval stack (Add) = let         
                       (x, stack2) = pop stack
                       (y, stack3) = pop stack2
                   in
                       push (x + y) stack3                        
eval stack (Mul) = let         
                       (x, stack2) = pop stack
                       (y, stack3) = pop stack2
                   in
                       push (x * y) stack3
eval stack (Neg) = let         
                       (x, stack2) = pop stack
                   in
                       push (-x) stack2                        
eval stack (Push x) = push x stack

interp = fst . pop . foldl (eval) (Stack [])

{-
 - isCorrect is a function that checks if a given Program is valid. 
 -}
newSize x (Add) = x - 1
newSize x (Mul) = x - 1
newSize x (Neg) = x
newSize x (Push _) = x + 1
                
isCorrectOp (Stack (x:y:xs)) (Add) = True
isCorrectOp _ (Add) = False

isCorrectOp (Stack (x:y:xs)) (Mul) = True
isCorrectOp _ (Mul) = False

isCorrectOp (Stack (x:xs)) (Neg) = True
isCorrectOp _ (Neg) = False

isCorrectOp _ (Push _) = True

isCorrect' [] _ = True
isCorrect' (i:is) old = let
                            new = newSize old i
                        in
                            if new > 0 then isCorrect' is new else False

isCorrect is = isCorrect' is 0

{-
 - maxStack is a function that given a Program caculates the highest height of the Stack memory during this Program execution.
 - letOne is a function that given a Program checks if this Program ends with a single value in the Stack memory.
 - Both functions don't need to run (interp) the Program.
 -}
maxList = foldr1 (max)

maxStack = maxList . scanl (newSize) 0

letOne is = (foldl (newSize) 0 is) == 1

{-
 - compile is a function that converts an expression built with Exp values to a Program (list of Instructions).
 - (interp $ compile exampleExp) returns 6, just as ((2 + (1 - 5)) * -3)
 -}
data Exp = Value Int
         | AddExp Exp Exp
         | SubExp Exp Exp
         | MulExp Exp Exp
         deriving (Show)

-- (2 + (1 - 5)) * -3
exampleExp = (MulExp (AddExp (Value 2) (SubExp (Value 1) (Value 5))) (Value (-3)))
    
compile (Value x) = [Push x]
compile (AddExp exp1 exp2) = (compile exp1) ++ (compile exp2) ++ [Add]
compile (SubExp exp1 exp2) = (compile exp1) ++ (compile exp2) ++ [Neg, Add]
compile (MulExp exp1 exp2) = (compile exp1) ++ (compile exp2) ++ [Mul]


















