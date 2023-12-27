import Distribution.TestSuite (TestInstance(name))
import Data.List(sortOn)
import Control.Monad.RWS (Sum)
-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type VarName = String
data VarValue = IntegerValue Integer
              | BoolValue Bool
              deriving Show

printVarVal :: VarValue -> String
printVarVal (IntegerValue x) = Prelude.show x
printVarVal (BoolValue x) = Prelude.show x

printVar :: Var -> String
printVar (name, value) = name ++ "=" ++ printVarVal value

type Stack = [VarValue]

type Var = (VarName, VarValue)

type State = [Var]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str = foldr (\x y -> if y /= "" then printVarVal x ++ "," ++ y else printVarVal x) ""

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = foldr (\x y -> if y /= "" then printVar x ++ "," ++ y else printVar x) "" sorted
                  where sorted = sortOn fst state

-- Stack operations
top :: Stack -> VarValue
top (x:_) = x

pop :: Stack -> Stack
pop (_:xs) = xs

push :: VarValue -> Stack -> Stack
push x stack = x:stack

-- Instruction set
addValues :: VarValue -> VarValue -> VarValue
addValues (IntegerValue a) (IntegerValue b) = IntegerValue (a + b)
addValues _ _ = error "Run-time error"

add :: Stack -> Stack
add stack = push result (pop (pop stack))
            where result = top stack `addValues` top (pop stack)

multValues :: VarValue -> VarValue -> VarValue
multValues (IntegerValue a) (IntegerValue b) = IntegerValue (a * b)
multValues _ _ = error "Run-time error"

mult :: Stack -> Stack
mult stack = push result (pop (pop stack))
             where result = top stack `multValues` top (pop stack)

subValues :: VarValue -> VarValue -> VarValue
subValues (IntegerValue a) (IntegerValue b) = IntegerValue (a - b)
subValues _ _ = error "Run-time error"

sub :: Stack -> Stack
sub stack = push result (pop (pop stack))
            where result = top stack `subValues` top (pop stack)

true :: Stack -> Stack
true = push (BoolValue True)

false :: Stack -> Stack
false = push (BoolValue False)

eqValues :: VarValue -> VarValue -> VarValue
eqValues (IntegerValue a) (IntegerValue b) = BoolValue (a == b)
eqValues (BoolValue a) (BoolValue b) = BoolValue (a == b)
eqValues _ _ = error "Run-time error"

eq :: Stack -> Stack
eq stack = push result (pop (pop stack))
           where result = top stack `eqValues` top (pop stack)

leValues :: VarValue -> VarValue -> VarValue
leValues (IntegerValue a) (IntegerValue b) = BoolValue (a <= b)
leValues _ _ = error "Run-time error"

le :: Stack -> Stack
le stack = push result (pop (pop stack))
           where result = top stack `leValues` top (pop stack)

andValues :: VarValue -> VarValue -> VarValue
andValues (BoolValue a) (BoolValue b) = BoolValue (a && b)
andValues _ _ = error "Run-time error"

and :: Stack -> Stack
and stack = push result (pop (pop stack))
            where result = top stack `andValues` top (pop stack)

negValue :: VarValue -> VarValue
negValue (BoolValue a) = BoolValue (not a)
negValue _ = error "Run-time error"

neg :: Stack -> Stack
neg stack = push result (pop stack)
            where result = negValue (top stack)

fetch :: VarName -> Stack -> State -> Stack
fetch varName oldStack ((headName, headVal):stateTail) | headName /= varName = fetch varName oldStack stateTail
                                                       | otherwise = push headVal oldStack

store :: VarName -> Stack -> State -> (Stack, State)
store varName oldStack [] = (pop oldStack, newState)
                        where newState = [(varName, top oldStack)]
store varName oldStack ((headName, headVal):oldStateTail) | varName == headName = (pop oldStack, newState1)
                                                          | otherwise = (newStack, (headName, headVal):newState)
                                                          where newState1 = (varName, top oldStack):oldStateTail
                                                                (newStack, newState) = store varName oldStack oldStateTail

branch :: Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 stack state = case top stack of
  BoolValue True -> (c1, pop stack, state)
  BoolValue False -> (c2, pop stack, state)
  _ -> error "Run-time error"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:r, stack, state) = case inst of
  Push n -> run (r, push (IntegerValue n) stack, state)
  Add -> run (r, add stack, state)
  Mult -> run(r, mult stack, state)
  Sub -> run(r, sub stack, state)
  Tru -> run(r, true stack, state)
  Fals -> run(r, false stack, state)
  Equ -> run(r, eq stack, state)
  Le -> run(r, le stack, state)
  And -> run(r, Main.and stack, state)
  Neg -> run(r, neg stack, state)
  Fetch x -> run(r, fetch x stack state, state)
  Store x -> let (newStack, newState) = store x stack state in run (r, newStack, newState)
  Noop -> run(r, stack, state)
  Branch c1 c2 -> let (branchCode, newStack, newState) = branch c1 c2 stack state in run(branchCode ++ r, newStack, newState)
  Loop c1 c2 -> run(c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ r, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp = AddExp Aexp Aexp | MultExp Aexp Aexp | SubExp Aexp Aexp | Var VarName | Num Integer
data Bexp = AndExp Bexp Bexp | LeExp Aexp Aexp | EquExp Aexp Aexp | NegExp Bexp | Bool Bool

data Stm = AssignAexp VarName Aexp | AssignBexp VarName Bexp | While Bexp [Stm] | If Bexp [Stm] [Stm]

compA :: Aexp -> Code
compA (AddExp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (MultExp a1 a2) = compA a1 ++ compA a2 ++ [Mult]
compA (SubExp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]

compB :: Bexp -> Code
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LeExp a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (EquExp a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (NegExp b) = compB b ++ [Neg]
compB (Bool True) = [Tru]
compB (Bool False) = [Fals]


-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
