import Distribution.TestSuite(TestInstance(name))
import Data.List(sortOn)
import Text.Parsec (ParseError,try,char,digit,letter,string,eof,many1,option,(<|>),many)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Monad
import Data.Either
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
  Mult -> run (r, mult stack, state)
  Sub -> run (r, sub stack, state)
  Tru -> run (r, true stack, state)
  Fals -> run (r, false stack, state)
  Equ -> run (r, eq stack, state)
  Le -> run (r, le stack, state)
  And -> run (r, Main.and stack, state)
  Neg -> run (r, neg stack, state)
  Fetch x -> run (r, fetch x stack state, state)
  Store x -> let (newStack, newState) = store x stack state in run (r, newStack, newState)
  Noop -> run (r, stack, state)
  Branch c1 c2 -> let (branchCode, newStack, newState) = branch c1 c2 stack state in run (branchCode ++ r, newStack, newState)
  Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ r, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

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

data Exp = Aexp | Bexp

data Aexp = AddExp Aexp Aexp | MultExp Aexp Aexp | SubExp Aexp Aexp | VarA VarName | Num Integer
            deriving (Eq, Show)
data Bexp = AndExp Bexp Bexp | LeExp Aexp Aexp | EquExp Aexp Aexp | NegExp Bexp | VarB VarName | Bool Bool
            deriving (Eq, Show)
data Stm = AssignAexp VarName Aexp | AssignBexp VarName Bexp | While Bexp [Stm] | IfThenElse Bexp [Stm] [Stm] | NoopStm
           deriving (Eq, Show)

type Program = [Stm]

compA :: Aexp -> Code
compA (AddExp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (MultExp a1 a2) = compA a1 ++ compA a2 ++ [Mult]
compA (SubExp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (VarA x) = [Fetch x]
compA (Num n) = [Push n]

compB :: Bexp -> Code
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LeExp a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (EquExp a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (NegExp b) = compB b ++ [Neg]
compB (VarB x) = [Fetch x]
compB (Bool True) = [Tru]
compB (Bool False) = [Fals]

compStm :: Stm -> Code
compStm (AssignAexp name a) = compA a ++ [Store name]
compStm (AssignBexp name b) = compB b ++ [Store name]
compStm (While b stmList) = [Loop (compB b) (compile stmList)]
compStm (IfThenElse b thenStmList elseStmList) = compB b ++ [Branch (compile thenStmList) (compile elseStmList)]
compStm NoopStm = [Noop]

compile :: Program -> Code
compile = concatMap compStm

{-
Nesta secção definimos um cpnjunto de Parsers (tipo definido pelo Parsec), 
que são utlizados para transformar a string com o código na representação
interna definida acima.
-}

{-
Parser que faz parse de uma string com inteiro para respetivo inteiro.
Ex: Parsec.parse intParser "" "123" => Right 123
-}
intParser :: Parser Integer
intParser = do
    n <- many1 digit
    return (read n)

{-
Função que recebe um Parser e retorna outro Parser que consome os todos os
espaços, new lines, e tabs, que estejam depois do texto pretendido.
-}
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

{-
Função que recebe uma string e retorna um Parser que faz parse da respetiva
string e consome todos os espaços, new lines, e tabs, que se seguem.
Ex: Parsec.parse (stringWithSpaces "haskell") "" "haskell \n\t  " => Right "haskell"
-}
stringWithSpaces :: String -> Parser String
stringWithSpaces s = lexeme (string s)

{-
Função que recebe um caráter e retorna um Parser que faz parse do respetivo
caráter e consome todos os espaços, new lines, e tabs, que se seguem.
Ex: Parsec.parse (charWithSpaces 'o') "" "o \n\t  " => Right 'o'
-}
charWithSpaces :: Char -> Parser Char
charWithSpaces c = lexeme (char c)

{-
Parser que faz parse de uma string com código na nossa liguagem e devolve
uma lista de statements, ou seja, um Program.
-}
codeParser :: Parser Program
codeParser = many commentParser >> many (statementParser <* many commentParser) <* eof

{-
Parser que faz parse de uma string com uma parte do código (como o código
dentro de um if statement ou de um while loop) e devolve uma lista de
statements, ou seja, um Program. Caso este conjunto seja vazio, delvolve 
uma lista com apenas um elemento, o NoopStm, que posteriormente pode ser 
transformado numa instrução Noop.
-}
blockOfStatementsParser :: Parser Program
blockOfStatementsParser = option [NoopStm] (many commentParser >> many1 (statementParser <* many commentParser))

{-
Parser que faz parse de uma string com um statement na nossa linguagem e
devolve um Stm que representa internamente esse statement
Ex: Parsec.parse statementParser "" "x := 42;" => Right(AssignAexp "x" (Num 42))
-}
statementParser :: Parser Stm
statementParser = ifParser <|> whileParser <|> noStatementParser <|> assignAexpParser

{-
Parser que faz parse de um comentário.
Ex: Parsec.parse commentParser "" "/* lala */" => Right " lala "
-}
commentParser :: Parser String
commentParser = try (lexeme (string "/*" >> manyTill anyChar (string "*/"))) <|>
                     lexeme (string "//" >> manyTill anyChar (char '\n'))

{-
Lista de nomes reservados, ou seja, nenhum dos elementos desta lista pode
ser o nome de uma variável.
-}
reservedNames :: [String]
reservedNames = ["if", "then", "else", "while", "do", "and", "not", "or"]

{-
Parser que faz parse do nome de uma variável. Este tem que começar com uma
letra minuscula, e o restante do nome só pode conter letras(maiusculas ou 
minusculas), números e underscores.
Ex: Parsec.parse varNameParser "" "certo_numero_1" => Right "" "certo_numero_1"
-}
varNameParser :: Parser String
varNameParser = do
  first <- lower
  rest <- many (letter <|> digit <|> char '_')
  if (first:rest) `notElem` reservedNames
    then return (first:rest) 
    else error ("\nYou cannot name a variable " ++ (first:rest) ++ " because it is a reserved name.\n")

aExpParser :: Parser Aexp
aExpParser = do {
    exp <- try (Num <$> intParser) <|>
               (VarA <$> many1 letter);
    many (letter   <|> digit    <|> char '+' <|>
          char '-' <|> char '*' <|> char '/');
    return exp;
}

bExpParser :: Parser Bexp
bExpParser = try (string "True" >> return (Bool True)) <|>
             try (string "False" >> return (Bool False)) <|>
             try (string "(True)" >> return (Bool True)) <|>
                 (string "(False)" >> return (Bool False))

{-
Parser que faz parse de uma string com um assignment statement na nossa
linguagem e devolve um Stm do formato 'AssignAexp VarName Aexp' que representa
internamente esse statement.
Ex: Parsec.parse assignAexpParser "" "x := 42;" => Right (AssignAexp "x" (Num 42))
-}
assignAexpParser :: Parser Stm
assignAexpParser = AssignAexp <$> lexeme varNameParser
                              <*> (stringWithSpaces ":=" >> lexeme aExpParser <* charWithSpaces ';')

{-
Parser que faz parse de uma string apenas com ';', espaços, tabs ou new
lines e devolve um NoopStm. Este parser tem como objetivo evitar erros,
caso o utilizador escreva ';' a mais no inicio ou no fim de outros statements,
não o parser não devolverá nenhum erro. Se este parser não existisse código
como este "a := 6;;" devolveria um erro, assim o segundo ';' apenas é
ignorado.
Ex: Parsec.parse noStatementParser "" "; \n ; " => Right NoopStm
-}
noStatementParser :: Parser Stm
noStatementParser = try (many1 (charWithSpaces ';') >> return NoopStm)

{-
Parser que faz parse de uma string com um if statement na nossa linguagem 
e devolve um Stm do formato 'IfThenElse Bexp [Stm] [Stm]' que representa
internamente esse statement.
Ex: Parsec.parse ifParser "" "if (True) then x :=1; else y := 2;" => 
      Right (IfThenElse (Bool True) [AssignAexp "x" (Num 1)] [AssignAexp "y" (Num 2)])
-}
ifParser :: Parser Stm
ifParser = IfThenElse <$> try (stringWithSpaces "if" >> lexeme bExpParser)
                      <*> (stringWithSpaces "then" >> 
                          ((charWithSpaces '(' >> blockOfStatementsParser <* charWithSpaces ')')
                      <|>   do { s <- statementParser; return [s] }))
                      <*> 
                          option [NoopStm] (
                            stringWithSpaces "else" >> 
                            ((charWithSpaces '(' >> blockOfStatementsParser <* charWithSpaces ')')
                      <|>   do { s <- statementParser; return [s] }))

{-
Parser que faz parse de uma string com um while loop statement na nossa 
linguagem e devolve um Stm do formato 'While Bexp [Stm]' que representa 
internamente esse statement.
Ex: Parsec.parse whileParser "" "while (True) do (a :=10; b := 20;)" => 
      Right (While (Bool True) [AssignAexp "a" (Num 10),AssignAexp "b" (Num 20)])
-}
whileParser :: Parser Stm
whileParser = While <$> try (stringWithSpaces "while" >> lexeme bExpParser)
                    <*> (stringWithSpaces "do" >> 
                        ((charWithSpaces '(' >> blockOfStatementsParser <* charWithSpaces ')')
                    <|>   do { s <- statementParser; return [s] }))

{-
Função que recebe como argumento uma string com um programa na nossa liguagem
e faz parse do mesmo, retornando o programa na sua representação interna 
(lista de statements). Se não conseguir devove uma exceção a indicar aonde se
encontra o erro.
-}
parse :: String -> Program
parse programString | isRight res = parsedProgram
                    | isLeft res = throwParseError errorWhileParsing
    where
        res = Parsec.parse codeParser "" programString
        Right parsedProgram = res
        Left errorWhileParsing = res

{-
Função que recebe um ParseError e lança-o, através da função 'error', de
forma mais inteligível.
-}
throwParseError :: ParseError -> Program
throwParseError errorToThrow = error ("\nParse Error in " ++ show errorToThrow ++ "\n")

{-
Função que recebe como argumento uma string com o nome de um ficheiro, cujo
conteudo é um programa na nossa liguagem e faz parse do mesmo, retornando o 
programa na sua representação interna (lista de statements). Se não conseguir
devove uma exceção a indicar aonde se encontra o erro.
-}
parseFile :: String -> IO [Stm]
parseFile fileName = do program  <- readFile fileName
                        return (parse program)

{-
Função que recebe como argumento uma string com o nome de um ficheiro, cujo
conteudo é um programa na nossa liguagem, executa-o e devolve um par de 
strings que representam a stack e o armazenamento após a execução.
-}
testParserFile :: String -> IO (String, String)
testParserFile fileName = do programCode  <- readFile fileName
                             return (testParser programCode)

{-
Função que recebe como argumento uma string com um programa na nossa
liguagem, executa-o e devolve um par de strings que representam a stack
e o armazenamento após a execução.
-}
-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
