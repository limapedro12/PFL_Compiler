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
import GHC.Generics (Associativity(LeftAssociative))
import qualified Data.Functor.Identity
import qualified Text.Parsec.Prim
import Data.Binary.Get (lookAhead)
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

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
top [] = error "Run-time error"

pop :: Stack -> Stack
pop (_:xs) = xs
pop [] = error "Run-time error"

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
fetch _ _ [] = error "Run-time error"
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
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

{-
Tipo de dados que representa uma expressão aritmética
-}
data Aexp = AddExp Aexp Aexp | MultExp Aexp Aexp | SubExp Aexp Aexp | Var VarName | Num Integer
            deriving (Eq, Show)

{-
Tipo de dados que representa uma expressão booleana
-}
data Bexp = AndExp Bexp Bexp | LeExp Aexp Aexp | AEquExp Aexp Aexp | BEquExp Bexp Bexp | NegExp Bexp | Bool Bool
            deriving (Eq, Show)

{-
Tipo de dados que representa um statement
-}
data Stm = Assign VarName Aexp | SequenceOfStatements [Stm] | While Bexp Stm | IfThenElse Bexp Stm Stm | NoopStm
           deriving (Eq, Show)

{-
Tipo de dados que representa um programa (lista de statements)
-}
type Program = [Stm]

{-
Função que recebe uma expressão aritmética na sua representação interna
(do tipo Aexp) e retorna um conjunto de intruções equivalentes.
Ex: compA (AddExp (Var "x") (MultExp (Num 5) (Num 70))) =>
      [Push 70,Push 5,Mult,Fetch "x",Add]
-}
compA :: Aexp -> Code
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (MultExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]

{-
Função que recebe uma expressão boleana na sua representação interna (do
tipo Bexp) e retorna um conjunto de intruções equivalentes.
Ex: compB (AndExp (EquExp (Var "x") (Num 2)) (Bool True)) =>
      [Tru,Push 2,Fetch "x",Equ,And]
-}
compB :: Bexp -> Code
compB (AndExp b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (LeExp a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AEquExp a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (BEquExp b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (NegExp b) = compB b ++ [Neg]
compB (Bool True) = [Tru]
compB (Bool False) = [Fals]

{-
Função que recebe um statement na sua representação interna (do tipo Stm)
e retorna um conjunto de intruções equivalentes.
Ex: compStm (While (Bool True) [Assign "a" (Num 10),Assign "b" (Num 20)]) =>
      [Loop [Tru] [Push 10,Store "a",Push 20,Store "b"]]
-}
compStm :: Stm -> Code
compStm (Assign name a) = compA a ++ [Store name]
compStm (While b stmList) = [Loop (compB b) (compStm stmList)]
compStm (IfThenElse b thenStmList elseStmList) = compB b ++ [Branch (compStm thenStmList) (compStm elseStmList)]
compStm (SequenceOfStatements stmList) = concatMap compStm stmList
compStm NoopStm = [Noop]

{-
Função que recebe um programa na sua representação interna (lista de statements)
e retorna um conjunto de intruções equivalentes.
Ex: compile [Assign "y" (AddExp (Var "x") (Num 1)), Assign "x" (Num 2)] =>
      [Push 1,Fetch "x",Add,Store "y",Push 2,Store "x"]
-}
compile :: Program -> Code
compile = concatMap compStm

{-
Na secção seguinte, definimos um conjunto de Parsers (do tipo definido pelo 
Parsec), que são utlizados para transformar a string com o código na 
representação interna definida acima.
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
Parser que faz parse de uma string com um statement na nossa linguagem e
devolve um Stm que representa internamente esse statement
Ex: Parsec.parse statementParser "" "x := 42;" => Right(Assign "x" (Num 42))
-}
statementParser :: Parser Stm
statementParser = ifParser <|> whileParser <|> noStatementParser <|> sequenceOfStatementsParser <|> assignParser

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
aExpParser = buildExpressionParser aOperators aTerm

aOperators :: [[Operator String () Data.Functor.Identity.Identity Aexp]]
aOperators = [[Infix (MultExp <$ charWithSpaces '*') AssocLeft],
              [Infix (AddExp <$ charWithSpaces '+') AssocLeft, Infix (SubExp <$ charWithSpaces '-') AssocLeft]]

aTerm :: Parser Aexp
aTerm = lexeme term
  where
    term = try (parens (lexeme aExpParser))
      <|> try (Num <$> lexeme intParser)
      <|> try (Var <$> lexeme varNameParser)
    parens = between (stringWithSpaces "(") (stringWithSpaces ")")

bExpParser :: Parser Bexp
bExpParser = buildExpressionParser bOperators bTerm

bOperators :: [[Operator String () Data.Functor.Identity.Identity Bexp]]
bOperators = [[Prefix (NegExp <$ stringWithSpaces "not")],
              [Infix (BEquExp <$ charWithSpaces '=') AssocNone],
              [Infix (AndExp <$ stringWithSpaces "and") AssocLeft]]

bTerm :: Parser Bexp
bTerm = lexeme term
  where
    term = try (parens (lexeme bExpParser))
      <|> try (lexeme arithmeticComparisonParser)
      <|> try (NegExp <$> (stringWithSpaces "not" >> bExpParser))
      <|> try (Bool False <$ lexeme (stringWithSpaces "False"))
      <|> try (Bool True <$ lexeme (stringWithSpaces "True"))
    parens = between (stringWithSpaces "(") (stringWithSpaces ")")

arithmeticComparisonParser :: Text.Parsec.Prim.ParsecT   String () Data.Functor.Identity.Identity Bexp
arithmeticComparisonParser =
  do a1 <- aExpParser
     op <- arithmeticComparisonOperators
     a2 <- aExpParser
     return (op a1 a2)

arithmeticComparisonOperators :: Text.Parsec.Prim.ParsecT   String () Data.Functor.Identity.Identity (Aexp -> Aexp -> Bexp)
arithmeticComparisonOperators = (stringWithSpaces "<=" >> return LeExp)
                            <|> (stringWithSpaces "==" >> return AEquExp)

{-
Parser que faz parse de uma string com um assignment statement na nossa
linguagem e devolve um Stm do formato 'Assign VarName Aexp' que representa
internamente esse statement.
Ex: Parsec.parse assignParser "" "x := 42;" => Right (Assign "x" (Num 42))
-}
assignParser :: Parser Stm
assignParser = try (Assign <$> lexeme varNameParser
                           <*> (stringWithSpaces ":=" >> lexeme aExpParser <* charWithSpaces ';'))

{-
Parser que faz parse de uma string com uma parte do código (como o código
dentro de um if statement ou de um while loop) e devolve uma lista de
statements, ou seja, um Program. Caso este conjunto seja vazio, delvolve 
uma lista com apenas um elemento, o NoopStm, que posteriormente pode ser 
transformado numa instrução Noop.
-}
sequenceOfStatementsParser :: Parser Stm
sequenceOfStatementsParser = try (SequenceOfStatements <$> (charWithSpaces '(' >> 
                                 (many commentParser >> many1 (statementParser <* many commentParser))
                                  <* charWithSpaces ')'))

{-
Parser que faz parse de uma string apenas com ';' ou "()"", espaços, tabs ou new
lines e devolve um NoopStm. Este parser tem como objetivo evitar erros,
caso o utilizador escreva algum ';' a mais, no inicio ou no fim de outros
statements, ou caso algum if statement ou while loop não contenham instruções, 
o parser, não devolverá nenhum erro. Se este parser não existisse código como 
"a := 6;;" ou "while (x >= 1) do ()" devolveria um erro, e assim estes
statements apenas são ignorados.
Ex: Parsec.parse noStatementParser "" "; \n ; " => Right NoopStm
-}
noStatementParser :: Parser Stm
noStatementParser = try (many1 (charWithSpaces ';') >> return NoopStm) <|> 
                    try (stringWithSpaces "()" >> return NoopStm)

{-
Parser que faz parse de uma string com um if statement na nossa linguagem 
e devolve um Stm do formato 'IfThenElse Bexp [Stm] [Stm]' que representa
internamente esse statement.
Ex: Parsec.parse ifParser "" "if (True) then x :=1; else y := 2;" => 
      Right (IfThenElse (Bool True) [Assign "x" (Num 1)] [Assign "y" (Num 2)])
-}
ifParser :: Parser Stm
ifParser = IfThenElse <$> (try (string "if" >> Text.Parsec.Combinator.lookAhead (space <|> char '(')) >>
                               spaces >> lexeme bExpParser)
                      <*> (stringWithSpaces "then" >> statementParser)
                      <*> 
                          option NoopStm (
                            stringWithSpaces "else" >> statementParser)

{-
Parser que faz parse de uma string com um while loop statement na nossa 
linguagem e devolve um Stm do formato 'While Bexp [Stm]' que representa 
internamente esse statement.
Ex: Parsec.parse whileParser "" "while (True) do (a :=10; b := 20;)" => 
      Right (While (Bool True) [Assign "a" (Num 10),Assign "b" (Num 20)])
-}
whileParser :: Parser Stm
whileParser = While <$> (try (string "while" >> Text.Parsec.Combinator.lookAhead (space <|> char '(')) >>
                               spaces >> lexeme bExpParser)
                    <*> (stringWithSpaces "do" >> statementParser)

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
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
