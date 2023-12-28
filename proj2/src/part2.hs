{-# LANGUAGE BlockArguments #-}
import Text.Parsec as Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Monad
import Data.Either

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

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Exp = Aexp | Bexp

data Aexp = AddExp Aexp Aexp | MultExp Aexp Aexp | SubExp Aexp Aexp | Var VarName | Num Integer
            deriving (Eq, Show)
data Bexp = AndExp Bexp Bexp | LeExp Aexp Aexp | EquExp Aexp Aexp | NegExp Bexp | Bool Bool
            deriving (Eq, Show)
data Stm = AssignAexp VarName Aexp | AssignBexp VarName Bexp | While Bexp [Stm] | IfThenElse Bexp [Stm] [Stm] | NoopStm
           deriving (Eq, Show)

type Program = [Stm]

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

intParser :: Parser Integer
intParser = do
    n <- many1 digit
    return (read n)

codeParser :: Parser Program
codeParser = many statementParser <* eof

statementParser :: Parser Stm
statementParser = try (assignAexpParser) <|> ifParser

aExpParser :: Parser Aexp
aExpParser = do {
    exp <- try (Num <$> intParser) <|>
               (Var <$> many1 letter);
    many (letter   <|> digit    <|> char '+' <|>
          char '-' <|> char '*' <|> char '/');
    return exp;
}

bExpParser :: Parser Bexp
bExpParser = (string "True" >> return (Bool True)) <|>
             (string "False" >> return (Bool False))

assignAexpParser :: Parser Stm
assignAexpParser = AssignAexp <$> many1 letter
                              <*> (string ":=" >> aExpParser <* char ';')

ifParser :: Parser Stm
ifParser = IfThenElse <$> (string "if" >> char '(' >> bExpParser <* char ')')
                      <*> (string "then" >> char '(' >> many statementParser <* char ')')
                      <*> option [NoopStm] (string "else" >> char '(' >> many statementParser <* char ')')

parse :: String -> Program
parse programString | isRight res = parsedProgram
                    | isLeft res = throwParseError errorWhileParsing
    where
        res = Parsec.parse codeParser "" (cleanSpaces programString)
        Right parsedProgram = res
        Left errorWhileParsing = res

cleanSpaces :: String -> String
cleanSpaces = filter (`notElem` " \n\t")

throwParseError :: ParseError -> Program
throwParseError errorToThrow = error ("Parse Error in " ++ show errorToThrow ++ "\n")

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, state2Str state)
--   where (_,stack,state) = run(compile (Main.parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

