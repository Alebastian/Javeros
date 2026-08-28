{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                     { Num $1 }
    | bool                    { Boolean $1 }
    | '(' '+' lista ')'       { Add $3 } 
    | '(' '-' lista ')'       { Sub $3 }
    | '(' '*' lista ')'       { Mul $3 }
    | '(' '/' lista ')'       { Div $3 }
    | '(' "and" lista ')'     { And $3 } 
    | '(' "or" lista ')'      { Or $3 }
    | '(' "not" ASA ')'       { Not $3 }
    | '(' "add1" ASA ')'      { Add1 $3 }
    | '(' "sub1" ASA ')'      { Sub1 $3 }
    | '(' "zero?" ASA ')'     { ZeroP $3 }
    | '(' "expt" ASA ASA ')'  { Expt $3 $4 }
    | '(' '<' lista ')'       { Lt $3 } 
    | '(' '>' lista ')'       { Gt $3 }
    | '(' "<=" lista ')'      { Le $3 } 
    | '(' ">=" lista ')'      { Ge $3 }
    | '(' "eq" ASA ASA ')'    { EqP $3 $4 }

-- RETO 3: No terminal para representar dos o más argumentos.
lista : ASA ASA               { [$1, $2] }
      | ASA lista             { $1 : $2 }

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}
