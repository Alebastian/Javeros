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

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }
  
-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;
--   * operadores estrictamente binarios: expt y eq;
--   * operadores unarios: not, add1, sub1, zero?.  
    
    
    | '(' '+' [ASA] ')'        { Add $3 } 
    | '(' '-' [ASA] ')'        { Sub $3 }
    | '(' '*' [ASA] ')'	       { Mult $3 }
    | '(' '/' [ASA] ')'	       { Div $3 }
    | '(' "and" [ASA] ')'      { And $3 } 
    | '(' "or" [ASA] ')'       { Or $3 }
    | '(' "not" ASA ')'	       { Not $3 }
    | '(' "add1" ASA ')'       { Add1 $3 }
    | '(' "sub1" ASA ')'       { Sub1 $3 }
    | '(' "zero?" ASA ')'      { ZeroP $3 }
    | '(' "expt" ASA ASA ')'   { ZeroP $3 $4 }
    | '(' '<' [ASA] ')'        { Lt $3 } 
    | '(' '>' [ASA] ')'        { Gt $3 }
    | '(' "<=" [ASA] ')'       { Le $3 } 
    | '(' ">=" [ASA] ')'       { Ge $3 }
    | '(' "eq" ASA ASA ')'     { EqP $3 $4 }

-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.

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
