{
{-# OPTIONS -w #-}
{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of Cobalt.                                      ║
│                                                                   ║
│ Cobalt is free software: you can redistribute it and/or modify it ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ Cobalt is distributed in the hope that it will be useful, but     ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with Cobalt.  If not, see <http://www.gnu.org/licenses/>.   ║
│                                                                   ║
│ Copyright 2016 Luca Padovani                                      ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Parser (parseProcess) where

import Lexer
import Language
import Exceptions

import Data.Char (isUpper)
import Data.Either (partitionEithers)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Exception
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  DONE   { Token _ TokenDone }
  CLASS  { Token _ TokenClass }
  LINEAR { Token _ TokenLinear }
  OBJECT { Token _ TokenObject }
  LET    { Token _ TokenLet }
  AND    { Token _ TokenAnd }
  CASE   { Token _ TokenCase }
  OF     { Token _ TokenOf }
  IN     { Token _ TokenIn }
  TRUE   { Token _ TokenTrue }
  FALSE  { Token _ TokenFalse }
  IF     { Token _ TokenIf }
  THEN   { Token _ TokenThen }
  ELSE   { Token _ TokenElse }
  TYPE   { Token _ TokenType }
  '&'    { Token _ TokenAmp }
  '¬'    { Token _ TokenNeg }
  INT    { Token _ (TokenInt $$) }
  DOUBLE { Token _ (TokenDouble $$) }
  STRING { Token _ (TokenString $$) }
  ID     { $$@(Token _ (TokenId _)) }
  REF    { Token _ (TokenRef $$) }
  '='    { Token _ TokenEQ }
  '.'    { Token _ TokenDot }
  ':'    { Token _ TokenColon }
  ';'    { Token _ TokenSemiColon }
  ','    { Token _ TokenComma }
  '|'    { Token _ TokenOr }
  '('    { Token _ TokenLParen }
  ')'    { Token _ TokenRParen }
  '{'    { Token _ TokenLBrace }
  '}'    { Token _ TokenRBrace }
  '['    { Token _ TokenLBrack }
  ']'    { Token _ TokenRBrack }
  '+'    { Token _ TokenPlus }
  '-'    { Token _ TokenMinus }
  '×'    { Token _ TokenTimes }
  '/'    { Token _ TokenDiv }
  '%'    { Token _ TokenMod }
  '·'    { Token _ TokenCDot }
  '*'    { Token _ TokenStar }
  '^'    { Token _ TokenHat }
  '?'    { Token _ TokenQMark }
  '!'    { Token _ TokenEMark }
  '▸'    { Token _ TokenArrow }
  '∧'    { Token _ TokenAND }
  '∨'    { Token _ TokenOR }
  '<'    { Token _ TokenLT }
  '≤'    { Token _ TokenLE }
  '>'    { Token _ TokenGT }
  '≥'    { Token _ TokenGE }
  '≠'    { Token _ TokenNE }

%nonassoc '}' ']' IN ELSE
%right '&'
%right ';'

%left '∧' '∨'
%nonassoc '<' '>' '=' '≠' '≤' '≥'
%right '^'
%left '+' '-'
%left '×' '/' '%' MUL
%left '·'
%nonassoc '¬'
%nonassoc '*' UMINUS
%left '.'

%%

Program
  : TypeDeclList Process { ($1, $2) }

Process
  : DONE { S_Null }
  | '{' Process '}' { $2 }
  | CLASS Id ClassOpt '[' RuleList ']' InOpt Process
    { S_Object $2 (U_Static $3) $5 $8 }
  | OBJECT Id TypeOpt '[' MethodList ']' InOpt Process
    { S_Object $2 (U_Dynamic $3) $5 $8 }
  | LINEAR OBJECT Id TypeOpt '[' MethodList ']' InOpt Process
    { S_Object $3 (U_Linear $4) $6 $9 }
  | Process '&' Process { S_Parallel $1 $3 }
  | Expr '!' Tag Exprs { S_Send $1 $3 $4 }
  | Expr ';' Process { S_Sequence $1 $3 }
  | LET ArgNeList '=' Expr IN Process { S_Let $2 $4 $6 }
  | CASE Expr OF '[' SimpleRuleList ']' { S_Case $2 $5 }
  | IF Expr THEN Process ELSE Process { S_If $2 $4 $6 }

Id
  : ID { getId $1 }

Tag
  : ID { Tag (Just $ getPos $1) (getId $1) }

TypeDeclList
  : { [] }
  | TYPE TypeDeclNeList IN { $2 }

TypeDeclNeList
  : TypeDecl { [$1] }
  | TypeDecl AND TypeDeclNeList { $1 : $3 }

TypeDecl
  : REF '=' Type { ($1, $3) }

ClassOpt
  : { Nothing }
  | ':' Class { Just $2 }

InOpt
  : { }
  | IN { }

TypeOpt
  : { Nothing }
  | ':' Type { Just $2 }

Class
  : MessageSigList { M.fromList $1 }

MessageSigList
  : { [] }
  | MessageSigNeList { $1 }

MessageSigNeList
  : MessageSig { [$1] }
  | MessageSig '·' MessageSigNeList { $1 : $3 }

MessageSig
  : '*' Tag Types { (Key $2 (length $3), $3) }

Type
  : INT
    { if $1 == 0 then Zero
      else if $1 == 1 then One
      else throw (ErrorInvalidType $1)  }
  | '?' { anonymousType }
  | REF { Ref $1 }
  | Type '+' Type { $1 :+: $3 }
  | Type '·' Type { $1 :·: $3 }
  | '*' Type { Star $2 }
  | Tag Types { Message $1 $2 }
  | '(' Type ')' { $2 }

Types
  : { [] }
  | '(' TypeList ')' { $2 }

TypeList
  : { [] }
  | TypeNeList { $1 }

TypeNeList
  : Type { [$1] }
  | Type ',' TypeNeList { $1 : $3 }

Exprs
  : { [] }
  | '(' ExprList ')' { $2 }

ExprList
  : { [] }
  | ExprNeList { $1 }

ExprNeList
  : Expr { [$1] }
  | Expr ',' ExprNeList { $1 : $3 }

Expr
  : TRUE { S_True }
  | FALSE { S_False }
  | INT { S_Int $1 }
  | DOUBLE { S_Double $1 }
  | STRING { S_String $1 }
  | Id { S_Name $1 }
  | Expr '∧' Expr { S_And $1 $3 }
  | Expr '∨' Expr { S_Or $1 $3 }
  | Expr '.' Tag Exprs { S_Call $1 $3 $4 }
  | Expr '=' Expr { binaryOp "Number" "EQ" $1 $3 }
  | Expr '≠' Expr { binaryOp "Number" "NE" $1 $3 }
  | Expr '<' Expr { binaryOp "Number" "LT" $1 $3 }
  | Expr '≤' Expr { binaryOp "Number" "LE" $1 $3 }
  | Expr '>' Expr { binaryOp "Number" "LT" $3 $1 }
  | Expr '≥' Expr { binaryOp "Number" "LE" $3 $1 }
  | Expr '^' Expr { binaryOp "String" "Append" $1 $3 }
  | Expr '+' Expr { binaryOp "Number" "Add" $1 $3 }
  | Expr '-' Expr { binaryOp "Number" "Sub" $1 $3 }
  | Expr '×' Expr { binaryOp "Number" "Mul" $1 $3 }
  | Expr '*' Expr %prec MUL { binaryOp "Number" "Mul" $1 $3 }
  | Expr '/' Expr { binaryOp "Number" "Div" $1 $3 }
  | Expr '%' Expr { binaryOp "Number" "Mod" $1 $3 }
  | '-' Expr %prec UMINUS { unaryOp "Number" "Neg" $2 }
  | '¬' Expr { S_Not $2 }
  | '(' Expr ')' { $2 }
  | '[' MethodList ']' { S_AnonymousObject $2 }

Args
  : { [] }
  | '(' ArgList ')' { $2 }

ArgList
  : { [] }
  | ArgNeList { $1 }

ArgNeList
  : Id { [$1] }
  | Id ',' ArgNeList { $1 : $3 }

RuleList
  : { [] }
  | RuleNeList { $1 }

RuleNeList
  : Rule { [$1] }
  | Rule '|' RuleNeList { $1 : $3 }

Rule
  : Pattern '▸' Process
    { case $1 of
        Left msg -> S_Rule [msg] [] $3
        Right tag -> throw (ErrorInvalidNegativePattern tag)
    }

SimpleRuleList
  : { [] }
  | SimpleRuleNeList { $1 }

SimpleRuleNeList
  : SimpleRule { [$1] }
  | SimpleRule '|' SimpleRuleNeList { $1 : $3 }

SimpleRule
  : Tag Args '▸' Process { S_SimpleRule $1 $2 $4 }

MethodList
  : { [] }
  | MethodNeList { $1 }

MethodNeList
  : Method { [$1] }
  | Method '|' MethodNeList { $1 : $3 }

Method
  : JoinPattern '▸' Process
    { let (msgs, guards) = partitionEithers $1 in S_Rule msgs guards $3 }

JoinPattern
  : Pattern { [$1] }
  | Pattern '&' JoinPattern { $1 : $3 }

Pattern
  : Tag Args { Left (U_Pattern $1 $2) }
  | '¬' Tag { Right $2 }

{
unaryOp :: Name -> String -> S_Expression -> S_Expression
unaryOp u tag e = S_Call (S_Name u) (Tag Nothing tag) [e]

binaryOp :: Name -> String -> S_Expression -> S_Expression -> S_Expression
binaryOp u tag e₁ e₂ = S_Call (S_Name u) (Tag Nothing tag) [e₁, e₂]

getId :: Token -> String
getId (Token _ (TokenId x)) = x

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String ([(Name, Type)], S_Process)
parseProcess = runAlex' parse
}
