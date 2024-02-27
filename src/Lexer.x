{
{-# OPTIONS -w  #-}
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit  = 0-9
$extra  = [₀-₉⁰-⁹⁺⁻₊₋]
$alpha  = [A-Za-z]
@first  = $alpha
@next   = $alpha | $digit | $extra | \_ | \'
@id     = @first @next*
@int    = $digit+
@float  = (@int \. @int) | (@int \. @int)
@string = \"[^\"]*\"

tokens :-
  $white+ ;
  "//".*  ;
  "."     { lex' TokenDot         }
  ","     { lex' TokenComma       }
  ":"     { lex' TokenColon       }
  ";"     { lex' TokenSemiColon   }
  "&"     { lex' TokenAmp         }
  "¬"     { lex' TokenNeg         }
  "~"     { lex' TokenNeg         }
  "("     { lex' TokenLParen      }
  ")"     { lex' TokenRParen      }
  "{"     { lex' TokenLBrace      }
  "}"     { lex' TokenRBrace      }
  "["     { lex' TokenLBrack      }
  "]"     { lex' TokenRBrack      }
  "▸"     { lex' TokenArrow       }
  "=>"    { lex' TokenArrow       }
  "="     { lex' TokenEQ          }
  "≠"     { lex' TokenNE          }
  "<>"    { lex' TokenNE          }
  "<"     { lex' TokenLT          }
  "≤"     { lex' TokenLE          }
  "<="    { lex' TokenLE          }
  ">"     { lex' TokenGT          }
  "≥"     { lex' TokenGE          }
  ">="    { lex' TokenGE          }
  "∧"     { lex' TokenAND         }
  "/\\"   { lex' TokenAND         }
  "∨"     { lex' TokenOR          }
  "\\/"   { lex' TokenOR          }
  "+"     { lex' TokenPlus        }
  "-"     { lex' TokenMinus       }
  "×"     { lex' TokenTimes       }
  "/"     { lex' TokenDiv         }
  "%"     { lex' TokenMod         }
  "·"     { lex' TokenCDot        }
  "'"     { lex' TokenCDot        }
  "*"     { lex' TokenStar        }
  "?"     { lex' TokenQMark       }
  "!"     { lex' TokenEMark       }
  "|"     { lex' TokenOr          }
  "#" @id { lex (TokenRef . tail) }
  "#" @int { lex (TokenRef . tail) }
  "^"     { lex' TokenHat         }
  @string { lex (TokenString . tail . init) }
  @id     { lex lookupId          }
  @int    { lex (TokenInt . read) }
  @int "." { lex (TokenDouble . fromIntegral . read . init) }
  @float  { lex (TokenDouble . read) }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

keywords :: [(String, TokenClass)]
keywords = [("done",   TokenDone),
            ("let",    TokenLet),
            ("and",    TokenAnd),
            ("in",     TokenIn),
            ("of",     TokenOf),
            ("case",   TokenCase),
            ("linear", TokenLinear),
            ("class",  TokenClass),
            ("object", TokenObject),
            ("new",    TokenObject),
            ("true",   TokenTrue),
            ("false",  TokenFalse),
            ("if",     TokenIf),
            ("then",   TokenThen),
            ("else",   TokenElse),
            ("type",   TokenType)]

lookupId :: String -> TokenClass
lookupId s = case lookup s keywords of
               Nothing -> TokenId s
               Just tok -> tok

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving (Show)

data TokenClass
  = TokenDone
  | TokenLinear
  | TokenObject
  | TokenClass
  | TokenLet
  | TokenAnd
  | TokenIn
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenOf
  | TokenCase
  | TokenType
  | TokenOr
  | TokenInt Int
  | TokenDouble Double
  | TokenString String
  | TokenId String
  | TokenEQ
  | TokenNE
  | TokenLT
  | TokenGT
  | TokenLE
  | TokenGE
  | TokenAND
  | TokenOR
  | TokenTrue
  | TokenFalse
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenMod
  | TokenCDot
  | TokenStar
  | TokenDot
  | TokenComma
  | TokenColon
  | TokenSemiColon
  | TokenAmp
  | TokenNeg
  | TokenArrow
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenLBrack
  | TokenRBrack
  | TokenQMark
  | TokenEMark
  | TokenRef String
  | TokenHat
  | TokenEOF
  deriving (Show)

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
