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
│ Copyright 2016-2017 Luca Padovani                                 ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Exceptions where

import Language
import Render
import Control.Exception
import Data.List (intersperse)

data MyException
  = ErrorIncoherentType Type Key
  | ErrorNonLinearMessage Key
  | ErrorNonLinearName Name
  | ErrorMultipleTypeDefinitions Name
  | ErrorUndefinedNames [Name]
  | ErrorAmbiguousMatch Type
  | ErrorTypeMismatch Type Type
  | ErrorNonContractiveTypes
  | ErrorFreeTypeVariables
  | ErrorMethodNotAvailable Name Key
  | ErrorInvalidNegativePattern Tag
  | ErrorInvalidType Int
  | ErrorUndefinedType Name
  | ErrorSyntax String
  | ErrorIncompatibleDependencies [Name]
  | ErrorRelevantArgument Key Type

instance Exception MyException

instance Show MyException where
  show (ErrorIncoherentType t (Key tag _)) = showTag tag ++ ": incoherent type " ++ show t
  show (ErrorNonLinearMessage (Key tag _)) = showTag tag ++ ": non-linear message in pattern"
  show (ErrorNonLinearName u) = u ++ ": non-linear name in pattern"
  show (ErrorMultipleTypeDefinitions x) = x ++ ": multiple type definitions"
  show (ErrorUndefinedNames us) = "undefined names: " ++ concat (intersperse ", " us)
  show (ErrorAmbiguousMatch t) = "the pattern " ++ show t ++ " matches messages with different argument types"
  show (ErrorTypeMismatch t s) = "type mismatch:\n" ++ showRelation '⋢' t s
  show ErrorNonContractiveTypes =  "there are non-contractive type definitions"
  show ErrorFreeTypeVariables = "there are free type variables in type definitions"
  show (ErrorMethodNotAvailable u κ) = "there is no method " ++ show κ ++ " in class " ++ u
  show (ErrorInvalidNegativePattern tag) = showTag tag ++ ": invalid use of negative pattern"
  show (ErrorInvalidType n) = show n ++ ": not a valid type"
  show (ErrorUndefinedType x) = x ++ ": undefined type"
  show (ErrorSyntax msg) = msg
  show (ErrorIncompatibleDependencies us) = "incompatible dependencies: " ++ concat (intersperse "," us)
  show (ErrorRelevantArgument κ t) = "message " ++ show κ ++ " has a relevant argument of type " ++ show t
