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

module Presburger.Formula
  (Expression(..),
   Relation(..),
   Formula(..),
   Goal(..))
where

data Expression = Const Int
                | Var String
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Int Expression

instance Show Expression where
  show (Const n) = show n
  show (Var x) = x
  show (Add e f) = "(" ++ show e ++ " + " ++ show f ++ ")"
  show (Sub e f) = "(" ++ show e ++ " - " ++ show f ++ ")"
  show (Mul n e) = show n ++ " × " ++ show e

data Relation = RelEQ | RelLE | RelLT

instance Show Relation where
  show RelEQ = "="
  show RelLE = "≤"
  show RelLT = "<"

data Formula = FALSE
             | TRUE
             | Rel Relation Expression Expression
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Forall String Formula
             | Exists String Formula

instance Show Formula where
  show FALSE = "⊥"
  show TRUE = "⊤"
  show (Rel rel e f) = "(" ++ show e ++ " " ++ show rel ++ " " ++ show f ++ ")"
  show (And e f) = "(" ++ show e ++ " ∧ " ++ show f ++ ")"
  show (Or e f) = "(" ++ show e ++ " ∨ " ++ show f ++ ")"
  show (Not e) = "¬" ++ show e
  show (Forall x e) = "∀" ++ x ++ ":" ++ show e
  show (Exists x e) = "∃" ++ x ++ ":" ++ show e

data Goal = Implication [String] Formula Formula
