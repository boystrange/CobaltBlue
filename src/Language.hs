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

module Language where

import qualified Data.Map as M

type Pos  = Maybe (Int, Int)
type Name = String

----------
-- TAGS --
----------

data Tag = Tag Pos String
  deriving (Show)

instance Eq Tag where
  (==) (Tag _ tag₁) (Tag _ tag₂) = tag₁ == tag₂

instance Ord Tag where
  compare (Tag _ tag₁) (Tag _ tag₂) = compare tag₁ tag₂

-----------------------
-- TYPES AND CLASSES --
-----------------------

type ClassType = M.Map Key [Type]

data Key = Key Tag Int
  deriving (Eq, Ord)

data TVar = TVar Int
  deriving (Eq, Ord)

data Type
  = Var TVar
  | Basic Name
  | Ref Name
  | Zero
  | One
  | Message Tag [Type]
  | Type :+: Type
  | Type :·: Type
  | Star Type
  deriving (Eq, Ord)

boolTag :: Bool -> Tag
boolTag = Tag Nothing . show

trueTag :: Tag
trueTag = boolTag True

falseTag :: Tag
falseTag = boolTag False

boolType :: Type
boolType = Message trueTag [] :+: Message falseTag []

numberType :: Type
numberType = Basic "Number"

stringType :: Type
stringType = Basic "String"

arrayType :: Type
arrayType = Basic "Array"

handleType :: Type
handleType = Basic "Handle"

timeType :: Type
timeType = Basic "Time"

replyVar :: Name
replyVar = "$sender"

replyTag :: Tag
replyTag = Tag Nothing "Reply"

replyType :: [Type] -> Type
replyType = Message replyTag

anonymousType :: Type
anonymousType = Var (TVar (-1))

-----------------
-- CONSTRAINTS --
-----------------

data Constraint
  = Trivial
  | UpperBound TVar Type
  | LowerBound Type Type
  deriving (Eq, Ord)

---------------------
-- SOURCE LANGUAGE --
---------------------

data S_Expression
  = S_True
  | S_False
  | S_Int Int
  | S_Double Double
  | S_String String
  | S_Name Name
  | S_Call S_Expression Tag [S_Expression]
  | S_And S_Expression S_Expression
  | S_Or S_Expression S_Expression
  | S_Not S_Expression
  | S_AnonymousObject [S_Rule]

data S_Process
  = S_Null
  | S_Send S_Expression Tag [S_Expression]
  | S_Parallel S_Process S_Process
  | S_Object Name U_Kind [S_Rule] S_Process
  | S_Let [Name] S_Expression S_Process
  | S_Sequence S_Expression S_Process
  | S_If S_Expression S_Process S_Process
  | S_Case S_Expression [S_SimpleRule]

data S_Rule = S_Rule [U_Pattern] [Tag] S_Process
data S_SimpleRule = S_SimpleRule Tag [Name] S_Process
data S_Class = S_Class Name (Maybe ClassType) [S_SimpleRule]

----------------------
-- UNTYPED LANGUAGE --
----------------------

data U_Expression
  = U_Int Int
  | U_Double Double
  | U_String String
  | U_Name Name

data U_Process
  = U_Null
  | U_Send Name Tag [U_Expression]
  | U_Parallel U_Process U_Process
  | U_Object Name U_Kind [U_Rule] U_Process

data U_Kind
  = U_Static (Maybe ClassType)
  | U_Dynamic (Maybe Type)
  | U_Linear (Maybe Type)

data U_Pattern = U_Pattern Tag [Name]
data U_Rule = U_Rule [U_Pattern] [Tag] U_Process
data U_SimpleRule = U_SimpleRule Tag [Name] U_Process
data U_Class = U_Class Name (Maybe ClassType) [(U_Pattern, U_Process)]

-----------------------
-- INTERNAL LANGUAGE --
-----------------------

data Expression
  = Int Int
  | Double Double
  | String String
  | Name Name Type

data Process
  = Null
  | Send Name (Either ClassType Type) Tag [Expression]
  | Parallel Process Process
  | Object Name Kind [Rule] Process

data Kind
  = Static ClassType
  | Dynamic Type
  | Linear Type

data Pattern = Pattern Tag [(Name, Type)]
data Rule = Rule [Pattern] [Tag] Process
data SimpleRule = SimpleRule Pattern Process
data Class = Class Name ClassType [(Pattern, Process)]

type TypeDefinitions = M.Map Name Type
type TypeInequations = M.Map TVar Type
type TypeEquations = M.Map TVar Type
