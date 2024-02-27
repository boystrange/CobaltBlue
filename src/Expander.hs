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

module Expander (expand) where

import Language
import Render
import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as ST

data ExpansionState = ExpansionState { next :: Int }

type Expansion a = ST.State ExpansionState a

nextTemp :: String -> Expansion Name
nextTemp prefix = do
  state <- ST.get
  let n = next state
  ST.put (state { next = n + 1 })
  return ("_" ++ prefix ++ subscript n)

expandExprs :: ([U_Expression] -> Expansion U_Process) -> [S_Expression] ->
               Expansion U_Process
expandExprs f [] = f []
expandExprs f (e : es) = expandExpr auxHead e
  where
    auxHead e' = expandExprs (auxTail e') es
    auxTail e' es' = f (e' : es')

-- expandExpr f e generates the process that eventually executes (f v)
-- where v is the simple expression corresponding to e
expandExpr :: (U_Expression -> Expansion U_Process) -> S_Expression ->
              Expansion U_Process
expandExpr f (S_Int n) = f (U_Int n)
expandExpr f (S_Double n) = f (U_Double n)
expandExpr f (S_String s) = f (U_String s)
expandExpr f (S_Name u) = f (U_Name u)
expandExpr f (S_AnonymousObject rules) = do
  temp <- nextTemp "tmp"
  rules' <- mapM expandRule rules
  p <- f (U_Name temp)
  return $ U_Object temp (U_Linear Nothing) rules' p
expandExpr f e = do
  temp <- nextTemp "tmp"
  p <- f (U_Name temp)
  expandLet [temp] e p

-- expandCont e u generates the process that sends the value of e to u
expandCont :: S_Expression -> Name -> Expansion U_Process
expandCont S_True u = return $ U_Send u trueTag []
expandCont S_False u = return $ U_Send u falseTag []
expandCont (S_Call e tag es) u = expandSend e tag (es ++ [S_Name u])
expandCont (S_And b₁ b₂) u = do
  p₁ <- expandCont b₂ u
  p₂ <- expandCont S_False u
  expandIf b₁ p₁ p₂
expandCont (S_Or b₁ b₂) u = do
  p₁ <- expandCont S_True u
  p₂ <- expandCont b₂ u
  expandIf b₁ p₁ p₂
expandCont (S_Not b) u = do
  p₁ <- expandCont S_False u
  p₂ <- expandCont S_True u
  expandIf b p₁ p₂
expandCont e u = expandExpr (\e' -> return $ U_Send u replyTag [e']) e

-- expandCase e cases generates a process that sends the value of e to
-- an object that handles "cases"
expandCase :: S_Expression -> [U_SimpleRule] -> Expansion U_Process
expandCase e cases = do
  cont <- nextTemp "cont"
  body <- expandCont e cont
  return $ U_Object cont (U_Linear (Just t)) rules body
  where
    t :: Type
    t = foldl (:+:) One replies

    replies :: [Type]
    replies = [ Message tag (replicate (length args) anonymousType)
              | U_SimpleRule tag args _ <- cases ]

    rules :: [U_Rule]
    rules = [ U_Rule [U_Pattern tag args] [] p
            | U_SimpleRule tag args p <- cases ]

expandIf :: S_Expression -> U_Process -> U_Process -> Expansion U_Process
expandIf e p₁ p₂ = expandCase e [U_SimpleRule trueTag [] p₁,
                                 U_SimpleRule falseTag [] p₂]

expandLet :: [Name] -> S_Expression -> U_Process -> Expansion U_Process
expandLet xs e p = expandCase e [U_SimpleRule replyTag xs p]

expandSend :: S_Expression -> Tag -> [S_Expression] -> Expansion U_Process
expandSend e tag es = expandExpr auxReceiver e
  where
    auxReceiver e' = expandExprs (auxArgs e') es
    auxArgs (U_Name u) es' = return $ U_Send u tag es'

expandRule :: S_Rule -> Expansion U_Rule
expandRule (S_Rule msgs tags p) = do
  q <- expandProcess p
  return $ U_Rule msgs tags q

expandSimpleRule :: S_SimpleRule -> Expansion U_SimpleRule
expandSimpleRule (S_SimpleRule tag args p) = do
  q <- expandProcess p
  return $ U_SimpleRule tag args q

expandProcess :: S_Process -> Expansion U_Process
expandProcess S_Null = return U_Null
expandProcess (S_Send e tag es) = expandSend e tag es
expandProcess (S_Parallel p₁ p₂) = do
  q₁ <- expandProcess p₁
  q₂ <- expandProcess p₂
  return $ U_Parallel q₁ q₂
expandProcess (S_Object u kind rules p) = do
  rules' <- mapM expandRule rules
  q <- expandProcess p
  return $ U_Object u kind rules' q
expandProcess (S_Let xs e p) = expandProcess p >>= expandLet xs e
expandProcess (S_Sequence e p) = expandProcess p >>= expandLet [] e
expandProcess (S_If e p₁ p₂) = do
  q₁ <- expandProcess p₁
  q₂ <- expandProcess p₂
  expandIf e q₁ q₂
expandProcess (S_Case e rules) = mapM expandSimpleRule rules >>= expandCase e

expand :: S_Process -> U_Process
expand p = ST.evalState (expandProcess p) (ExpansionState { next = 0 })
