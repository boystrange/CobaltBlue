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

module Presburger.Z3.Solver (initialize, solver) where

import Z3.Monad
import qualified Data.Map as M
import qualified Presburger.Formula as P
import Debug.Trace (trace)

type Var = String
type Environment = M.Map Var Int

translate :: P.Formula -> Z3 AST
translate = auxF M.empty
  where
    auxF :: Environment -> P.Formula -> Z3 AST
    auxF _ P.FALSE = mkFalse
    auxF _ P.TRUE = mkTrue
    auxF env (P.Rel rel e₁ e₂) = do
      ast₁ <- auxE env e₁
      ast₂ <- auxE env e₂
      auxR rel ast₁ ast₂
    auxF env (P.And f₁ f₂) = do
      ast₁ <- auxF env f₁
      ast₂ <- auxF env f₂
      mkAnd [ast₁, ast₂]
    auxF env (P.Or f₁ f₂) = do
      ast₁ <- auxF env f₁
      ast₂ <- auxF env f₂
      mkOr [ast₁, ast₂]
    auxF env (P.Not f) = auxF env f >>= mkNot
    auxF env (P.Forall x f) = do
      sym <- mkStringSymbol x
      int <- mkIntSort
      ast <- auxF (M.insert x 0 (M.map (+ 1) env)) f
      mkForall [] [sym] [int] ast
    auxF env (P.Exists x f) = do
      sym <- mkStringSymbol x
      int <- mkIntSort
      ast <- auxF (M.insert x 0 (M.map (+ 1) env)) f
      mkExists [] [sym] [int] ast

    auxR :: P.Relation -> AST -> AST -> Z3 AST
    auxR P.RelEQ = mkEq
    auxR P.RelLE = mkLe
    auxR P.RelLT = mkLt

    auxE :: Environment -> P.Expression -> Z3 AST
    auxE _ (P.Const n) = mkIntNum (fromIntegral n)
    auxE env (P.Var x) | Just i <- M.lookup x env = do
                           int <- mkIntSort
                           mkBound i int
    auxE env (P.Add e₁ e₂) = do
      ast₁ <- auxE env e₁
      ast₂ <- auxE env e₂
      mkAdd [ast₁, ast₂]
    auxE env (P.Sub e₁ e₂) = do
      ast₁ <- auxE env e₁
      ast₂ <- auxE env e₂
      mkSub [ast₁, ast₂]
    auxE env (P.Mul n e) = do
      ast₁ <- mkIntNum (fromIntegral n)
      ast₂ <- auxE env e
      mkMul [ast₁, ast₂]

initialize :: IO ()
initialize = return ()

script :: P.Goal -> Z3 Bool
script (P.Implication vars f₁ f₂) = do
  ast <- translate (foldr P.Forall (P.Or (P.Not f₁) f₂) vars)
  assert ast
  res <- check
  case res of
    Sat -> return True
    _ -> return False

solver :: P.Goal -> IO Bool
solver = evalZ3 . script
