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

module Presburger.Lash.Solver (initialize, solver) where

import Presburger.Formula
import qualified Presburger.Lash.Automaton as Lash
import qualified Data.List as List

type Var = String
type Vars = [Var]
type CompiledExpression = ([Int], Int)

variableIndex :: Var -> Vars -> Int
variableIndex x vars | Just n <- List.elemIndex x vars = n
                     | otherwise = error $ "undefined variable " ++ x

compileExpr :: Vars -> Expression -> ([Int], Int)
compileExpr vars (Const n) = (map (const 0) vars, n)
compileExpr vars (Var x) =
  case variableIndex x vars of
    _ -> (map (\y -> if x == y then 1 else 0) vars, 0)
compileExpr vars (Add e f) =
  let (v, c) = compileExpr vars e
      (w, d) = compileExpr vars f
  in (map (uncurry (+)) (zip v w), c + d)
compileExpr vars (Sub e f) =
  let (v, c) = compileExpr vars e
      (w, d) = compileExpr vars f
  in (map (uncurry (-)) (zip v w), c - d)
compileExpr vars (Mul n e) =
  let (v, c) = compileExpr vars e in (map (n *) v, n * c)

compileRel :: Vars -> Relation -> Expression -> Expression -> (Relation, CompiledExpression)
compileRel vars RelEQ e f = (RelEQ, compileExpr vars (Sub e f))
compileRel vars RelLE e f = (RelLE, compileExpr vars (Sub e f))
compileRel vars RelLT e f = compileRel vars RelLE (Add e (Const 1)) f

compileFormula :: Vars -> Formula -> IO Lash.NDD
compileFormula vars FALSE = Lash.empty (length vars)
compileFormula vars TRUE = Lash.universe (length vars)
compileFormula vars (Rel rel e f) =
  case compileRel vars rel e f of
    (RelEQ, (v, c)) -> Lash.fromEquation v (-c)
    (RelLE, (v, c)) -> Lash.fromInequation v (-c)
    _ -> error "impossible"
compileFormula vars (And p q) = do
  a <- compileFormula vars p
  b <- compileFormula vars q
  Lash.intersection a b
compileFormula vars (Or p q) = do
  a <- compileFormula vars p
  b <- compileFormula vars q
  Lash.union a b
compileFormula vars (Not p) = do
  u <- compileFormula vars TRUE
  a <- compileFormula vars p
  Lash.difference u a
compileFormula vars (Exists x p) = do
  a <- compileFormula (x : vars) p
  Lash.projection a 0
compileFormula vars (Forall x p) = compileFormula vars (Not (Exists x (Not p)))

initialize :: IO ()
initialize = Lash.initialize

solver :: Goal -> IO Bool
solver (Implication vars f₁ f₂) = do
  ndd₁ <- compileFormula vars f₁
  ndd₂ <- compileFormula vars f₂
  Lash.isIncludedIn ndd₁ ndd₂
