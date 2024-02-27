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

module DeadlockAnalysis where

import Language
import qualified Type
import Exceptions
import Data.List (groupBy)
import qualified Data.Set as S
import qualified Dependencies as D
import Control.Monad (liftM, forM_)
import Control.Exception

type NameSet = S.Set Name
type Deps = D.DependencyRelation Name

basicType :: Type -> Bool
basicType (Basic _) = True
basicType One = True
basicType _ = False

checkDisjointness :: [Name] -> IO ()
checkDisjointness us | null dups = return ()
                     | otherwise = throw (ErrorIncompatibleDependencies dups)
  where
    dups = concat $ filter (\vs -> length vs > 1) (groupBy (==) us)

checkCompatibility :: Deps -> Deps -> IO Deps
checkCompatibility deps₁ deps₂ =
  case D.disjointUnion deps₁ deps₂ of
    Right deps -> return deps
    Left us -> throw (ErrorIncompatibleDependencies us)

checkLiveness :: TypeDefinitions -> TypeEquations -> Type -> [Rule] -> IO ()
checkLiveness tenv solution t rules = forM_ (S.toList unmatched) checkMolecule
  where
    guard :: Type -> Type
    guard = Type.substAll False solution

    checkMolecule :: Type.Molecule -> IO ()
    checkMolecule mol = forM_ (S.toList mol) checkAtom
      where
        checkAtom :: Type.Atom -> IO ()
        checkAtom (κ, ts) = forM_ ts (checkType κ)

        checkType :: Key -> Type -> IO ()
        checkType κ t | irrelevant s = return ()
                      | otherwise = throw (ErrorRelevantArgument κ s)
          where
            s = guard t

        -- irrelevant t <==> t ≤ 1
        irrelevant :: Type -> Bool
        irrelevant (Basic _) = True
        irrelevant Zero = False
        irrelevant One = True
        irrelevant (Ref tag) = irrelevant (Type.expand tenv tag)
        irrelevant (Message _ _) = False
        irrelevant (t₁ :+: t₂) = irrelevant t₁ || irrelevant t₂
        irrelevant (t₁ :·: t₂) = irrelevant t₁ && irrelevant t₂
        irrelevant (Star _) = True
        irrelevant t = error $ "cannot decide whether " ++ show t ++ " is irrelevant"

    -- set of molecules
    sem :: Type.Semantics
    sem = Type.semantics tenv solution t

    -- set of molecules that do not fire any reaction
    unmatched :: Type.Semantics
    unmatched = S.difference sem matched

    -- set of molecules that fire at least one reaction
    matched :: Type.Semantics
    matched = S.unions [ Type.filterMolecules keys sem
                       | keys <- keysOfRules rules ]

    keyOfPattern :: Pattern -> Key
    keyOfPattern (Pattern tag xs) = Key tag (length xs)

    keysOfJoinPattern :: [Pattern] -> S.Set Key
    keysOfJoinPattern msgs = S.fromList (map keyOfPattern msgs)

    keysOfRule :: Rule -> S.Set Key
    keysOfRule (Rule msgs _ _) = keysOfJoinPattern msgs

    keysOfRules :: [Rule] -> [S.Set Key]
    keysOfRules = map keysOfRule

check :: TypeDefinitions -> TypeEquations -> Process -> IO ()
check tenv solution p = auxP p >> return ()
  where
    auxP :: Process -> IO (NameSet, Deps)
    auxP Null = return (S.empty, D.empty)
    auxP (Send _ (Left cls) _ exprs) = do
      us <- auxEs exprs
      checkDisjointness us
      return (S.fromList us, D.fromList us)
    auxP (Send u (Right _) _ exprs) = do
      us <- auxEs exprs
      let us' = u : us
      checkDisjointness us'
      return (S.fromList us', D.fromList us')
    auxP (Parallel p₁ p₂) = do
      (names₁, deps₁) <- auxP p₁
      (names₂, deps₂) <- auxP p₂
      deps <- checkCompatibility deps₁ deps₂
      return (S.union names₁ names₂, deps)
    auxP (Object _ (Static _) rules p) = do
      _ <- mapM auxRule rules
      auxP p
    auxP (Object u (Dynamic t) rules p) = do
      _ <- mapM auxRule rules
      (names, deps) <- auxP p
      checkLiveness tenv solution t rules
      return (S.delete u names, D.delete u deps)
    auxP (Object u (Linear t) rules p) = do
      uset <- liftM S.unions (mapM auxRule rules)
      (names, deps) <- auxP p
      let depsᵤ = D.fromSet (S.insert u uset)
      deps' <- checkCompatibility depsᵤ deps
      checkLiveness tenv solution t rules
      return (S.delete u names, D.delete u deps')

    receivedNames :: [Pattern] -> NameSet
    receivedNames = S.unions . map aux
      where
        aux (Pattern _ nts) = S.fromList (map fst nts)

    auxRule :: Rule -> IO NameSet
    auxRule (Rule msgs _ p) = do
      (names, _) <- auxP p
      return $ S.difference names (receivedNames msgs)

    auxE :: Expression -> IO [Name]
    auxE (Name u t) = do
      let s = Type.substAll False solution t
      -- putStrLn $ "checking type of " ++ u ++ " " ++ show s
      return (if basicType s then [] else [u])
    auxE _ = return []

    auxEs :: [Expression] -> IO [Name]
    auxEs = liftM concat . mapM auxE
