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

module Inference where

import Aux
import Language
import Exceptions
import qualified Runtime
import qualified Presburger.Formula as P
import qualified Type
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Monad.State.Lazy as ST
import Data.List (intersperse)
import Control.Monad (forM_, foldM, when, unless, liftM)
import Control.Exception
import Debug.Trace (trace)

type ConstraintSet = S.Set Constraint

type TypeEnvironment = M.Map Name Type
type ClassEnvironment = M.Map Name ClassType

data InferenceState
  = InferenceState {
    next :: Int,
    cons :: ConstraintSet
  }

type Inference a = ST.State InferenceState a

----------------------
-- MONAD OPERATIONS --
----------------------

newType :: Inference Type
newType = do
  state <- ST.get
  let n = next state
  ST.put (state { next = n + 1 })
  return (Var $ TVar n)

getType :: TypeEnvironment -> Name -> Inference (Type, TypeEnvironment)
getType env u | Just t <- M.lookup u env = return (t, M.empty)
getType _ u = do
  tᵤ <- newType
  return (tᵤ, M.singleton u tᵤ)

makeConstraint :: Type -> Type -> Constraint
makeConstraint t s | t == s = Trivial
makeConstraint (Var α) t = UpperBound α t
makeConstraint t s | Type.guarded t = LowerBound t s
makeConstraint t s = error $ "dangerous constraint " ++ show (LowerBound t s)

addConstraint :: Type -> Type -> Inference ()
addConstraint t s = ST.modify (\state -> state { cons = S.insert (makeConstraint t s) (cons state) })

----------------------------------
-- TYPE AND CLASS INSTANTIATION --
----------------------------------

-- these functions turn dummy type variables coming from the parser
-- into real type variables

makeType :: Type -> Inference Type
makeType = aux
  where
    aux t@(Ref _) = return t
    aux (Var _) = newType
    aux Zero = return Zero
    aux One = return One
    aux (Message tag ts) = do
      ts' <- mapM aux ts
      return $ Message tag ts'
    aux (t :+: s) = do
      t' <- aux t
      s' <- aux s
      return $ t' :+: s'
    aux (t :·: s) = do
      t' <- aux t
      s' <- aux s
      return $ t' :·: s'
    aux (Star t) = do
      t' <- aux t
      return $ Star t'

makeClass :: ClassType -> Inference ClassType
makeClass cls = liftM M.fromList (mapM aux (M.toList cls))
  where
    aux :: (Key, [Type]) -> Inference (Key, [Type])
    aux (κ, ts) = do
      ts' <- mapM makeType ts
      return (κ, ts')

---------------------------
-- CONSTRAINT GENERATION --
---------------------------

generate :: [(Name, Type)] -> U_Process -> (TypeDefinitions, Process, ConstraintSet)
generate udefs p =
  let initialState = InferenceState { next = 0, cons = S.empty }
      (q, state) = ST.runState (auxMain p) initialState
  in (tenv, q, cons state)
  where
    defs :: [(Name, Type)]
    defs = sdefs ++ udefs

    sdefs :: [(Name, Type)]
    sdefs =
      [ ("0", Zero),
        ("1", One),
        ("Number", numberType),
        ("String", stringType),
        ("Array", arrayType),
        ("Handle", handleType),
        ("Time", timeType),
        ("Bool", boolType)
      ]

    tenv :: TypeDefinitions
    tenv = M.fromListWithKey (throw . ErrorMultipleTypeDefinitions) defs

    auxE :: ClassEnvironment -> U_Expression -> Inference (Expression, Type, TypeEnvironment)
    auxE _ (U_Int n) = return (Int n, numberType, M.empty)
    auxE _ (U_Double d) = return (Double d, numberType, M.empty)
    auxE _ (U_String s) = return (String s, stringType, M.empty)
    auxE cenv (U_Name u) =
      case M.lookup u cenv of
        Nothing -> do
          tᵤ <- newType
          return (Name u tᵤ, tᵤ, M.singleton u tᵤ)
        Just cls -> error "first-class classes are currently unsupported"

    auxEs :: ClassEnvironment -> [U_Expression] -> Inference ([Expression], [Type], TypeEnvironment)
    auxEs cenv exprs = do
      ets <- mapM (auxE cenv) exprs
      let (exprs', ts, envs) = unzip3 ets
      let env = foldl merge M.empty envs
      return (exprs', ts, env)

    merge :: TypeEnvironment -> TypeEnvironment -> TypeEnvironment
    merge e₁ e₂ = M.unions [a, b, c]
      where
        a = M.difference e₁ e₂
        b = M.difference e₂ e₁
        c = M.intersectionWith (:·:) e₁ e₂

    meet :: TypeEnvironment -> TypeEnvironment -> TypeEnvironment
    meet e₁ e₂ = M.unions [a, b, c]
      where
        a = M.map (:+: One) (M.difference e₁ e₂)
        b = M.map (One :+:) (M.difference e₂ e₁)
        c = M.intersectionWith (:+:) e₁ e₂

    -- unbind u tₑ env removes u (with expected type tₑ) from env and
    -- adds the appropriate constraint
    unbind :: Name -> Type -> TypeEnvironment -> Inference TypeEnvironment
    unbind u tₑ env =
      case M.lookup u env of
        Nothing -> do
          addConstraint tₑ One
          return env
        Just t -> do
          addConstraint tₑ t
          return $ M.delete u env

    -- unbinds envₑ env removes all the assignments in envₑ from env
    -- and adds the appropriate constraints
    unbinds :: TypeEnvironment -> TypeEnvironment -> Inference TypeEnvironment
    unbinds envₑ env = foldM (\env (u, t) -> unbind u t env) env (M.toList envₑ)

    auxMain :: U_Process -> Inference Process
    auxMain p = do
      let tvars = S.unions (map Type.ftv (M.elems tenv))
      unless (S.null tvars) (throw ErrorFreeTypeVariables)
      unless (Type.contractive tenv) (throw ErrorNonContractiveTypes)
      (p', env) <- auxP Runtime.nativeClassEnvironment p
      unless (M.null env) (throw $ ErrorUndefinedNames $ M.keys env)
      return p'

    auxP :: ClassEnvironment -> U_Process -> Inference (Process, TypeEnvironment)
    auxP _ U_Null = return (Null, M.empty)
    auxP cenv (U_Send u tag exprs) =
      case M.lookup u cenv of
        Nothing -> do -- send message to dynamic object
          (exprs', ts, envs) <- auxEs cenv exprs
          -- tᵤ <- newType
          -- addConstraint tᵤ (Message tag ts)
          let t = Message tag ts
          return (Send u (Right t) tag exprs', merge (M.singleton u t) envs)
        Just cls -> do -- send message to class
          (exprs', ts, envs) <- auxEs cenv exprs
          let κ = Key tag (length exprs)
          case M.lookup κ cls of
            Nothing -> throw (ErrorMethodNotAvailable u κ)
            Just ets -> do
              forM_ (zip ts ets) (uncurry addConstraint)
              return (Send u (Left cls) tag exprs', envs)
    auxP cenv (U_Parallel p₁ p₂) = do
      (q₁, env₁) <- auxP cenv p₁
      (q₂, env₂) <- auxP cenv p₂
      return (Parallel q₁ q₂, merge env₁ env₂)
    auxP cenv (U_Object u (U_Static cls₀) rules p) = do
      cls <- makeClass (defaultClass cls₀ rules)
      let cenv' = M.insert u cls cenv
      rules' <- mapM (auxStaticRule cenv' u cls) rules
      (q, env) <- auxP cenv' p
      return (Object u (Static cls) rules' q, env)
    auxP cenv (U_Object u (U_Dynamic τ) rules p) = auxDynamicObject cenv u False τ rules p
    auxP cenv (U_Object u (U_Linear τ) rules p) = auxDynamicObject cenv u True τ rules p

    auxDynamicObject :: ClassEnvironment -> Name -> Bool -> Maybe Type ->
                        [U_Rule] -> U_Process ->
                        Inference (Process, TypeEnvironment)
    auxDynamicObject cenv u linear τ rules p = do
      let cenv' = M.delete u cenv
      tᵤ <- newType
      t <- makeType ((if linear then defaultLinearType else defaultType) τ rules)
      (rules', envs) <- liftM unzip (mapM (auxRule cenv' linear u tᵤ t) rules)
      let envᵣ = (if envs == [] then M.empty else foldl1 meet envs)
      (q, env) <- auxP cenv' p
      env <- unbind u tᵤ env
      addConstraint t tᵤ
      let kind = (if linear then Linear else Dynamic) tᵤ
      return (Object u kind rules' q, merge env envᵣ)

    keyOfPattern :: U_Pattern -> Key
    keyOfPattern (U_Pattern tag xs) = Key tag (length xs)

    keysOfJoinPattern :: [U_Pattern] -> S.Set Key
    keysOfJoinPattern msgs = M.keysSet m
      where
        m :: M.Map Key ()
        m = M.fromListWithKey (throw . ErrorNonLinearMessage) (zip keys (repeat ()))

        keys :: [Key]
        keys = map keyOfPattern msgs

    keysOfRule :: U_Rule -> S.Set Key
    keysOfRule (U_Rule msgs _ _) = keysOfJoinPattern msgs

    mapPattern :: ClassType -> U_Pattern -> (Pattern, TypeEnvironment)
    mapPattern cls (U_Pattern tag args) =
      case M.lookup (Key tag (length args)) cls of
        Nothing -> error $ "this should not happen " ++ show tag
        Just ts -> let args' = zip args ts
                       env = M.fromListWithKey (throw . ErrorNonLinearName) args'
                   in (Pattern tag args', env)

    mapPatterns :: ClassType -> [U_Pattern] -> ([Pattern], TypeEnvironment)
    mapPatterns cls msgs = (msgs', env)
      where
        (msgs', envs) = unzip (map (mapPattern cls) msgs)
        env = foldl (M.unionWithKey (throw . ErrorNonLinearName)) M.empty envs

    -- classOfPatterns t msgs computes the mapping from keys in msgs
    -- and argument types, provided that the mapping is uniquely
    -- determined by considering all the valid message configurations
    -- of t
    classOfPatterns :: Type -> [U_Pattern] -> ClassType
    classOfPatterns t msgs =
      case S.size mset₂ of
        n | n == 0 -> throw (ErrorTypeMismatch (Type.strip t) s)
        n | n > 1 -> throw (ErrorAmbiguousMatch s)
        _ -> M.fromListWithKey (throw . ErrorIncoherentType t) (S.elems $ S.findMin mset₂)
      where
        -- the set of keys corresponding to the join pattern
        keyset = keysOfJoinPattern msgs

        -- mset₁ is the set of molecules that contain *at least* the
        -- keys in keyset
        mset₁ = Type.filterMolecules keyset (Type.guardedSemantics tenv t)

        -- mset₂ is the set of sub-molecules of mset₁ corresponding to
        -- the keys in keyset
        mset₂ = S.map (Type.filterAtoms keyset) mset₁

        s = Type.typeOfMolecule $ S.map (\κ -> (κ, [])) keyset

    -- compute the default class of a static object
    defaultClass :: Maybe ClassType -> [U_Rule] -> ClassType
    defaultClass (Just cls) _ = cls
    defaultClass Nothing rules = M.fromList (map keyType keys)
      where
        keys :: [Key]
        keys = S.elems (S.unions (map keysOfRule rules))

        keyType :: Key -> (Key, [Type])
        keyType (Key tag arity) = (Key tag arity, replicate arity (Var $ TVar 0))

    defaultKeyType :: Key -> Type
    defaultKeyType (Key tag arity) = Message tag (replicate arity (Var $ TVar 0))

    -- compute the default type of a linear object
    defaultLinearType :: Maybe Type -> [U_Rule] -> Type
    defaultLinearType (Just t) _ = t
    defaultLinearType Nothing rules = Type.simplify tenv (foldl (:+:) One ts)
      where
        ts = [ defaultKeyType κ
             | κ <- S.elems $ S.unions $ map keysOfRule rules ]

    -- compute the default type of a non-linear object
    defaultType :: Maybe Type -> [U_Rule] -> Type
    defaultType (Just t) _ = t
    defaultType Nothing rules = Type.simplify tenv (foldl (:·:) One ts)
      where
        ts = [ Star (defaultKeyType κ)
             | κ <- S.elems $ S.unions $ map keysOfRule rules ]

    auxRule :: ClassEnvironment -> Bool -> Name -> Type -> Type -> U_Rule ->
               Inference (Rule, TypeEnvironment)
    auxRule cenv linear u tᵤ t₀ (U_Rule msgs guards p) = do
      let t = foldr (Type.removeTag tenv) t₀ guards
      let (msgs', envⱼ) = mapPatterns (classOfPatterns t msgs) msgs
      let cenv' = M.difference cenv envⱼ
      (q, env) <- auxP cenv' p
      -- compute the derivative of the type after consuming the
      -- matched molecule
      let tᵣ = Type.simplify tenv $ foldr (Type.derivative tenv . Type.deriveKey) t (map keyOfPattern msgs)
      let envᵣ = merge env (M.singleton u tᵣ) -- add tᵣ to the type of self
      env <- unbind u tᵤ envᵣ -- unbind self
      env <- unbinds envⱼ env -- unbind received names
      -- unless the object is linear, there must be no free names
      -- occurring in the body of the rules except for the name of the
      -- object (u) and the received ones (in envⱼ)
      unless (linear || M.null env) (throw $ ErrorUndefinedNames $ M.keys env)
      return (Rule msgs' guards q, env)

    auxStaticRule :: ClassEnvironment -> Name -> ClassType -> U_Rule ->
                     Inference Rule
    auxStaticRule cenv u cls (U_Rule msgs [] p) = do
      let (msgs', envⱼ) = mapPatterns cls msgs
      let cenv' = M.difference cenv envⱼ
      (q, env) <- auxP cenv' p
      env <- unbinds envⱼ env
      unless (M.null env) (throw $ ErrorUndefinedNames $ M.keys env)
      return (Rule msgs' [] q)

---------------------------
-- CONSTRAINT SATURATION --
---------------------------

saturate :: TypeDefinitions -> ConstraintSet -> ConstraintSet
saturate tenv = limit infer
  where
    infer :: ConstraintSet -> ConstraintSet
    infer cset = S.union cset (S.unions [ aux¹ boundm t s
                                        | LowerBound t s <- cs ])
      where
        cs = S.elems cset
        boundm = M.fromListWith (:+:) [ (α, t) | UpperBound α t <- cs ]

    -- derive all the constraints implied by t₁ ⊑ t₂ where t₁ is guarded
    aux¹ :: TypeInequations -> Type -> Type -> ConstraintSet
    aux¹ boundm t₁ t₂
      = S.unions [ aux² t₁ mol
                 | mol <- S.elems $ Type.semantics tenv boundm t₂ ]

    -- derive all the constraints implied by comparing the semantics
    -- of t₁ (tsem) and (part of) a molecule of t₂ (mol₂)
    aux² :: Type -> Type.Molecule -> ConstraintSet
    aux² t mol | S.null matched = throw err
               | otherwise = S.unions (map (aux³ mol) (S.elems matched))
      where
        err = ErrorTypeMismatch (Type.strip t) (Type.strip $ Type.typeOfMolecule mol)
        matched :: Type.Semantics
        matched = Type.filterMolecules (S.map fst mol) (Type.guardedSemantics tenv t)

    -- derive all the constraints implied by comparing (part of) a
    -- molecule of t₂ (mol₂) with (part of) a molecule of t₁ (mol₁)
    -- such that mol₁ and mol₂ have the same signature
    aux³ :: Type.Molecule -> Type.Molecule -> ConstraintSet
    aux³ mol₂ mol₁ = S.fromList (map (uncurry makeConstraint) cs)
      where
        cs = concat [ zip ts₂ ts₁ | (κ₁, ts₁) <- S.elems mol₁, (κ₂, ts₂) <- S.elems mol₂, κ₁ == κ₂ ]

------------------------------
-- UNBOUNDED TYPE VARIABLES --
------------------------------
-- compute the set of type variables that have no upper bound

checkBounds :: ConstraintSet -> (S.Set TVar, ConstraintSet)
checkBounds cset = (uvars, S.union cset (S.map (\α -> UpperBound α One) uvars))
  where
    uvars :: S.Set TVar
    uvars = Type.unboundedTypeVariables bounds tvars

    cs :: [Constraint]
    cs = S.elems cset

    bounds :: TypeInequations
    bounds = M.fromListWith (:+:) [ (α, t) | UpperBound α t <- cs ]

    tvars :: S.Set TVar
    tvars = S.unions (map constraintVariables cs)

    constraintVariables :: Constraint -> S.Set TVar
    constraintVariables Trivial = S.empty
    constraintVariables (UpperBound α t) = S.insert α (Type.ftv t)
    constraintVariables (LowerBound t s) = S.union (Type.ftv t) (Type.ftv s)

---------------------------
-- CONSTRAINT RESOLUTION --
---------------------------
-- compute upper bounds and check lower bounds

resolve :: (P.Goal -> IO Bool) -- ^ the solver of subtype relations
        -> Bool                -- ^ constraints are expanded lazily?
        -> Bool                -- ^ verbose?
        -> TypeDefinitions     -- ^ type definitions
        -> ConstraintSet       -- ^ the whole set of constraints
        -> IO TypeInequations
resolve solver lazy verbose tenv cset₀ = do
  forM_ [ (t, s) | LowerBound t s <- S.elems cset ] (uncurry checkConstraint)
  return solution
  where
    cset = if lazy then cset₀ else S.map mapConstraint cset₀

    checkConstraint = if lazy then checkUnexpandedConstraint
                      else checkExpandedConstraint

    checkUnexpandedConstraint t s = do
      when verbose
        (do putStrLn $ show (LowerBound t s)
            putStrLn $ "  which expands to")
      checkExpandedConstraint (mapType t) (mapType s)

    checkExpandedConstraint t s = do
      when verbose (putStrLn $ show (LowerBound t s))
      res <- solver (Type.subtype tenv t s)
      unless res (throw $ ErrorTypeMismatch (Type.strip t) (Type.strip s))

    mapType :: Type -> Type
    mapType = Type.substAll False solution

    mapConstraint :: Constraint -> Constraint
    mapConstraint Trivial = Trivial
    mapConstraint (UpperBound α t) = UpperBound α (mapType t)
    mapConstraint (LowerBound t s) = LowerBound (mapType t) (mapType s)

    system :: TypeInequations
    system = M.fromListWith (:+:) [ (α, t) | UpperBound α t <- S.elems cset₀ ]

    -- phase 1: we resolve each equation from top to bottom and
    -- substitute as we move forward
    solution₁ :: TypeInequations
    solution₁ =
      foldr (\(α, t) m ->
                let s = Type.resolve tenv α (Type.substAll False m t)
                in M.insert α s m) M.empty (M.toAscList system)

    -- phase 2: we substitute each equation from bottom to top
    solution₂ :: TypeInequations
    solution₂ =
      foldr (\(α, t) m ->
                let s = Type.substAll False m t
                in M.insert α s m) M.empty (M.toDescList solution₁)

    -- the solution is the simplification of phase 2
    solution :: TypeInequations
    solution = M.map (Type.simplify tenv) solution₂
