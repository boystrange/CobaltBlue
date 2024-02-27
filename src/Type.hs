{-# LANGUAGE MonadComprehensions #-}
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

module Type
  ( ftv
  , guarded
  , strip
  , contractive
  , expand
  , subst
  , substAll
  , Semantics
  , Molecule
  , Atom
  , typeOfMolecule
  , semantics
  , guardedSemantics
  , simplify
  , subtype
  , deriveKey
  , derivative
  , removeTag
  , resolve
  , filterMolecules
  , filterAtoms
  , unboundedTypeVariables
  , refold
  )
where

import Aux
import Language
import Exceptions
import Data.Tuple (swap)
import qualified Presburger.Formula as P
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import qualified Control.Monad.State.Lazy as ST
import Control.Exception
import Debug.Trace (trace)

-------------------------------
-- BASIC OPERATIONS ON TYPES --
-------------------------------

ftv :: Type -> S.Set TVar
ftv Zero = S.empty
ftv One = S.empty
ftv (Basic _) = S.empty
ftv (Ref _) = S.empty
ftv (Var α) = S.singleton α
ftv (Message _ ts) = S.unions (map ftv ts)
ftv (t :+: s) = S.union (ftv t) (ftv s)
ftv (t :·: s) = S.union (ftv t) (ftv s)
ftv (Star t) = ftv t

-- rtv m t computes the reachable type variables starting from those
-- in t and including all of those in the corresponding definitions
-- given by m
rtv :: TypeEquations -> Type -> S.Set TVar
rtv m t = limit closure (ftv t)
  where
    closure :: S.Set TVar -> S.Set TVar
    closure tvars = S.union tvars (reachable tvars)

    reachable :: S.Set TVar -> S.Set TVar
    reachable tvars = S.unions (map ftv (M.elems (restrictKeys m tvars)))

    restrictKeys m tvars = M.filterWithKey (\tvar _ -> S.member tvar tvars) m

-- t is guarded if all the type variables in t occur within message
-- type constructors
guarded :: Type -> Bool
guarded Zero = True
guarded One = True
guarded (Basic _) = True
guarded (Ref _) = True
guarded (Var _) = False
guarded (Message _ _) = True
guarded (t :+: s) = guarded t && guarded s
guarded (t :·: s) = guarded t && guarded s
guarded (Star t) = guarded t

-- strip t removes all arguments from message types in t
strip :: Type -> Type
strip (Message tag ts) = Message tag []
strip (t :+: s) = strip t :+: strip s
strip (t :·: s) = strip t :·: strip s
strip (Star t) = Star (strip t)
strip t = t

substAll :: Bool -> TypeEquations -> Type -> Type
substAll deep m = aux
  where
    aux (Var α) | Just t <- M.lookup α m = aux t
    aux (Message tag ts) | deep = Message tag (map aux ts)
    aux (s₁ :·: s₂) = aux s₁ :·: aux s₂
    aux (s₁ :+: s₂) = aux s₁ :+: aux s₂
    aux (Star s) = Star (aux s)
    aux s = s

subst :: Bool -> TVar -> Type -> Type -> Type
subst deep α t = substAll deep (M.singleton α t)

removeTag :: TypeDefinitions -> Tag -> Type -> Type
removeTag tenv tag = aux
  where
    aux Zero = Zero
    aux One = One
    aux (Ref x) = aux (expand tenv x)
    aux (Var _) = error "not implemented"
    aux t@(Message tag' _) | tag == tag' = Zero
                           | otherwise = t
    aux (t :+: s) = aux t :+: aux s
    aux (t :·: s) = aux t :·: aux s
    aux (Star t) = Star (aux t)

derivative :: TypeDefinitions -> (Type -> Type) -> Type -> Type
derivative tenv d = aux
  where
    aux (Ref x) = aux (expand tenv x)
    aux (t :·: s) = (aux t :·: s) :+: (t :·: aux s)
    aux (t :+: s) = aux t :+: aux s
    aux (Star t) = aux t :·: (Star t)
    aux t = d t

deriveKey :: Key -> Type -> Type
deriveKey κ (Message tag ts) | κ == Key tag (length ts) = One
deriveKey _ _ = Zero

deriveVar :: TVar -> Type -> Type
deriveVar α (Var β) | α == β = One
deriveVar _ _ = Zero

resolve :: TypeDefinitions -> TVar -> Type -> Type
resolve tenv α t = subst False α t₀ (Star $ derivative tenv (deriveVar α) t) :·: t₀
  where
    t₀ = subst False α Zero t

expand :: TypeDefinitions -> Name -> Type
expand tenv x | Just t <- M.lookup x tenv = t
expand _ x = throw (ErrorUndefinedType x)

-------------------------
-- TYPE SIMPLIFICATION --
-------------------------

-- guiding principle 1: no operator is distributed
-- guiding principle 2: shallow simplification only
simplify :: TypeDefinitions -> Type -> Type
simplify tenv = aux
  where
    aux Zero = Zero
    aux One = One
    aux t@(Basic _) = t
    aux t@(Var _) = t
    aux t@(Message _ _) = t
    aux (Ref tag) = aux (expand tenv tag)
    aux t@(_ :+: _) | null ts = Zero
                    | otherwise = foldl1 (:+:) ts
      where
        ts₀ = S.elems (auxS t)
        ts = if any unlimited ts₀ then filter (/= One) ts₀ else ts₀
    aux (t₁ :·: t₂) =
      case (aux t₁, aux t₂) of
        (Zero, _) -> Zero
        (_, Zero) -> Zero
        (One, s₂) -> s₂
        (s₁, One) -> s₁
        (Basic name₁, Basic name₂) | name₁ == name₂ -> Basic name₁
        (s₁, s₂) | s₁ == s₂ && unlimited s₁ -> s₁
        (s₁, s₂) -> s₁ :·: s₂
    aux (Star t) =
      case aux t of
        Zero -> One
        One -> One
        s | unlimited s -> s
        s -> Star s

    auxS :: Type -> S.Set Type
    auxS Zero = S.empty
    auxS (t₁ :+: t₂) = S.union (auxS (aux t₁)) (auxS (aux t₂))
    auxS t = S.singleton t

    -- unlimited t <==> t = t·t ∧ t ≠ 1
    unlimited :: Type -> Bool
    unlimited (Basic _) = True
    unlimited (Star _) = True
    unlimited (t₁ :·: t₂) = unlimited t₁ && unlimited t₂
    unlimited _ = False

----------------------
-- FINITE SEMANTICS --
----------------------

type Atom = (Key, [Type])
type Molecule = S.Set Atom
type Semantics = S.Set Molecule

semantics :: TypeDefinitions -> TypeInequations -> Type -> Semantics
semantics tenv bounds = aux S.empty
  where
    aux _ Zero = S.empty -- XXX DANGER XXX
    aux _ One = S.singleton S.empty
    aux _ (Basic _) = S.singleton S.empty -- XXX DANGER XXX
    aux _ (Message tag ts) = S.singleton (S.singleton (Key tag (length ts), ts))
    aux visited (Ref tag) = aux visited (expand tenv tag)
    aux visited (Var α) | S.member α visited = S.singleton S.empty -- XXX DANGER XXX
                        | Just t <- M.lookup α bounds = aux (S.insert α visited) t
                        | otherwise = S.singleton S.empty -- XXX DANGER XXX
    aux visited (t :+: s) = S.union (aux visited t) (aux visited s)
    aux visited (t :·: s) = S.fromList [ S.union x y
                                       | x <- S.elems (aux visited t),
                                         y <- S.elems (aux visited s) ]
    aux visited (Star t) = limit closure (S.insert S.empty (aux visited t))

    closure :: Semantics -> Semantics
    closure zset = S.fromList [ S.union x y | let zs = S.elems zset, x <- zs, y <- zs ]

guardedSemantics :: TypeDefinitions -> Type -> Semantics
guardedSemantics tenv t | guarded t = semantics tenv M.empty t
                        | otherwise = error $ "cannot compute guarded semantics of " ++ show t

-- filterMolecules keyset mset returns the subset of mset that contain
-- (at least) all the keys in keyset
filterMolecules :: S.Set Key -> Semantics -> Semantics
filterMolecules keyset = S.filter (\mol -> S.isSubsetOf keyset (S.map keyOfAtom mol))

-- filterAtoms keyset mol returns the sub-molecule of mol that
-- contains only those atoms whose key can be found in keyset
filterAtoms :: S.Set Key -> Molecule -> Molecule
filterAtoms keyset = S.filter (\atom -> S.member (keyOfAtom atom) keyset)

keyOfAtom :: Atom -> Key
keyOfAtom = fst

typeOfAtom :: Atom -> Type
typeOfAtom (Key tag _, ts) = Message tag ts

typeOfMolecule :: Molecule -> Type
typeOfMolecule mol | S.null mol = One
                   | otherwise = foldl1 (:·:) (map typeOfAtom (S.elems mol))

-------------------------
-- UNBOUNDED VARIABLES --
-------------------------

-- bounded tvars t determines whether t is bounded, assuming that all
-- of the type variables in tvars are unbounded
bounded :: S.Set TVar -> Type -> Bool
bounded tvars = aux
  where
    aux Zero = False
    aux One = True
    aux (Basic _) = True
    aux (Ref _) = True
    aux (Message _ _) = True
    aux (Var α) = not (S.member α tvars)
    aux (t :+: s) = aux t || aux s
    aux (t :·: s) = aux t && aux s
    aux (Star _) = True

unboundedTypeVariables :: TypeInequations -> S.Set TVar -> S.Set TVar
unboundedTypeVariables bounds = limit loop
  where
    loop :: S.Set TVar -> S.Set TVar
    loop tvars = S.difference tvars (boundedTypeVariables tvars)

    boundedTypeVariables :: S.Set TVar -> S.Set TVar
    boundedTypeVariables tvars =
      S.filter (\α -> case M.lookup α bounds of
                        Nothing -> False
                        Just t -> bounded tvars t) tvars

-----------------------
-- SHALLOW SUBTYPING --
-----------------------

type Vector = MS.MultiSet Key
type Base = Vector
type Period = Vector
type LinearSet = (Base, S.Set Period)
type SemiLinearSet = S.Set LinearSet

signature :: TypeDefinitions -> Type -> S.Set Key
signature tenv = aux
  where
    aux Zero = S.empty
    aux One = S.empty
    aux (Ref tag) = aux (expand tenv tag)
    aux (Var α) = error $ "signature of " ++ show α ++ " is undefined"
    aux (Message tag αs) = S.singleton (Key tag (length αs))
    aux (p :·: q) = S.union (aux p) (aux q)
    aux (p :+: q) = S.union (aux p) (aux q)
    aux (Star p) = aux p

normalize :: TypeDefinitions -> Type -> SemiLinearSet
normalize tenv = aux
  where
    aux Zero = S.empty
    aux One = semiLinearSetOne
    aux (Ref x) = aux (expand tenv x)
    aux (Message tag αs) = S.singleton (MS.singleton (Key tag (length αs)), S.empty)
    aux (t :+: s) = S.union (aux t) (aux s)
    aux (t :·: s) = semiLinearSetProduct (aux t) (aux s)
    aux (Star t) = S.foldr semiLinearSetProduct semiLinearSetOne (S.map linearSetStar (aux t))
    aux t = error $ "cannot normalize " ++ show t

    linearSetProduct :: LinearSet -> LinearSet -> LinearSet
    linearSetProduct (base₁, periods₁) (base₂, periods₂) =
      (MS.union base₁ base₂, S.union periods₁ periods₂)

    semiLinearSetProduct :: SemiLinearSet -> SemiLinearSet -> SemiLinearSet
    semiLinearSetProduct slset₁ slset₂ =
      S.fromList [ linearSetProduct lset₁ lset₂ | lset₁ <- S.toList slset₁, lset₂ <- S.toList slset₂ ]

    linearSetQuasiStar :: LinearSet -> LinearSet
    linearSetQuasiStar (base, periods) = (base, S.union periods (S.singleton base))

    linearSetOne :: LinearSet
    linearSetOne = (MS.empty, S.empty)

    semiLinearSetOne :: SemiLinearSet
    semiLinearSetOne = S.singleton linearSetOne

    linearSetStar :: LinearSet -> SemiLinearSet
    linearSetStar lset = S.insert linearSetOne (S.singleton (linearSetQuasiStar lset))

tupleOfVector :: [Key] -> Vector -> [Int]
tupleOfVector κs v = map (\κ -> MS.occur κ v) κs

tupleSum :: [P.Expression] -> [P.Expression] -> [P.Expression]
tupleSum tuple₁ tuple₂ = map (uncurry P.Add) (zip tuple₁ tuple₂)

tupleMul :: String -> [Int] -> [P.Expression]
tupleMul x = map (\n -> P.Mul n (P.Var x))

formulaOfLinearSet :: [Key] -> LinearSet -> P.Formula
formulaOfLinearSet κs (base, periods) = foldr P.Exists conjunction vars
  where
    vars :: [String]
    vars = map (\i -> "$" ++ show i) [1..length periods]

    baseTuple :: [P.Expression]
    baseTuple = map P.Const (tupleOfVector κs base)

    periodsTuples :: [[P.Expression]]
    periodsTuples = map (\(x, vec) -> tupleMul x (tupleOfVector κs vec)) (zip vars (S.elems periods))

    tuple :: [P.Expression]
    tuple = foldl tupleSum baseTuple periodsTuples

    equalities :: [P.Formula]
    equalities = map (\(κ, expr) -> P.Rel P.RelEQ (P.Var $ show κ) expr) (zip κs tuple)

    constraints :: [P.Formula]
    constraints = map (\x -> P.Rel P.RelLE (P.Const 0) (P.Var x)) vars

    conjunction :: P.Formula
    conjunction = foldl P.And (foldl P.And P.TRUE constraints) equalities

linearSetSize :: LinearSet -> Int
linearSetSize (_, periods) = 1 + S.size periods

formulaOfSemiLinearSet :: [Key] -> SemiLinearSet -> P.Formula
formulaOfSemiLinearSet κs = S.foldr (P.Or . formulaOfLinearSet κs) P.FALSE

trivialGoal :: Bool -> P.Goal
trivialGoal b = P.Implication [] (formulaOfBool (not b)) (formulaOfBool b)
  where
    formulaOfBool False = P.FALSE
    formulaOfBool True = P.TRUE

isObjectType :: TypeDefinitions -> Type -> Bool
isObjectType tenv = aux
  where
    aux :: Type -> Bool
    aux (Var _) = False
    aux (Basic _) = False
    aux (Ref x) = aux (expand tenv x)
    aux Zero = True
    aux One = True
    aux (Message _ _) = True
    aux (t₁ :+: t₂) = aux t₁ && aux t₂
    aux (t₁ :·: t₂) = aux t₂ && aux t₂
    aux (Star t) = aux t

subtype :: TypeDefinitions -> Type -> Type -> P.Goal
subtype tenv t₁ t₂ =
  case (simplify tenv t₁, simplify tenv t₂) of
    (_, Zero) -> trivialGoal True
    (Basic _, One) -> trivialGoal True
    (Basic name₁, Basic name₂) -> trivialGoal (name₁ == name₂)
    (_, _) | isObjectType tenv t₁ && isObjectType tenv t₂ ->
               P.Implication (map show κs) f₂ f₁
      where
        f₁ = formulaOfSemiLinearSet κs (normalize tenv t₁)
        f₂ = formulaOfSemiLinearSet κs (normalize tenv t₂)
        κs = S.elems (S.union (signature tenv t₁) (signature tenv t₂))
    _ -> trivialGoal False

---------------------------
-- REGULAR TREE EQUALITY --
---------------------------

find :: TypeEquations -> Type -> Type
find m (Var α) | Just t <- M.lookup α m = find m t
find _ t = t

equal :: TypeEquations -> Type -> Type -> Bool
equal m = aux S.empty
  where
    aux :: S.Set (Type, Type) -> Type -> Type -> Bool
    aux mem t s | S.member (t, s) mem = True
    aux mem t s = auxT (S.insert (t, s) mem) (find m t) (find m s)

    auxT :: S.Set (Type, Type) -> Type -> Type -> Bool
    auxT _ Zero Zero = True
    auxT _ One One = True
    auxT _ (Ref tag₁) (Ref tag₂) = tag₁ == tag₂
    auxT _ (Var α) (Var β) = α == β
    auxT mem (Message tag₁ ts₁) (Message tag₂ ts₂) =
      tag₁ == tag₂ && length ts₁ == length ts₂ && and (map (uncurry (aux mem)) (zip ts₁ ts₂))
    auxT mem (t₁ :+: t₂) (s₁ :+: s₂) = aux mem t₁ s₁ && aux mem t₂ s₂
    auxT mem (t₁ :·: t₂) (s₁ :·: s₂) = aux mem t₁ s₁ && aux mem t₂ s₂
    auxT mem (Star t) (Star s) = aux mem t s
    auxT _ _ _ = False

---------------
-- REFOLDING --
---------------

remap :: (Type, TypeEquations) -> (Type, TypeEquations)
remap (t, m) = (substAll True rename t, m')
  where
    m' = M.fromList (zip (map TVar [0..]) (map (substAll True rename) (M.elems m)))
    rename = M.fromList (zip (M.keys m) (map (Var . TVar) [0..]))

data RefoldState
  = RefoldState { next :: Int,
                  reverseMap :: M.Map Type TVar,
                  forwardMap :: M.Map TVar Type }

type Refold a = ST.State RefoldState a

getNextVar :: Refold TVar
getNextVar = do
  state <- ST.get
  ST.put (state { next = next state + 1 })
  return (TVar $ next state)

getTree :: Type -> Refold (TVar, Maybe Type)
getTree t = do
  state <- ST.get
  case M.lookup t (reverseMap state) of
    Just α -> return (α, M.lookup α (forwardMap state))
    Nothing -> do
      α <- getNextVar
      ST.modify (\state -> state { reverseMap = M.insert t α (reverseMap state) })
      return (α, Nothing)

addForwardBinding :: TVar -> Type -> Refold ()
addForwardBinding α t =
  ST.modify (\state -> state { forwardMap = M.insert α t (forwardMap state) })

refold :: TypeEquations -> Type -> (Type, TypeEquations)
refold m t = let initialState = RefoldState { next = 0,
                                              forwardMap = M.empty,
                                              reverseMap = M.empty }
                 (s, state) = ST.runState (aux S.empty t) initialState
                 tvarset = rtv (forwardMap state) s
             in remap (s, M.filterWithKey (\α _ -> S.member α tvarset) (forwardMap state))
  where
    aux :: S.Set TVar -> Type -> Refold Type
    aux visiting t = do
      (α, τ) <- getTree t
      case (S.member α visiting, τ) of
        (True, _) -> return (Var α)
        (_, Just s) -> return s
        (_, Nothing) -> do s <- auxT (S.insert α visiting) (find m t)
                           addForwardBinding α s
                           return (if S.member α (ftv s) then (Var α) else s)

    auxT _ Zero = return Zero
    auxT _ One = return One
    auxT _ t@(Ref _) = return t
    auxT _ (Var _) = error "not implemented"
    auxT visiting (Message tag ts) = do
      ts' <- mapM (aux visiting) ts
      return $ Message tag ts'
    auxT visiting (t :+: s) = do
      t' <- aux visiting t
      s' <- aux visiting s
      return $ t' :+: s'
    auxT visiting (t :·: s) = do
      t' <- aux visiting t
      s' <- aux visiting s
      return $ t' :·: s'
    auxT visiting (Star t) = do
      t' <- aux visiting t
      return $ Star t'

----------------------
-- TYPE DEFINITIONS --
----------------------

contractive :: TypeDefinitions -> Bool
contractive tenv = all (auxT S.empty . Ref) (M.keys tenv)
  where
    auxT :: S.Set Name -> Type -> Bool
    auxT _ Zero = True
    auxT _ One = True
    auxT _ (Basic _) = True
    auxT _ (Message _ _) = True
    auxT visited (Ref x) | S.member x visited = False
    auxT visited (Ref x) =
      case M.lookup x tenv of
        Nothing -> throw (ErrorUndefinedType x)
        Just t -> auxT (S.insert x visited) t
    auxT visited (t :+: s) = auxT visited t && auxT visited s
    auxT visited (t :·: s) = auxT visited t && auxT visited s
    auxT visited (Star t) = auxT visited t
