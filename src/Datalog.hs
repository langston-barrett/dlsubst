{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Datalog where

import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Traversable (for)

newtype Program
  = Program (Set Rule)
  deriving (Eq, Ord, Show)

data Rule 
  = Rule
    { rHead :: Atom
    , body :: Set Atom
    }
  deriving (Eq, Ord, Show)

data Atom
  = Atom
    { rel :: Rel
    , terms :: [Term]
    }
  deriving (Eq, Ord, Show)

newtype Rel = Rel String
  deriving (Eq, Ord, Show)

data Term
  = TConst Const
  | TVar Var
  deriving (Eq, Ord, Show)

newtype Const = Const String
  deriving (Eq, Ord, Show)

newtype Var = Var String
  deriving (Eq, Ord, Show)

newtype Subst = Subst (Map Var Const)
  deriving (Eq, Monoid, Ord, Semigroup, Show)

singletonSubst :: Var -> Const -> Subst
singletonSubst v c = Subst (Map.singleton v c)

-- Union of substitutions; @Nothing@ when they conflict
union :: Subst -> Subst -> Maybe Subst
union (Subst sub1) (Subst sub2) = Subst <$> unionMatch sub1 sub2
  where
    unionMatch :: 
      Ord a =>
      Eq b =>
      Map a b -> 
      Map a b -> 
      Maybe (Map a b)
    unionMatch = unionMaybe (\x y -> if x == y then Just x else Nothing)

    unionMaybe :: 
      Ord a =>
      (b -> b -> Maybe b) ->
      Map a b ->
      Map a b ->
      Maybe (Map a b)
    unionMaybe merge m n =
      sequence $
        Map.unionWith
          -- HACK
          (\x y -> merge (Maybe.fromJust x) (Maybe.fromJust y))
          (Map.map Just m)
          (Map.map Just n)

newtype Tuple = Tuple [Const]
  deriving (Eq, Ord, Show)

unTuple :: Set Tuple -> Set [Const]
unTuple = Set.map coerce -- why not just coerce?

tupleArity :: Tuple -> Int
tupleArity (Tuple t) = length t

-- invariant: all the lists are the same length, the arity of the rel
newtype Db = Db (Map Rel (Set Tuple))
  deriving (Eq, Show)

unify :: Term -> Const -> Subst -> Maybe Subst
unify term c (Subst sub) =
  case term of
    TConst c' | c == c' -> Just (Subst sub)
    TConst _ -> Nothing
    TVar var ->
      case Map.lookup var sub of
        Just c' | c == c' -> Just (Subst sub)
        Just _ -> Nothing
        Nothing -> Just (Subst (Map.insert var c sub))

unifies :: [Term] -> [Const] -> Maybe Subst
unifies = go mempty
  where
    go s ts cs =
      case (ts, cs) of
        ([], []) -> Just s
        ([], _) -> Nothing
        (_, []) -> Nothing
        (t:ts', c:cs') ->
          do result <- unify t c s
             go result ts' cs'

setMapMaybe :: Ord a => Ord b => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList

-- Given a subsitution, return all the extensions of this substitution that are
-- compatible with this atom and database
extend :: Db -> Atom -> Subst -> Set Subst
extend (Db db) atom sub =
  case Map.lookup (rel atom) db of
    Nothing -> Set.empty
    Just facts ->
      setMapMaybe (union sub <=< unifies (terms atom)) (unTuple facts)

extends :: Db -> [Atom] -> Subst -> Set Subst
extends db atoms sub =
  foldr go (Set.singleton sub) atoms
  where go atom substs = Set.unions (Set.map (extend db atom) substs)

ground :: Atom -> Subst -> Maybe [Const]
ground atom (Subst sub) =
  for (terms atom) $ 
    \case
      TConst c -> Just c
      TVar v -> Map.lookup v sub

evalRule :: Db -> Rule -> Db
evalRule db@(Db m) rule =
  let -- All consistent substitutions of variables
      substs = extends db (Set.toList (body rule)) mempty
      -- Substitute constants for variables in the head
      maybeFacts = traverse (ground (rHead rule)) (Set.toList substs)
  in case Set.map Tuple . Set.fromList <$> maybeFacts of
       Nothing -> error "range restriction violation?"
       Just tuples ->
         Db (Map.insertWith Set.union (rel (rHead rule)) tuples m)

immediateConsequence :: Set Rule -> Db -> Db
immediateConsequence rules db = foldl evalRule db rules

eval :: Set Rule -> Db -> Db
eval rules db =
  let db' = immediateConsequence rules db
  in if db == db'
     then db'
     else eval rules db'
