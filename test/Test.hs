{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test (tests) where

import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)

import Hedgehog ((===))
import Hedgehog qualified as HG
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHG
import Test.Tasty.HUnit qualified as TastyH

import qualified Datalog as Dl

-- import Debug.Trace (traceM)

con :: HG.Gen Dl.Const
con = Dl.Const <$> Gen.string (Range.linear 0 4) Gen.lower

var :: HG.Gen Dl.Var
var = Dl.Var <$> Gen.string (Range.singleton 1) Gen.upper

rel :: HG.Gen Dl.Rel
rel = Dl.Rel <$> Gen.string (Range.linear 1 4) Gen.lower

term :: HG.Gen Dl.Term
term = Gen.choice [Dl.TConst <$> con, Dl.TVar <$> var]

subst :: HG.Gen Dl.Subst
subst = Dl.Subst <$> Gen.map (Range.linear 0 8) ((,) <$> var <*> con)

_atom :: HG.Gen Dl.Atom
_atom = Dl.Atom <$> rel <*> Gen.list (Range.linear 0 6) term

db :: Int -> HG.Gen Dl.Db
db minSize = Dl.Db <$> Gen.map (Range.linear minSize 16) ((,) <$> rel <*> facts)
 where 
   facts :: HG.Gen (Set Dl.Tuple)
   facts =
     do arity <- Range.singleton <$> Gen.integral (Range.linear 0 5)
        Gen.set (Range.linear 1 16) (Dl.Tuple <$> Gen.list arity con)

dbRelFacts :: Dl.Db -> HG.Gen (Dl.Rel, Set Dl.Tuple)
dbRelFacts (Dl.Db d) = Gen.choice (map return (Map.toList d))

dbRelArity :: Dl.Db -> HG.Gen (Dl.Rel, Int)
dbRelArity (Dl.Db d) =
  do (r, facts) <- dbRelFacts (Dl.Db d)
     return (r, Dl.tupleArity (head (Set.toList facts)))

dbAtom :: Dl.Db -> HG.Gen Dl.Term -> HG.Gen Dl.Atom
dbAtom d genTerm =
  do (r, arity) <- dbRelArity d
     terms <- Gen.list (Range.singleton arity) genTerm
     return $
       Dl.Atom
       { Dl.rel = r
       , Dl.terms = terms
       }

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "dlsubst"
    [ TastyHG.testProperty "unify const const" $ do
        HG.property $
          do c1 <- HG.forAll con
             c2 <- HG.forAll con
             sub <- HG.forAll subst
             Dl.unify (Dl.TConst c1) c2 sub ===
               if c1 == c2
               then Just sub
               else Nothing
    , TastyHG.testProperty "unify var const => var |-> const" $ do
        HG.property $
          do v <- HG.forAll var
             c <- HG.forAll con
             Dl.unify (Dl.TVar v) c mempty === Just (Dl.singletonSubst v c)
    , TastyHG.testProperty "union x x = x" $ do
        HG.property $
          do s <- HG.forAll subst
             Dl.union s s === Just s
    , TastyHG.testProperty "union x y = union y x" $ do
        HG.property $
          do s1 <- HG.forAll subst
             s2 <- HG.forAll subst
             Dl.union s1 s2 === Dl.union s2 s1
    , TastyHG.testProperty "extend empty" $ do
        HG.property $
          do d@(Dl.Db m) <- HG.forAll (db 1)
             a <- HG.forAll (dbAtom d (Dl.TVar <$> var))
             -- There can be fewer substitutions if the atom has the same
             -- variable in multiple positions.
             HG.diff
              (Just (Set.size (Dl.extend d a mempty)))
              (<=)
              (length <$> Map.lookup (Dl.rel a) m)
    -- Extending should yield extensions
    , TastyHG.testProperty "extend subst vars" $ do
        HG.property $
          do d <- HG.forAll (db 1)
             a <- HG.forAll (dbAtom d (Dl.TVar <$> var))
             sub0@(Dl.Subst m0) <- HG.forAll subst
             let substs = Dl.extend d a sub0
             forM_ substs $
               \(Dl.Subst sub) ->
                 HG.diff (Map.keysSet m0) Set.isSubsetOf (Map.keysSet sub)
    -- Extending a substitution by an atom should yield substitutions that
    -- cover all the variables of that atom
    , TastyHG.testProperty "extend atom vars" $ do
        HG.property $
          do d <- HG.forAll (db 1)
             a <- HG.forAll (dbAtom d (Dl.TVar <$> var))
             sub0 <- HG.forAll subst
             let substs = Dl.extend d a sub0
             let aVars = foldr (\(Dl.TVar v) -> Set.insert v) Set.empty (Dl.terms a)
             forM_ substs $
               \(Dl.Subst sub) ->
                 let sVars = Map.keysSet sub
                 in HG.diff aVars Set.isSubsetOf sVars
    , TastyH.testCase "copy" $
        let a = Dl.Rel "a"
            b = Dl.Rel "b"
            copy =
              Dl.Rule 
              { Dl.rHead = Dl.Atom a [Dl.TVar (Dl.Var "X")]
              , Dl.body = Set.fromList [Dl.Atom b [Dl.TVar (Dl.Var "X")]]
              }
            rules = Set.fromList [copy]
            facts = Set.fromList [Dl.Tuple [Dl.Const "c"]]
            edbMap = Map.fromList [(b, facts)]
            result = Map.insert a facts edbMap
        in Dl.Db result TastyH.@=? Dl.eval rules (Dl.Db edbMap)
    ]
