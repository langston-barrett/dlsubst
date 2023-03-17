{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.ByteString.Lazy qualified as BS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import System.Exit qualified as Exit

import qualified Data.Csv as Csv

import Datalog qualified as Dl

main :: IO ()
main = 
  do csv <- BS.getContents
     rows <-
       case Csv.decode Csv.NoHeader csv :: Either String (Vector (String, String)) of
         Right rs -> return rs
         Left err ->
           do putStrLn ("bad data: " ++ show err)
              Exit.exitWith (Exit.ExitFailure 1)
     let tuples = map (\(a, b) -> Dl.Tuple [Dl.Const a, Dl.Const b]) (Vec.toList rows)
     let edb = 
           Dl.Db $
             Map.fromList
               [ (Dl.Rel "edge", Set.fromList tuples)
               -- , (Dl.Rel "path", Set.empty)
               ]
     print (Dl.eval rules edb)
  where
    var = Dl.TVar . Dl.Var
    rules =
      Set.fromList
        [ Dl.Rule
          { Dl.rHead = Dl.Atom (Dl.Rel "path") [var "A", var "B"]
          , Dl.body = Set.fromList [ Dl.Atom (Dl.Rel "edge") [var "A", var "B"] ]
          }
        , Dl.Rule
          { Dl.rHead = Dl.Atom (Dl.Rel "path") [var "A", var "C"]
          , Dl.body =
            Set.fromList 
              [ Dl.Atom (Dl.Rel "path") [var "A", var "B"]
              , Dl.Atom (Dl.Rel "edge") [var "B", var "C"]
              ]
          }
        ]
