module Main (main) where

import qualified Test.Tasty as Tasty

import qualified Test

main :: IO ()
main = Tasty.defaultMain Test.tests