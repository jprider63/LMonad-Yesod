module Main where

import System.Exit (exitFailure)
import Test.HUnit

import qualified Clearance
import qualified DC

main :: IO ()
main = do
    Counts _ _ errC failC <- runTestTT tests
    if errC + failC == 0 then
        return ()
    else
        exitFailure

tests = TestList [Clearance.tests, DC.tests]

