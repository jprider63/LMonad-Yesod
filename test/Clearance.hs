module Clearance where

import LMonad
import Test.HUnit

import Sim

tests = TestLabel "Clearance: Public | Secret" $ TestList [ 
      f1
    , f2
    , f3
    ]

{-
data L = Public | Secret

A
    aa Text <_, Secret, _>
    as Text <Secret, Secret, Secret>

f1 (ProtectedEntity k pa) = do
    -- bottom
    
    -- Secret
    s <- unlabel $ pAs pa
    
    -- Should fail: Secret cannotFlowTo Public
    update k [AAA =. s]

f2 (ProtectedEntity k pa) = do
    -- bottom
    
    -- bottom
    s <- declassify $ pAs pa
    
    -- Should succeed: Public canFlowTo Public
    update k [AAA =. s]
-}

data L = Public | Secret
    deriving (Show, Ord, Eq)

instance Label L where
    lub = max
    glb = min
    canFlowTo = (<=)
    bottom = Public

f1 = TestLabel "f1" $ TestCase $ do
    let clearance = Secret
    let current = Public

    current <- unlabelSuccess current Secret clearance

    updateFailure current bottom Secret clearance

f2 = TestLabel "f2" $ TestCase $ do
    let clearance = Secret
    let current = Public

    declassifySuccess current Secret clearance

    updateSuccess current bottom Secret clearance

f3 = TestLabel "f3" $ TestCase $ do
    let clearance = Public
    let current = Public

    current <- unlabelFailure current Secret clearance
    return ()

