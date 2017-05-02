module DC where

import LMonad
import LMonad.Label.DisjunctionCategory
import Test.HUnit

import Sim

tests = TestLabel "DC" $ TestList [ 
      g1
    , g2
    , g3
    , g4
    ]

apRead = dcConfidentialitySingleton $ User 1
apWrite = dcIntegritySingleton $ User 1
apCreate = dcIntegritySingleton $ User 1
asRead = dcConfidentialitySingleton $ User 1
asWrite = dcIntegritySingleton $ User 1
asCreate = dcIntegritySingleton $ User 1

g1 = TestCase $ do
    let clearance = dcSingleton $ User 1
    let current = bottom

    current <- unlabelSuccess current apRead clearance

    updateFailure current bottom apWrite clearance

g2 = TestCase $ do
    let clearance = dcSingleton $ User 1
    let current = bottom

    declassifySuccess current apRead clearance

    updateSuccess current bottom apWrite clearance

g3 = TestCase $ do
    let clearance = dcSingleton $ User 1
    let current = bottom

    insertSuccess current apRead apCreate clearance
    insertSuccess current asRead asCreate clearance
    insertSuccess current bottom bottom clearance

g4 = TestCase $ do
    let clearance = dcSingleton $ User 2
    let current = bottom

    insertFailure current apRead apCreate clearance
    insertFailure current asRead asCreate clearance
    insertSuccess current bottom bottom clearance

    

{-
data U = User UserId
type L = DCLabel U

A
    p Text <_, Field u, _>
    s Text <Field u, Field u, Field u>
    u UserId

-- Clearance: User 1 %% User 1
g1 (ProtectedEntity k pa) = do 
    -- bottom %% bottom
    
    -- User 1 %% bottom
    s <- unlabel $ pS pa
    
    -- Fail: (User 1 %% bottom) cannot flow to (bottom %% User 1)
    update k [AP =. s]

-- Clearance: User 1 %% User 1
g2 (ProtectedEntity k pa) = do 
    -- bottom %% bottom
    
    -- bottom %% bottom
    s <- declassify $ pAs pa
    
    -- Success: (bottom %% bottom) can flow to (bottom %% User 1)
    update k [AAA =. s]

-- Clearance: User 1 %% User 1
g3  = do
    -- bottom %% bottom
    
    let p = "p"
    let s = "s"
    let userId = 1

    -- Success: 
    -- bottom %% bottom canFlowTo (bottom %% User 1)
    -- && bottom %% bottom canFlowTo (User 1 %% User 1)
    -- && bottom %% bottom canFlowTo bottom %% bottom
    k <- insert $ A p s userId

-- Clearance: User 2 %% User 2
g4  = do
    -- bottom %% bottom
    
    let p = "p"
    let s = "s"
    let userId = 1

    -- Failure: 
    -- bottom %% bottom canFlowTo (bottom %% User 1) cannotFlowTo (User 2 %% User 2)
    -- && bottom %% bottom canFlowTo (User 1 %% User 1) cannotFlowTo (User 2 %% User 2)
    -- && bottom %% bottom canFlowTo bottom %% bottom canFlowTo (User 2 %% User 2)
    k <- insert $ A p s userId

-}

type UserId = Int
data P = User UserId
    deriving (Eq, Ord, Show)
type L = DCLabel P
