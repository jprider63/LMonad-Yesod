module Sim where

import LMonad

import Test.HUnit

unlabelFailure :: (Show l, Label l) => l -> l -> l -> IO l
unlabelFailure = unlabelHelper not "unlabel expected failure"

unlabelSuccess :: (Show l, Label l) => l -> l -> l -> IO l
unlabelSuccess = unlabelHelper id "unlabel expected success"

unlabelHelper f desc' current label clearance = do
    assertBool desc (f $ current `canFlowTo` label && label `canFlowTo` clearance)
    return $ current `lub` label

    where
        desc = desc' ++ ": " ++ show current ++ "," ++ show label ++ "," ++ show clearance

updateHelper f desc' current read write clearance = do
    -- let l = read `glb` write 
    -- let l = read `lub` write 
    -- assertBool desc (f $ current `canFlowTo` l && l `canFlowTo` clearance)
    -- assertBool desc (f $ current `canFlowTo` read && read `canFlowTo` clearance 
    assertBool desc (f $ current `canFlowTo` read
        && write `canFlowTo` clearance)
        -- && current `canFlowTo` write && write `canFlowTo` clearance)

    where 
        desc = desc'

updateFailure :: Label l => l -> l -> l -> l -> IO ()
updateFailure = updateHelper not "update expected failure"

updateSuccess :: Label l => l -> l -> l -> l -> IO ()
updateSuccess = updateHelper id "update expected success"

insertSuccess :: Label l => l -> l -> l -> l -> IO ()
insertSuccess = updateHelper id "insert expected success"

insertFailure :: Label l => l -> l -> l -> l -> IO ()
insertFailure = updateHelper not "insert expected failure"

declassifyHelper f desc' current label clearance = do
    assertBool desc (f $ current `lub` label `canFlowTo` clearance)

    where
        desc = desc'

declassifySuccess :: Label l => l -> l -> l -> IO ()
declassifySuccess = declassifyHelper id "declassify expected success"
