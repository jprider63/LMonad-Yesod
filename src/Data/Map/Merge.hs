{-# lANGUAGE BangPatterns #-}
module Data.Map.Merge where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Data.Map.Internal

-- Inefficient merge since it's not available until later version.

maps :: Ord k => (k -> Maybe v -> Maybe v -> Maybe v) -> Map k v -> Map k v -> Map k v
maps f a b = 
    foldr (\k acc -> 
        case f k (Map.lookup k a) (Map.lookup k b) of 
            Nothing -> acc
            Just x -> Map.insert k x acc
      ) Map.empty keys

    where
        keys = Set.fromList (Map.keys a) `Set.union` Set.fromList (Map.keys b)

mapAnd :: (Ord k, Show k, Eq v) => Map k v -> Map k v -> Map k v
mapAnd = maps f
    where
        f _ Nothing Nothing = Nothing
        f _ x@(Just _) Nothing = x
        f _ Nothing x@(Just _) = x
        f _ (Just x) (Just y) | x == y = Just x
        f k (Just _) (Just _) = error ("Field `" ++ show k ++ "` is constrained to different values.")

mapOr :: (Ord k, Eq v) => Map k v -> Map k v -> Map k v
mapOr = maps f
    where
        f _ Nothing Nothing = Nothing
        f _ (Just _) Nothing = Nothing
        f _ Nothing (Just _) = Nothing
        f _ (Just x) (Just y) | x == y = Just x
        f _ (Just _) (Just _) = Nothing

