{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Data.Experiment.Utils where

import           Control.Lens              ((.~), (^.))
import           Data.Function             (on)
import           Data.List                 (foldl1', groupBy, sortBy)
import           Data.List.Ordered         (nubSort)
import qualified Data.Map.Strict           as M
import           Data.Ord                  (comparing)
import qualified Data.Text                 as T

import           Bio.Data.Experiment.Types

-- | Merge experiments with same id.
mergeExps :: Experiment e => [e] -> [e]
mergeExps es = map combineExp expGroup
  where
    expGroup = groupBy ((==) `on` (^.eid)) $ sortBy (comparing (^.eid)) es
    combineExp e = if allEqual (map (^.commonFields) e)
        then replicates .~ mergeReps (concatMap (^.replicates) e) $ head e
        else error "Abort: Found experiments with same id but with different contents"
    allEqual (x:xs) = all (==x) xs
    allEqual _ = True

-- | Merge replicates with same number.
mergeReps :: [Replicate] -> [Replicate]
mergeReps rs = map (foldl1' combineRep) repGroup
  where
    repGroup = groupBy ((==) `on` (^.number)) $ sortBy (comparing (^.number)) rs
    combineRep r1 r2 = files .~ nubSort (r1^.files ++ r2^.files) $
        info .~ M.unionWith f (r1^.info) (r2^.info) $ r1
    f a b | a == b = a
          | otherwise = a `T.append` " | " `T.append` b

{-
-- ^ Split experiments such that each experiment contains a single file. This
-- makes it easier for parallel processing. id == mergeExp . splitExp.
splitExp :: Experiment e => [e] -> [e]
splitExp es = flip concatMap es $ \e -> zipWith f (repeat e) $ e^.files
  where
    f e x = files .~ [x] $ e
    -}
