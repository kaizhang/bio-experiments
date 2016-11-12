{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Data.Experiment.Utils where

import           Control.Lens              ((.~), (^.))
import           Data.Function             (on)
import           Data.List
import           Data.List.Ordered         (nubSort)
import qualified Data.Map.Strict           as M
import           Data.Ord                  (comparing)
import qualified Data.Text                 as T
import           Data.Yaml                 (decodeFile)

import           Bio.Data.Experiment.Types

{-
-- ^ Merge experiments with same id.
mergeExp :: [Experiment a] -> [Experiment a]
mergeExp es = flip map expGroup $ \e -> foldl' combine (head e) $ tail e
  where
    expGroup = groupBy ((==) `on` (^.eid)) $ sortBy (comparing (^.eid)) es
    combine e1 e2 =
        if e1^.celltype == e2^.celltype && e1^.target == e2^.target &&
           e1^.control == e2^.control
            then files .~ nubSort (e1^.files ++ e2^.files) $
                 info .~ M.unionWith f (e1^.info) (e2^.info) $
                 e1
            else error "Abort: Found experiments with same id but with different contents"
    f a b | a == b = a
          | otherwise = a `T.append` "|" `T.append` b

-- ^ Split experiments such that each experiment contains a single file. This
-- makes it easier for parallel processing. id == mergeExp . splitExp.
splitExp :: Experiment e => [e] -> [e]
splitExp es = flip concatMap es $ \e -> zipWith f (repeat e) $ e^.files
  where
    f e x = files .~ [x] $ e
          -}
