{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Bio.Data.Experiment.Types
    ( FileType(..)
    , File(..)
    , emptyFile
    , location
    , replication
    , format
    , keywords
    , Experiment(..)
    , eid
    , control
    , celltype
    , target
    , files
    , info
    ) where

import           Control.Lens          (makeFields, (.~), (^.))
import           Crypto.Hash.MD5       (hash)
import           Data.Aeson
import           Data.Aeson.TH         (defaultOptions, deriveJSON)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import           Numeric               (showHex)

data FileType = BamFile
              | BaiFile
              | BedFile
              | BedGZip
              | FastqFile
              | FastqGZip
              | BedgraphFile
              | BigWigFile
              | NarrowPeakFile
              | BroadPeakFile
              | Other
    deriving (Show, Read, Eq, Ord)

data File = File
    { fileLocation    :: !FilePath
    , fileReplication :: !Int
    , fileFormat      :: !FileType
    , fileKeywords    :: ![T.Text]
    , fileInfo        :: !(M.Map T.Text T.Text)
    } deriving (Show, Read, Eq, Ord)

makeFields ''File

emptyFile :: File
emptyFile = File
    { fileLocation = ""
    , fileReplication = 0
    , fileFormat = Other
    , fileKeywords = []
    , fileInfo = M.empty
    }

data Experiment = Experiment
    { experimentEid      :: !T.Text
    , experimentCelltype :: !T.Text
    , experimentTarget   :: !T.Text
    , experimentFiles    :: ![File]
    , experimentInfo     :: !(M.Map T.Text T.Text)
    , experimentControl  :: !(Maybe T.Text)
    } deriving (Show, Read, Eq, Ord)

makeFields ''Experiment

deriveJSON defaultOptions ''FileType

instance FromJSON File where
    parseJSON (Object obj) = do
        path <- obj .: "path"
        File <$> return path <*>
                 obj .:? "rep" .!= 1 <*>
                 obj .:? "format" .!= guessFormat path <*>
                 return [] <*>
                 return M.empty

instance FromJSON Experiment where
    parseJSON (Object obj) = do
        fls <- obj .: "files"
        let eid' = T.pack $ concat $ map (flip showHex "") $ B.unpack $ hash $
                   BC.pack $ unlines $ map (^.location) fls
        Experiment <$> obj .:? "id" .!= eid' <*>
                       obj .:? "celltype" .!= "" <*>
                       obj .: "target" <*>
                       return fls <*>
                       return M.empty <*>
                       obj .:? "control"

guessFormat :: FilePath -> FileType
guessFormat fl = case () of
    _ | ".bam" `T.isSuffixOf` fl' -> BamFile
      | ".bai" `T.isSuffixOf` fl' -> BaiFile
      | ".bed" `T.isSuffixOf` fl' -> BedFile
      | ".bed.gz" `T.isSuffixOf` fl' -> BedGZip
      | ".fastq" `T.isSuffixOf` fl' -> FastqFile
      | ".fastq.gz" `T.isSuffixOf` fl' -> FastqGZip
      | ".bw" `T.isSuffixOf` fl' -> BigWigFile
      | otherwise -> Other
  where
    fl' = T.pack fl
