{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Bio.Data.Experiment.Types
    ( FileType(..)
    , File
    , location
    , format
    , keywords
    , emptyFile

    , FileSet(..)

    , Replicate
    , rid
    , files
    , info
    , number
    , Experiment(..)
    , NGS(..)
    , IsDNASeq(..)
    , ChIPSeq
    , target
    , control
    , defaultChIPSeq

    , ATACSeq
    , RNASeq
    ) where

import           Control.Arrow         (first)
import           Control.Lens          hiding ((.=))
import           Crypto.Hash.MD5       (hash)
import           Data.Aeson
import           Data.Aeson.TH         (defaultOptions, deriveJSON,
                                        fieldLabelModifier)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map.Strict       as M
import           Data.Serialize        (Serialize (..))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
import           Numeric               (showHex)

instance Serialize T.Text where
    put txt = put $ T.encodeUtf8 txt
    get     = fmap T.decodeUtf8 get

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
    deriving (Show, Read, Eq, Ord, Generic)

deriveJSON defaultOptions ''FileType

instance Serialize FileType

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

data File = File
    { _location :: !FilePath
    , _format   :: !FileType
    , _keywords :: ![T.Text]
    } deriving (Show, Read, Eq, Ord, Generic)

makeLenses ''File

emptyFile :: File
emptyFile = File
    { _location = ""
    , _format = Other
    , _keywords = []
    }

instance FromJSON File where
    parseJSON = withObject "File" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        path <- obj .: "path"
        File <$> return path <*>
                 obj .:? "format" .!= guessFormat path <*>
                 obj .:? "keywords" .!= []

instance ToJSON File where
    toJSON     = genericToJSON defaultOptions{fieldLabelModifier=f}
      where
        f x = M.findWithDefault x x fieldTable
        fieldTable = M.fromList $
            [ ("_location", "path")
            , ("_keywords", "keywords")
            , ("_format", "format")
            ]

instance Serialize File

data FileSet = Single File
             | Pair File File
             deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON FileSet where
    parseJSON = withObject "FileSet" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        fls <- obj .:? "pair"
        case fls of
            Nothing -> Single <$> parseJSON (Object obj')
            Just array -> flip (withArray "FileSet") array $ \xs ->
                if V.length xs == 2
                    then Pair <$> parseJSON (xs `V.unsafeIndex` 0)
                              <*> parseJSON (xs `V.unsafeIndex` 1)
                    else error "The number of files must be 2."

instance ToJSON FileSet where
    toJSON (Single f) = toJSON f
    toJSON (Pair a b) = object [("pair", Array $ V.fromList [toJSON a, toJSON b])]

instance Serialize FileSet

data Replicate = Replicate
    { _rid    :: !T.Text
    , _files  :: [FileSet]
    , _info   :: !(M.Map T.Text T.Text)
    , _number :: !Int
    } deriving (Show, Read, Eq, Ord, Generic)

makeLenses ''Replicate

instance FromJSON Replicate where
    parseJSON = withObject "Replicate" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        fls <- obj .: "files"
        let rid' = T.pack $ concat $ map (flip showHex "") $ B.unpack $ hash $
                   BC.pack $ unlines $ concatMap getPath fls
        Replicate <$> obj .:? "id" .!= rid' <*>
                       return fls <*>
                       obj .:? "info" .!= M.empty <*>
                       obj .:? "rep" .!= 0
      where
        getPath (Single x) = [x^.location]
        getPath (Pair a b) = [a^.location, b^.location]


instance ToJSON Replicate where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=f}
      where
        f x = M.findWithDefault x x fieldTable
        fieldTable = M.fromList $
            [ ("_rid", "id")
            , ("_files", "files")
            , ("_info", "info")
            , ("_number", "rep")
            ]

instance Serialize Replicate


-- Data types representing different kinds of assays.

-- | A set of fields that exist in all kinds of Assays
data CommonFields = CommonFields
    { _commonEid        :: !T.Text
    , _commonGroupName  :: !(Maybe T.Text)
    , _commonCellType   :: !T.Text
    , _commonReplicates :: [Replicate]
    } deriving (Show, Read, Eq, Ord, Generic)

makeLenses ''CommonFields

instance FromJSON CommonFields where
    parseJSON = withObject "CommonFields" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        CommonFields <$> obj .: "id" <*>
                         obj .:? "group" <*>
                         obj .:? "celltype" .!= "" <*>
                         obj .: "replicates"

instance ToJSON CommonFields where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier=f}
      where
        f x = M.findWithDefault x x fieldTable
        fieldTable = M.fromList $
            [ ("_commonEid", "id")
            , ("_commonGroupName", "group")
            , ("_commonCellType", "celltype")
            , ("_commonReplicates", "replicates")
            ]

instance Serialize CommonFields

defaultCommonFields :: CommonFields
defaultCommonFields = CommonFields
    { _commonEid = ""
    , _commonGroupName = Nothing
    , _commonCellType = ""
    , _commonReplicates = []
    }

class Experiment e where
    commonFields :: Lens' e CommonFields

    eid :: Lens' e T.Text
    eid = commonFields . commonEid

    groupName :: Lens' e (Maybe T.Text)
    groupName = commonFields . commonGroupName

    cellType :: Lens' e T.Text
    cellType = commonFields . commonCellType

    replicates :: Lens' e [Replicate]
    replicates = commonFields . commonReplicates

    {-# MINIMAL commonFields #-}

-- | Next generation sequencing experiments.
class NGS e where
    pairedEnd :: e -> Bool

data ChIPSeq = ChIPSeq
    { chipseqCommon    :: CommonFields
    , chipseqTarget    :: !T.Text
    , chipseqPairedEnd :: !Bool
    , chipseqControl   :: Maybe ChIPSeq
    } deriving (Show, Read, Eq, Ord, Generic)

target :: Lens' ChIPSeq T.Text
target f e = (\x -> e{chipseqTarget = x}) <$> f (chipseqTarget e)

control :: Lens' ChIPSeq (Maybe ChIPSeq)
control f e = (\x -> e{chipseqControl = x}) <$> f (chipseqControl e)

instance Experiment ChIPSeq where
    commonFields f e = (\x -> e{chipseqCommon = x}) <$> f (chipseqCommon e)

instance NGS ChIPSeq where
    pairedEnd = chipseqPairedEnd

instance FromJSON ChIPSeq where
    parseJSON = withObject "ChIPSeq" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        ChIPSeq <$> parseJSON (Object obj') <*>
                    obj .: "target" <*>
                    obj .:? "pairedend" .!= False <*>
                    obj .:? "control"


instance ToJSON ChIPSeq where
    toJSON e = let Object common = toJSON (chipseqCommon e)
               in Object $ HM.fromList $ HM.toList common ++
                      [ "target" .= chipseqTarget e
                      , "pairedEnd" .= chipseqPairedEnd e
                      , "control" .= chipseqControl e
                      ]

instance Serialize ChIPSeq

defaultChIPSeq :: ChIPSeq
defaultChIPSeq = ChIPSeq
    { chipseqCommon = defaultCommonFields
    , chipseqTarget = ""
    , chipseqPairedEnd = False
    , chipseqControl = Nothing
    }


data ATACSeq = ATACSeq
    { atacseqCommon    :: CommonFields
    , atacseqPairedEnd :: !Bool
    } deriving (Show, Read, Eq, Ord, Generic)

instance Experiment ATACSeq where
    commonFields f e = (\x -> e{atacseqCommon = x}) <$> f (atacseqCommon e)

instance NGS ATACSeq where
    pairedEnd = atacseqPairedEnd

instance FromJSON ATACSeq where
    parseJSON = withObject "ATACSeq" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        ATACSeq <$> parseJSON (Object obj') <*>
                    obj .:? "pairedend" .!= False


instance ToJSON ATACSeq where
    toJSON e = let Object common = toJSON (atacseqCommon e)
               in Object $ HM.fromList $ HM.toList common ++
                      [ "pairedEnd" .= atacseqPairedEnd e ]

instance Serialize ATACSeq


data RNASeq = RNASeq
    { rnaseqCommon    :: CommonFields
    , rnaseqPairedEnd :: !Bool
    } deriving (Show, Read, Eq, Ord, Generic)

instance Experiment RNASeq where
    commonFields f e = (\x -> e{rnaseqCommon = x}) <$> f (rnaseqCommon e)

instance NGS RNASeq where
    pairedEnd = rnaseqPairedEnd

instance FromJSON RNASeq where
    parseJSON = withObject "RNASeq" $ \obj' -> do
        let obj = HM.fromList $ map (first T.toLower) $ HM.toList obj'
        RNASeq <$> parseJSON (Object obj') <*>
                    obj .:? "pairedend" .!= False


instance ToJSON RNASeq where
    toJSON e = let Object common = toJSON (rnaseqCommon e)
               in Object $ HM.fromList $ HM.toList common ++
                      [ "pairedEnd" .= rnaseqPairedEnd e ]

instance Serialize RNASeq

class IsDNASeq a
instance IsDNASeq ChIPSeq
instance IsDNASeq ATACSeq
