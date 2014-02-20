{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Network.AWS.S3SimpleTypes where

import Control.Applicative
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.Aeson as A
import Data.Yaml (parseJSON, FromJSON, (.:))
data S3Region = US | EU | USWest | APSouthEast | UndefinedRegion deriving (Generic, Eq, Show)

data S3Host = S3Host {
  s3HostName :: String
 ,s3Port :: Int
} deriving (Read, Eq, Show)
instance FromJSON S3Host where
  parseJSON (A.Object tObj) = S3Host <$>
                      tObj .: "s3HostName" <*>
                      tObj .: "s3Port"

data S3Connection = S3Connection {
  s3Host :: S3Host
 ,s3AccessKey :: String
 ,s3SecretKey :: String
} deriving (Read, Show,Eq)
instance FromJSON S3Connection where
    parseJSON (A.Object tObj) = S3Connection <$>
                          tObj .: "s3Host" <*>
                          tObj .: "s3AccessKey" <*>
                          tObj .: "s3SecretKey"
    parseJSON _ = fail "Rule: Expecting MongoDB Config Object Received, Other"

data S3Object = S3Object{
  s3ObjectName :: String
 ,s3ObjectContents :: B.ByteString
 ,s3ObjectBucket :: String
 ,s3ObjectPath :: String
 ,s3ContentType :: String
} deriving (Generic, Eq, Show)

data S3Bucket = S3Bucket {
  s3BucketName :: String
 ,s3BucketCreationDate :: String
 ,s3BucketRegion :: S3Region
} deriving (Generic, Eq, Show)

data S3NetworkError = S3NetworkError String  deriving (Generic, Eq, Show)

data S3AuthError = S3AuthError {
  errorMessage :: String
} deriving (Generic, Eq, Show)

data S3Result a = S3Error (Either S3NetworkError S3AuthError) | S3Success a deriving (Generic, Eq, Show)