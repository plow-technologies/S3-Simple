{-# LANGUAGE DeriveGeneric #-}
module Network.AWS.S3Simple (module Network.AWS.S3Simple) where

import Control.Applicative
import Data.Either
import GHC.Generics
import Network.AWS.S3SimpleTypes as Network.AWS.S3Simple
import Network.AWS.S3SimpleAdapters
import qualified Data.ByteString as B
import qualified Network.AWS.AWSConnection as Conn
import qualified Network.AWS.AWSResult as Res
import qualified Network.AWS.Authentication as Auth
import qualified Network.AWS.S3Bucket as Bucket
import qualified Network.AWS.S3Object as Obj

defaultS3Host :: S3Host
defaultS3Host = S3Host "s3.amazonaws.com" 80


uploadObject :: S3Connection -> S3Bucket ->  S3Object -> IO (S3Result ())
uploadObject sConn sBucket sObj = do
  let conn = adaptS3Connection sConn
      obj = adaptS3Object sObj
  res <- Obj.sendObject conn obj
  case (res) of
    (Left err) -> do
      case (err) of
        (Res.NetworkError _) -> return $ S3Error $ Left $ S3NetworkError "Error connecting to the server"
        (Res.AWSError sX sY) -> return $ S3Error $ Right$ S3AuthError $ sX ++ sY
    (Right _) -> return $ S3Success ()


createBucket :: S3Connection -> S3Bucket -> IO (S3Result ())
createBucket sConn bucket = do
  res <- (Bucket.createBucket (adaptS3Connection sConn) (s3BucketName bucket)) 
  return (adaptS3Result adaptLibS3Empty res)


receiveObject :: S3Connection -> S3Bucket -> S3Object -> IO (S3Result S3Object)
receiveObject conn bucket obj = do
  res <- Obj.getObject (adaptS3Connection conn) $ adaptS3Object obj
  return (adaptS3Result adaptLibS3Object res)

bucketContents :: S3Connection -> String -> Int -> S3Bucket -> IO (S3Result [S3Object])
bucketContents sConn sTerm maxRes bucket = do
  let lReq = Bucket.ListRequest sTerm "" "/" maxRes
  res <- Bucket.listObjects (adaptS3Connection sConn) (s3BucketName bucket) lReq
  return (adaptS3Result (adaptLibS3ListResult) res)

listBuckets :: S3Connection -> IO (S3Result [S3Bucket])
listBuckets sConn = do
  res <- Bucket.listBuckets (adaptS3Connection sConn)
  return (adaptS3Result (adaptLibS3Bucket <$>) res)