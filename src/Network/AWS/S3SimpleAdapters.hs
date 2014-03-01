{-# LANGUAGE DeriveGeneric #-}
module Network.AWS.S3SimpleAdapters where

import Network.AWS.S3SimpleTypes
import GHC.Generics
import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.AWS.AWSConnection as Conn
import qualified Network.AWS.AWSResult as Res
import qualified Network.AWS.Authentication as Auth
import qualified Network.AWS.S3Bucket as Bucket
import qualified Network.AWS.S3Object as Obj


adaptS3Connection :: S3Connection -> Conn.AWSConnection
adaptS3Connection conn = Conn.AWSConnection (s3HostName . s3Host$ conn) (s3Port . s3Host $ conn) (s3AccessKey conn) (s3SecretKey conn)

adaptS3Object :: S3Object -> Obj.S3Object
adaptS3Object obj = 
  case obj of
    (S3Object name contents "" path cType) -> Obj.S3Object "" name cType [] (L.fromStrict contents)
    (S3Object name contents bucket path cType) ->Obj.S3Object bucket ((path ++ "/") ++ name) cType [] (L.fromStrict contents)

adaptLibS3Object :: Obj.S3Object -> S3Object
adaptLibS3Object obj = 
  let nameList = splitOn "/" $ Obj.obj_name obj
      path = (intercalate "/") $ init nameList
      objName = last nameList
  in S3Object objName (L.toStrict $ Obj.obj_data obj) (Obj.obj_bucket obj) path (Obj.content_type obj)

adaptS3Result :: (a -> b) -> Res.AWSResult a -> S3Result b
adaptS3Result f res =
  case (res) of
    (Left err) -> do
      case (err) of
        (Res.NetworkError _) -> S3Error . Left . S3NetworkError $ Res.prettyReqError err
        (Res.AWSError sX sY) -> S3Error . Right . S3AuthError $ sX ++ sY
    (Right o) -> S3Success $ f o

adaptLibS3ListResult :: (Bucket.IsTruncated, [Bucket.ListResult]) -> [S3Object]
adaptLibS3ListResult (_,resList) = (\res -> S3Object (Bucket.key res) B.empty "" "" "") <$> resList

adaptLibS3Bucket :: Bucket.S3Bucket -> S3Bucket
adaptLibS3Bucket (Bucket.S3Bucket name date) = S3Bucket name date UndefinedRegion

adaptLibS3Empty :: () -> ()
adaptLibS3Empty a = ()

handleUploadObject :: (Conn.AWSConnection -> Obj.S3Object -> IO (Res.AWSResult ())) -> Conn.AWSConnection -> Obj.S3Object -> IO (S3Result ())
handleUploadObject f conn obj= do
  res <- f conn obj
  case (res) of
    (Left err) -> do
      case (err) of
        (Res.NetworkError _) -> return $ S3Error $ Left $ S3NetworkError "Error connecting to the server"
        (Res.AWSError sX sY) -> return $ S3Error $ Right$ S3AuthError $ sX ++ sY
    (Right _) -> return $ S3Success ()
