module Main where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified  Data.Yaml as Y

import Network.AWS.S3Simple
import Network.AWS.S3SimpleTypes



-- | The bucket that all documents will be exported to
s3Bucket :: S3Bucket
s3Bucket = S3Bucket "OnpingTestBucket" "" US

typeCsv :: String
typeCsv = "text/csv"

readS3Conf :: FilePath -> IO (Either String S3Connection)
readS3Conf fPath = do
  fCont <- B.readFile fPath
  return $ Y.decodeEither $ fCont

testUpload :: IO ()
testUpload = do
  eDbConf <- readS3Conf "s3config.yml"
  let file = "This is an amazonS3 test"
  case (eDbConf) of
    Left s -> putStrLn s
    Right s3Conn -> do
      let obj = S3Object "ThisIsAnS3Tetst.txt" (C.pack file) (s3BucketName s3Bucket) "testFolder" "text/plain"
      getRes <- uploadObject s3Conn s3Bucket obj
      case (getRes) of
        S3Success _ -> do
          s3Res <- receiveObject s3Conn s3Bucket obj
          case (s3Res) of
            (S3Success a) -> putStrLn $ ("Received -> ") ++  (show $ s3ObjectContents a)
        S3Error (Left (S3NetworkError _)) -> putStrLn "Error connecting to s3"
        S3Error (Right (S3AuthError _)) -> putStrLn "Error authenticating with s3"


main :: IO ()
main = do
  testUpload