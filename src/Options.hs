module Options(
         Options
       , defaultOptions
       , optS3Bucket
       , optTargetKey
       , optImageName
       , optKernel
       , optKernelArgs
       , optRamdisks
       )
 where

import Control.Lens(Lens', lens)
import Data.String(fromString)
import Data.Text(Text)
import Network.AWS(AccessKey, SecretKey)
import Network.AWS.S3.Types(BucketName, ObjectKey)

data Options = Options
  { _optS3Bucket     :: BucketName
  , _optTargetKey    :: ObjectKey
  , _optKernel       :: FilePath
  , _optKernelArgs   :: String
  , _optRamdisks     :: [FilePath]
  , _optImageName    :: Text
  }

defaultOptions :: Options
defaultOptions = Options
  { _optS3Bucket     = fromString "unikernels"
  , _optTargetKey    = fromString "badfile/,:"
  , _optKernel       = "kernel"
  , _optKernelArgs   = ""
  , _optRamdisks     = []
  , _optImageName    = fromString ""
  }

-- We're explicitly writing these instances because some combination of
-- Amazonka, lens, and Template Haskell explode when we try to build this
-- under GHC 7.8.4.

optS3Bucket :: Lens' Options BucketName
optS3Bucket = lens _optS3Bucket (\ x v -> x{ _optS3Bucket = v })

optTargetKey :: Lens' Options ObjectKey
optTargetKey = lens _optTargetKey (\ x v -> x{ _optTargetKey = v })

optKernel :: Lens' Options FilePath
optKernel = lens _optKernel (\ x v -> x{ _optKernel = v })

optKernelArgs :: Lens' Options String
optKernelArgs = lens _optKernelArgs (\ x v -> x{ _optKernelArgs = v })

optRamdisks :: Lens' Options [FilePath]
optRamdisks = lens _optRamdisks (\ x v -> x{ _optRamdisks = v })

optImageName :: Lens' Options Text
optImageName = lens _optImageName (\ x v -> x{ _optImageName = v })

