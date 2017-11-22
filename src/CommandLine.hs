module CommandLine(getOptions)
 where

import Control.Exception(SomeException,catch)
import Control.Lens(ASetter, view, set, elemOf, folded)
import Control.Monad(forM_, unless, when)
import Data.Char(isAlphaNum, toLower)
import Data.Either(isLeft)
import Data.String(IsString, fromString)
import Data.Time.Clock(UTCTime, getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import Network.AWS(Region(..), Credentials(..), Env,
                   runAWS, runResourceT, newEnv, send, envRegion)
import Network.AWS.Data(toText)
import Options
import System.Console.GetOpt(ArgDescr(..), OptDescr(..), ArgOrder(..))
import System.Console.GetOpt(getOpt, usageInfo)
import System.Directory(doesFileExist)
import System.Exit(ExitCode(ExitFailure), exitWith)
import System.FilePath(takeFileName)

type OptOrErr = Either [String] Options

addError :: OptOrErr -> String -> OptOrErr
addError (Left errs) err = Left (errs ++ [err])
addError (Right _)   err = Left [err]

addOpt :: OptOrErr -> (Options -> Options) -> OptOrErr
addOpt (Left errs) _ = Left errs
addOpt (Right o) f   = Right (f o)

validateS3Bucket :: String -> OptOrErr -> OptOrErr
validateS3Bucket b opts
  | any (not . isBuckCh) b = addError opts "S3 bucket has weird characters."
  | otherwise              = addOpt opts (set optS3Bucket (fromString b))
 where isBuckCh c = isAlphaNum c || (c == '-') || (c == '.')

validateRegion :: String -> OptOrErr -> OptOrErr
validateRegion r opts =
  case lookup (map toLower r) regions of
    Nothing -> addError opts "Unknown AWS region."
    Just v  -> addOpt   opts (set optAwsRegion v)

regions :: [(String, Region)]
regions =
  [ ("ireland", Ireland), ("eu-west-1", Ireland)
  , ("frankfurt", Frankfurt), ("eu-central-1", Frankfurt)
  , ("tokyo", Tokyo), ("ap-northeast-1", Tokyo)
  , ("singapore", Singapore), ("ap-southeast-1", Singapore)
  , ("sydney", Sydney), ("ap-southeast-2", Sydney)
  , ("beijing", Beijing), ("cn-north-1", Beijing)
  , ("northvirginia", NorthVirginia), ("us-east-1", NorthVirginia)
  , ("northcalifornia", NorthCalifornia), ("us-west-1", NorthCalifornia)
  , ("oregon", Oregon), ("us-west-2", Oregon)
  , ("govcloud", GovCloud), ("us-gov-west-1", GovCloud)
  , ("govcloudfips", GovCloudFIPS), ("fips-us-gov-west-1", GovCloudFIPS)
  , ("saopaulo", SaoPaulo), ("sa-east-1", SaoPaulo)
  ]

options :: [OptDescr (OptOrErr -> OptOrErr)]
options =
  [ Option ['b'] ["s3-bucket"] (ReqArg validateS3Bucket "BUCKET")
           "S3 bucket to upload to, temporarily."
  , Option ['r'] ["region"] (ReqArg validateRegion "REGION")
           "S3 region to upload to."
  , Option ['a'] ["kernel-args"]
           (ReqArg (\a opts -> addOpt opts (set optKernelArgs a)) "STRING")
           "Kernel arguments to pass to the unikernel."
  ]

maybeSet :: IsString b => ASetter s s a b -> Maybe String -> s -> s
maybeSet _     Nothing  x = x
maybeSet field (Just v) x = set field (fromString v) x

getOptions :: [String] -> IO (Options, Env)
getOptions argv =
  do let (res, xs, errs)  = getOpt RequireOrder options argv
         doneOpts         = foldl (flip id) (Right defaultOptions) res
         optErrors        = either id (const []) doneOpts
         kernelErrs       = if null xs then ["No unikernel specified!"] else []
         Right baseOpts   = doneOpts
     now                 <- getCurrentTime
     let opts             = adjustImageName
                               $ adjustTargetName now
                               $ set optKernel   (head xs)
                               $ set optRamdisks (tail xs) baseOpts
     when (isLeft doneOpts || null xs || not (null errs)) $
       fail' (optErrors ++ kernelErrs ++ errs)
     kernelOk   <- doesFileExist (view optKernel opts)
     ramdisksOk <- mapM doesFileExist (view optRamdisks opts)
     unless kernelOk $ fail' ["Unikernel not found"]
     unless (and ramdisksOk) $
       do let disks   = zip (view optRamdisks opts) ramdisksOk
              disks'  = filter (not . snd) disks
              disks'' = map fst disks'
          fail' (map (\s -> "Ramdisk "++s++" not found.") disks'')
     e <- set envRegion (view optAwsRegion opts) `fmap` newEnv Discover
     return (opts, e)

adjustTargetName :: UTCTime -> Options -> Options
adjustTargetName now opts
  | view optTargetKey opts == view optTargetKey defaultOptions =
      let baseName = takeFileName (view optKernel opts)
          formStr  = baseName ++ "-%0C%0y%m%d-%H%M%S.raw"
          keyStr   = formatTime defaultTimeLocale formStr now
      in set optTargetKey (fromString keyStr) opts
  | otherwise = opts

adjustImageName :: Options -> Options
adjustImageName opts
  | view optImageName opts == view optImageName defaultOptions =
      set optImageName (toText (view optTargetKey opts)) opts
  | otherwise = opts

fail' :: [String] -> IO a
fail' errs =
  do forM_ errs $ \ e -> putStrLn ("ERROR: " ++ e)
     putStrLn ("\n" ++ usageInfo hdr options)
     exitWith (ExitFailure 1)
 where hdr = "Usage: ec2-unikernel [OPTION...] KERNEL [RAMDISK ...]"

