#!/usr/bin/env stack
{- stack
  --resolver nightly-2018-11-24
  script
  --optimize
  --package filepath
  --package rio
  --package bytestring
  --package time
  --package random
  --package optparse-applicative
  --package process
  --package unix
  --package regex-pcre
-}

{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Time.Clock
import           Options.Applicative
import qualified Prelude                    as P
import           RIO
import           RIO.Process
import           RIO.Text                   as T
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO                  as IO
import           System.Posix.Signals
import qualified System.Process             as Process
import           System.Random
import           Text.Printf
import           Text.Regex.PCRE


type Args = [String]

execInfo ::
     String
  -> Args
  -> (Process () () () -> RIO SimpleApp a)
  -> RIO SimpleApp ExitCode
execInfo cmd args onProcess = do
  logInfo $ display $ ">>> " <> T.pack (P.unwords (cmd : args))
  exitCode <-
    proc cmd args $ \procConf ->
      withProcess (setCreateGroup True $ setStdin inherit procConf) $ \process -> do
        void $ onProcess process
        waitExitCode process
  case exitCode of
    ExitSuccess -> logError $ ">>> # Executed process finished successfully"
    _ -> logError $ ">>> # Executed process exited with: " <> displayShow exitCode
  return exitCode

exitOnFailure :: MonadIO m => ExitCode -> m ()
exitOnFailure =
  \case
    ExitSuccess -> return ()
    exitCode@(ExitFailure _) -> liftIO $ exitWith exitCode

stack ::
     Args
  -> (Process () () () -> RIO SimpleApp a)
  -> RIO SimpleApp ExitCode
stack args = execInfo "stack" args


stackBuild :: Args -> (Process () () () -> RIO SimpleApp a) -> RIO SimpleApp ExitCode
stackBuild args =
  stack (["build", "--test", "--no-run-tests"] ++ args)
  --, "--bench", "--no-run-benchmarks"] ++ args)

stackClean :: RIO SimpleApp ()
stackClean = stack ["clean", "--full"] pure >>= exitOnFailure


time :: MonadIO m => m a -> m (a, Int)
time action = do
  before <- liftIO $ getCurrentTime
  res <- action
  after <- liftIO $ getCurrentTime
  return (res, round (diffUTCTime after before * 1000000))

time_ :: MonadIO m => m a -> m Int
time_ = fmap snd . time

-- | Generate infinite number of random delays each withtin @n@ seconds.
genDelays :: RandomGen g => g -> Int -> Int -> [Int]
genDelays gen minVal maxVal = randomRs (minVal * 1_000_000, maxVal * 1_000_000) gen

-- | Do regular `threadDelay` in small increments, while checikng if process is still running.
threadDelayOrProcessExit :: MonadIO m => Process stdin stdout stderr -> Int -> m ()
threadDelayOrProcessExit process = go
  where
    deltaDelay = 300000
    go curDelay
          | curDelay < 0 = return ()
          | curDelay - deltaDelay < 0 = threadDelay curDelay
          | otherwise = do
              mExitCode <- getExitCode process
              case mExitCode of
                Nothing -> do
                  threadDelay deltaDelay
                  go (curDelay - deltaDelay)
                Just _ -> return ()

terminateProcessAfter
  :: MonadIO m => Int -> Process stdin stdout stderr -> m ()
terminateProcessAfter delay process =
  liftIO $ do
    threadDelayOrProcessExit process delay
    Process.terminateProcess (unsafeProcessHandle process)

signalProcessAfter :: MonadIO m => Int -> Signal -> Process stdin stdout stderr -> m ()
signalProcessAfter delay signal process =
  liftIO $ do
    threadDelayOrProcessExit process delay
    mPid <- Process.getPid (unsafeProcessHandle process)
    case mPid of
      Nothing  -> return ()
      Just pid -> signalProcess signal pid

signalProcessGroupAfter :: MonadIO m => Int -> Signal -> Process stdin stdout stderr -> m ()
signalProcessGroupAfter delay signal process =
  liftIO $ do
    threadDelayOrProcessExit process delay
    mPid <- Process.getPid (unsafeProcessHandle process)
    case mPid of
      Nothing  -> return ()
      Just pid -> signalProcessGroup signal pid

expectSignalExitCode ::
     (MonadReader env m, MonadIO m, HasLogFunc env, Integral a) => a -> ExitCode -> m ()
expectSignalExitCode sig =
  \case
    ExitSuccess -> do
      logInfo $ ">>> # Finished running"
      liftIO exitSuccess
    exitStatus@(ExitFailure exitCode)
      | exitCode /= sigCode -> do
        logError $
          ">>> # Expected to exit with a signal: " <> displayShow sigCode <> ", but died with: " <>
          displayShow exitCode
        liftIO $ exitWith exitStatus
    _ -> return ()
  where
    sigCode = negate (fromIntegral sig)

cmdRunDelayedKiller :: FilePath -> Args -> Signal -> Int -> RIO SimpleApp ()
cmdRunDelayedKiller cmd args sig delay = do
  (_, cmdFullPath') <- proc "which" [cmd] readProcessStdout
  let cmdFullPath = BSL8.init cmdFullPath' -- drop new line
  let onStart process = do
        (_, bs) <- proc "ps" ["-x"] readProcessStdout
        let cmdCountBefore = P.length $ P.filter (=~ cmdFullPath) $ BSL8.lines bs
        logInfo $
          ">>> # Currently running processes " <> displayShow cmdFullPath <> ": " <>
          displayShow cmdCountBefore
        signalProcessGroupAfter delay sig process
  killedAfter <- time_ $ execInfo cmd args onStart >>= expectSignalExitCode sig
  logInfo $ ">>> # Terminated the build after: " <> displayShow killedAfter <> " μs"
  (_, bs) <- proc "ps" ["-x"] readProcessStdout
  let cmdCountBefore = P.length $ P.filter (=~ cmdFullPath) $ BSL8.lines bs
  logInfo $
    ">>> # Leftover running processes " <> displayShow cmdFullPath <> ": " <> displayShow cmdCountBefore



data Opts = Opts
  { optExecCmd      :: String
  , optMinKillDelay :: Int
  , optMaxKillDelay :: Int
  , optKillSignal   :: Signal
  } deriving Show

optsParser :: Parser Opts
optsParser =
  Opts <$>
  strArgument (metavar "COMMAND" <> help "Command to execute in a loop periodically killing it.") <*>
  option
    auto
    (long "max-delay" <> help "Maximu number of seconds to wait before killing the process.") <*>
  option
    auto
    (long "min-delay" <>
     help "Minimum number of seconds to wait before killing the process (default 0)." <>
     value 0) <*>
  option auto (long "signal" <> short 's' <> help "Signal to kill the running process with") <*
  abortOption ShowHelpText (long "help" <> short 'h' <> help "Display this message.")


-- | The only command line argument this scripts expects is the maximum number of seconds before the
-- next signal will be sent.
main :: IO ()
main = do
  Opts {..} <-
    execParser $
    info
      optsParser
      (header ("exec-kill-loop - Runs a command in a loop periodically killing it") <>
       progDesc
         ("This script will run a command until it exits with success, " <>
          "or it exits with a code that doesn't match the signal that it was killed with."))
  gen <- newStdGen
  runSimpleApp $ do
    let delays = genDelays gen optMinKillDelay optMaxKillDelay
        getMaybeCmd strCmd =
          case P.words strCmd of
            [] -> Nothing
            cmdArgs -> Just cmdArgs
        (cmd:args) = fromMaybe (error "Command can't be empty") $ getMaybeCmd optExecCmd
    P.mapM_ (cmdRunDelayedKiller cmd args optKillSignal) delays
