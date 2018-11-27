#!/usr/bin/env stack
{- stack
  --resolver nightly-2018-11-24
  script
  --package filepath
  --package rio
-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

import qualified Prelude                    as P
import           RIO
import           RIO.Directory
import           RIO.Process


main :: IO ()
main = do
  let maxDelay = 10
      signals = P.cycle [2, 15, 9] :: [Int]
  let cmd = "stack build"
  let loop (sig:ss) n = do
        proc "stack" ["clean", "--full"] runProcess_
        proc "exec-kill-loop" [cmd, "-s", show sig, "--max-delay", show maxDelay] runProcess_
        -- finish if it ran successfully but there is no output file
        exitCode <- proc "stack" ["exec", "--", "stencil"] runProcess
        case exitCode of
          ExitSuccess -> do
            logInfo $
              "Compiled a complete binary and ran it. Repeating. Iteration: " <> displayShow n
            loop ss (n + 1)
          ExitFailure ec -> logInfo "Failed to execuite compiled binary. Finished."
  runSimpleApp (loop signals 1)
