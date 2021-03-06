#!/usr/bin/env stack
{- stack
  --resolver nightly-2018-11-24
  script
  --package filepath
  --package rio
  --package massiv
  --package data-default
-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

import qualified Prelude                    as P
import           RIO
import           RIO.Directory
import           RIO.Process


main :: IO ()
main = do
  let maxDelay = 10 -- tune this in order to increase kill interval
      signals = P.cycle [2, 15, 9] :: [Int]
  let cmd = "stack exec -- ghc stencil.hs Average.hs Sobel.hs -O2 -threaded -odir build -hidir build"
  let loop (sig:ss) n = do
        let buildDir = "build"
        whenM (doesDirectoryExist buildDir) $ removeDirectoryRecursive buildDir
        createDirectoryIfMissing True buildDir
        whenM (doesFileExist "stencil") $ removeFile "stencil"
        proc "../../exec-kill-loop.hs" [cmd, "-s", show sig, "--max-delay", show maxDelay] runProcess_
        -- finish if it ran successfully but there is no output file
        whenM (doesFileExist "stencil") $ do
          exitCode <- proc "./stencil" [] runProcess
          case exitCode of
            ExitSuccess -> do
              logInfo $
                "Compiled a complete binary and ran it. Repeating. Iteration: " <> displayShow n
              loop ss (n + 1)
            ExitFailure ec -> logInfo "Failed to execuite compiled binary. Finished."
  runSimpleApp $ do
    proc "stack" ["build", "massiv"] runProcess_
    loop signals 1
