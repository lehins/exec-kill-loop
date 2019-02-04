# exec-kill-loop - Repro for GHC-non-atomic-file-write problems

GHC as of writing does not ensure that files are written atomically.

This means that a Ctrl+C, kill or reboot at the right time can result in truncated files.
GHC does not detect this, so that resuming/rerunning the build with `ghc --make` continues to show up error messages.

In such situation, the only workaround is to wipe all files (e.g. `stack`/`cabal`/`make` `clean`).

Specifically, we've observed the following to happen:

* object files being written half-way, resulting in persistent linker errors
* executable files being written half-way, so they can start executing but then crash
* users reporting downstream tooling bug reports that only a build directory wipe helped

Issues about this:

* `stack` issue [#4559 - Get rid of persistent build errors due to non-atomic file writes in GHC and stack](https://github.com/commercialhaskell/stack/issues/4559) for more work on this.
* GHC issue [#14533 - Make GHC more robust against PC crashes by using atomic writes](https://ghc.haskell.org/trac/ghc/ticket/14533)


## Usage

```bash
cd sample/stencil/stack-stencil/
./run.hs
```

This script may take hours to run until it finds an issue.


## Example output

See [`emptyObjectFile.log`](sample/stencil/emptyObjectFile.log) for where the script detects a situation where sending a KILL signal to GHC at the right time results in a corrupted object file being created.


## Planned solution

GHC should use atomic writes (write to temp file, then `rename()` syscall).
