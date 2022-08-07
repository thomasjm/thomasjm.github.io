
# Haskell's `fork/exec` problem

The following code forks an external process to print to `stdout`:

```haskell
module Main where

import System.Process

main :: IO ()
main = do
  (_, _, _, p) <- createProcess (shell "echo hiiiii")
  _ <- waitForProcess p
  return ()
```

With one simple configuration change, this code brings my system to its knees.

```bash
ulimit -n 1073741815
```

asdfasdf
