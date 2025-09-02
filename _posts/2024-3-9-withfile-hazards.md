---
layout: post
title: Two ways System.IO.withFile has messed with me
tags: [haskell]
---

## Gotcha #1: handles getting closed

This is not really `withFile`'s fault, but:

```haskell
main = do
  withFile "/tmp/foo.txt" WriteMode $ \h -> do
    (_, _, _, p1) <- createProcess ((proc "hello" []) {
      std_out = UseHandle h
      , std_err = UseHandle h
      })
    _ <- waitForProcess p1

    (_, _, _, p2) <- createProcess ((proc "hello" []) {
      std_out = UseHandle h
      , std_err = UseHandle h
      })
    _ <- waitForProcess p2
```

This fails with the following:

```
my-exe: /tmp/foo.txt: withFile: invalid argument (Bad file descriptor)
```

Can you guess why? It's because the first `createProcess` call causes the handle to be closed. Then the "bad file descriptor" exception is thrown on the second `createProcess` call.


## Gotcha #2: it catches unrelated exceptions

Now let's say we trigger an unrelated exception within a `withFile`, say by trying to start a process that doesn't exist.

```haskell
main :: IO ()
main = do
  withFile "/tmp/foo.txt" WriteMode $ \h -> do
    _ <- readCreateProcess (proc "nonexistent-exe" [])
    putStrLn "Done!"
```

This fails with

```
withfile-repro-exe: /tmp/foo.txt: withFile: does not exist (No such file or directory)
```

Extremely confusing. Makes it sounds like it failed to open the log file.
