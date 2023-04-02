---
layout: post
title: Haskell's slow close_fds problem
---

On most Linux systems, the following simple Haskell program will print "hiii" immediately. But with one simple configuration change, it can bring your system to its knees for several minutes.

```haskell
module Main where

import System.Process

main :: IO ()
main = do
  (_, _, _, p) <- createProcess (shell "echo hiii")
  _ <- waitForProcess p
  return ()
```

Can you guess the configuration change (with the hint that this post is about file descriptor limits)?

<details style="margin-bottom: 1em">
    <summary>Spoiler warning</summary>

    The configuration change is to run <code>ulimit -n 1073741815</code>, thereby raising your file descriptor limit to 1 billion.

    (You might have to raise your system's hard limit and/or any user-specific limits before you can do this.)
</details>


Why does this happen? Well, the default `CreateProcess` values used in `System.Process` contain the flag `close_fds=True`. This is the flag that says to close all the file descriptors from the parent process before `exec`-ing the child process, and it's pretty important from a security perspective.

Processing this flag eventually leads into some C code that looks like this:

```c
if ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0) {
    int max_fd = get_max_fd();
    // XXX Not the pipe
    for (int i = 3; i < max_fd; i++) {
        if (i != forkCommunicationFds[1]) {
            close(i);
        }
    }
}
```

We're brute-force looping over all possible file descriptors available to the process in order to close them all!

I first encountered this problem on a somewhat unusual system: NixOS running a Kind Kubernetes cluster, where my Haskell code was running on a Kubernetes pod whose file descriptor limit ended up being ~**1 billion**. The reason why is a complicated story about how NixOS is set up (with an unbounded file descriptor limit on the init process) and the containerization magic used to run a local Kubernetes cluster. *Suffice to say, I did not expect to be running a billion `close` syscalls on every fork and it was not good for performance.*

On a more normal system like my Ubuntu laptop, the file descriptor limit is **1 million**. Doing that many unnecessary syscalls on every `fork/exec` is not a huge catastrophe for most workloads--I had never noticed it before--but it's still not great.

Digging into this problem takes you into a real rabbit hole of POSIX history and workarounds that have been applied over the years and in various languages.

### A little bit of history

In the beginning[^1], Unix systems had a per-process file descriptor limit of 20. If you were writing a library that forked off processes, and you were concerned about file descriptor leaks, it was no problem to guarantee they were all closed by writing a loop up to 19 and `close`ing them all.

Over time, that limit was bumped higher and higher. A common default ended up being 1024, which can be found in some C libraries[^2]. In modern Linux there's no system-wide limit baked into the kernel. Instead, the limit is controlled on a per-process basis as part of the `RLIMIT` ("resource limit") system. This is the same system that controls a process's maximum stack size, nice value, etc.

How a process gets its `RLIMIT` values is a bit complicated. Processes have both "soft" (i.e. effective) and "hard" (i.e. maximum) limits, and in general these are inherited from their parent processes. Processes can also change their own limits with the `setrlimit` syscall. They can lower their limits, or raise them up to the hard limit. Privileged processes may raise their hard limits. Also, external systems can moderate limits. Linux's PAM ("pluggable authentication modules") are often used to apply limits to user sessions. Systemd can manage the limits of the processes it manages. Container systems like Docker can set limits too, and can layer the above things on top of each other. When your process doesn't have the limit you expected, it can be tricky to find out why!

### The workarounds

Because of the above history, the POSIX standard doesn't offer a system call for a process to say "I'm doing an `exec` so please close all the file descriptors I'm currently holding, except perhaps for a few like `stdout` or `stderr`." There are a number of workarounds:

* First of all, you can try looping over all file descriptors just like the old days. But even correctly finding the maximum is a bit fraught. A common approach and the one used currently in `System.Process` is to use the `sysconf` mechanism by calling `sysconf(_SC_OPEN_MAX)`
**Pros**: Simple.
**Cons**: Not perfectly correct[^3], and if the maximum really is 1 billion you're going to have a bad time.

* A process can discover its own open file descriptors by listing the directory `/proc/${pid}/fd` (Linux) or `/dev/fd` (macOS), where it can find a file named after each file descriptor.
**Pros**: This approach is actually `O(number of open files)`.
**Cons**: Parsing integers from file names feels weird. And if you're using containerization tricks like file namespaces, then `/proc` files may not be available.

* MacOS offers `proc_pidinfo`, which lets a process enumerate its file descriptors.
**Pros**: Same as previous.
**Cons**: Platform-specific.

* Linux offers the `O_CLOEXEC` flag, which you can set when opening a file to ensure it will be closed on `exec`.
**Pros**: Simple, doesn't require any work at `exec` time.
**Cons**: Platform-specific. Can't guarantee this flag is always used, so can't be relied upon in a general forking library like `System.Process`.



### How does Python do it?


### What about static linking?


### Links

https://github.com/python/cpython/blob/main/Modules/_posixsubprocess.c

https://peps.python.org/pep-0446/#related-work

https://bugs.python.org/issue1663329



### How stable are syscalls really?



[^1]: Or so I've read; I wasn't alive for this.
[^2]: https://stackoverflow.com/questions/6798365/why-do-operating-systems-limit-file-descriptors
[^3]: https://stackoverflow.com/questions/899038/getting-the-highest-allocated-file-descriptor/918469#918469
