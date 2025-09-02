---
layout: post
title: Haskell's slow close_fds problem
tags: [haskell]
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

In the beginning, Unix systems had a per-process file descriptor limit of 20. If you were writing a library that forked off processes, and you were concerned about file descriptor leaks, it was no problem to guarantee they were all closed by writing a loop up to 19 and `close`ing them all.

Over time, that limit was bumped higher and higher. A common default ended up being 1024, which can be found in some C libraries[^1]. In modern Linux there's no system-wide limit baked into the kernel. Instead, the limit is controlled on a per-process basis as part of the `RLIMIT` ("resource limit") system. This is the same system that controls a process's maximum stack size, nice value, etc.

How a process gets its `RLIMIT` values is a bit complicated. Processes have both "soft" (i.e. effective) and "hard" (i.e. maximum) limits, and in general these are inherited from their parent processes. Processes can also change their own limits with the `setrlimit` syscall. They can lower their limits, or raise them up to the hard limit. Privileged processes may raise their hard limits. Also, external systems can moderate limits. Linux's PAM ("pluggable authentication modules") are often used to apply limits to user sessions. Systemd can manage the limits of the processes it manages. Container systems like Docker can set limits too, and can layer the above things on top of each other. When your process doesn't have the limit you expected, it can be tricky to find out why!

The problem of long delays due to file descriptor closing loops has been noticed before. In Python on FreeBSD, with a file descriptor limit of 655000, the loop took [3 seconds](https://bugs.python.org/issue11284#msg129043). A web proxy server called Squid would become [unresponsive for up to a minute](https://bugzilla.redhat.com/show_bug.cgi?id=837033) on a Xen machine as multiple child processes did 32 thousand close syscalls each. And another [Python bug report](https://bugs.python.org/issue1663329) found it took 14 seconds for a `subprocess.Popen` call with a limit of `260000` descriptors. These kinds of issues influenced the workarounds that most languages eventually adopted (see below).

### Some workarounds and fixes

Because of the above history, the POSIX standard doesn't offer a system call for a process to say "I'm doing an `exec` so please close all the file descriptors I'm currently holding, except perhaps for a few like `stdout` or `stderr`." There are a number of workarounds:

* First of all, you can try looping over all file descriptors just like the old days. But even correctly finding the maximum is a bit fraught. A common approach and the one used currently in `System.Process` is to use the `sysconf` mechanism by calling `sysconf(_SC_OPEN_MAX)`
  * **Pros**: Simple.
  * **Cons**: Not perfectly correct[^2], and if the maximum really is 1 billion you're going to have a bad time.

* A process can discover its own open file descriptors by listing the directory `/proc/${pid}/fd` (Linux) or `/dev/fd` (macOS), where it can find a file named after each file descriptor.
  * **Pros**: This approach is actually `O(number of open files)`.
  * **Cons**: Parsing integers from file names feels weird. And if you're using containerization tricks like file namespaces, then `/proc` files may not be available.

* MacOS offers `proc_pidinfo`, which lets a process enumerate its file descriptors.
  * **Pros**: Same as previous.
  * **Cons**: Platform-specific.

* The `close_range` syscall, which allows the caller to close a range of file descriptors efficiently (like from FD 3 to the max number).
  * **Pros**: Solves the performance problem if you can close all your file descriptors in a few chunks.
  * **Cons**: Linux-only (and pretty recent Linux at that: kernel 5.9, released in October 2020).

* Marking file descriptors as non-inheritable by default. For example, Linux offers the `O_CLOEXEC` flag, which you can set when opening a file to ensure it will be closed on `exec`. This flag was added to macOS in 10.7 (Lion) in 2011. Windows also has an equivalent.
  * **Pros**: Doesn't require any work at `exec` time.
  * **Cons**: Getting all the platform-specific details right. And also, race conditions. Some older platforms don't allow you to atomically create a file descriptor and mark it as non-inheritable. Some platforms can do this for a file descriptor but not a socket! This [table](https://peps.python.org/pep-0446/#status-of-python-3-3) explains the available support.

### How do other languages do it?

Most languages that aim to do portable subprocess creation have had to confront this issue at some point. A lot of languages did so surprisingly late!

All of the following languages have converged on the solution of making all new file descriptors non-inheritable by default (i.e. using `O_CLOEXEC`-type mechanisms)[^3].

* Perl 1.0 (1987)
* Go 1.0 (2009)
* Python 3.4 (2013)
* Ruby 2.0 (2013)

Somehow Perl was way ahead of the curve here.

Go has an interesting locking mechanism called `ForkLock` for dealing with non-atomic file descriptor or socket creation, which you can read about [here](https://github.com/golang/go/blob/7bba745820b771307593b7278ce17464eeda2f3d/src/syscall/exec_unix.go#L19).

Python's PEP 446, which implemented non-inheritable file descriptors by default in version 3.4, has a rather inspiring statement at the beginning:

> We are aware of the code breakage this is likely to cause, and doing it anyway for the good of mankind.

Maybe Haskell's `System.Process` should follow their lead!


[^1]: <https://stackoverflow.com/questions/6798365/why-do-operating-systems-limit-file-descriptors>
[^2]: <https://stackoverflow.com/questions/899038/getting-the-highest-allocated-file-descriptor/918469#918469>
[^3]: <https://peps.python.org/pep-0446/#related-work>
