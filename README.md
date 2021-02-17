# Haskell-FileManager

A simple file manager written on Haskell with completely pure testing library.

Basically, this project contains two monad stacks:
* Real-world stack which maintains current directory and calls IO functions.
* Testing stack which does not require IO and emulates file system.

## Building and executing
This project requires `stack`.

Build: `stack build`

Test: `stack test`

Run: `stack run`

## Available commands

* `dir` - shows contents of current directory
* `ls <DIR>` - shows contents of `DIR`
* `cd <DIR>` - changes current directory
* `mkdir <PATH>` - creates new directory
* `touch <PATH>` - creates new empty file
* `cat <FILE>` - shows contents of specified file
* `rm <FILE>` - deletes file
* `rmdir <DIR>` - deletes directory. Can delete non-empty directories
* `writefile <PATH> <TEXT>` - writes text to specified file
* `infofile <PATH>` - shows information of specified file (permissions, size, timestamp, path, extension)
* `find <DIR> <FILE>` - searches file recursively in specified directory and shows path to it
* `--help` - shows help
* `exit` - exit app
