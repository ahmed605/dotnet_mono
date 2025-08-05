# .NET/Mono Bext Codegen

> [!WARNING]
> The Codegen is currently a work-in-progress and does not have anything useful implemented in it.

This is a codegen for [the B extended compiler](https://github.com/bext-lang/b). It introduces targets that produce [.NET](https://dotnet.microsoft.com/en-us/)/[Mono](https://www.mono-project.com/) compatible binaries.

## Installation

```console
$ git clone https://github.com/bext-lang/b
$ cd b/src/codegen/
$ git clone https://github.com/bext-lang/dotnet-mono
$ cd ../../
$ make
$ ./build/b -tlist
```

## Targets and Dependencies

Right now the codegen introduces only one target `ilasm-mone`. It expects `ilasm` and `mono` executables to be available in the `$PATH` environment variables. Lots of Linux distros provide them via the mono packages in their official repos:

```consols
$ sudo xbps-install mono      # Void Linux
$ sudo apt install mono-devel # Ubuntu
...
```
