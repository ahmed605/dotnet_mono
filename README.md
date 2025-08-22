# .NET/Mono Bext Codegen

> [!WARNING]
> The Codegen is currently a work-in-progress.

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

Right now the codegen introduces two targets `ilasm-mono` and `ilasm-core`.

`ilasm-mono` target expects `ilasm` and `mono` executables to be available in the `$PATH` environment variables. Lots of Linux distros provide them via the mono packages in their official repos:

```consols
$ sudo xbps-install mono      # Void Linux
$ sudo apt install mono-devel # Ubuntu
...
```

On Windows the `mono` executable isn't used so it doesn't need to be in `$PATH`, instead the `ilasm-mono` target executes the resulting binary using .NET Framework. Both .NET Framework's `ilasm` and [.NET Core's `ilasm`](https://www.nuget.org/packages/runtime.win-x64.Microsoft.NETCore.ILAsm) can be used by the target to compile the binary on Windows.

.NET Core's `ilasm` can be also used on [Linux](https://www.nuget.org/packages/runtime.linux-x64.Microsoft.NETCore.ILAsm) and [macOS](https://www.nuget.org/packages/runtime.osx-arm64.Microsoft.NETCore.ILAsm) instead of Mono's `ilasm` if desired.

`ilasm-core` target expects .NET 9 SDK/Runtime (or later) to be installed and the `dotnet` executable to be available in the `$PATH` environment variables. Lots of Linux distros provide them via `dotnet-sdk-[version]` (and `dotnet-runtime-[version]`) packages in their official repos:

```consols
$ sudo apt install dotnet-sdk-9.0 # Ubuntu
...
```

It also expects the `ilasm` executable to be in the `$PATH` environment variables, which can be obtained from either Mono or the .NET Core ILAsm NuGet packages linked above (recommended), the .NET Framework one can be also used on Windows.