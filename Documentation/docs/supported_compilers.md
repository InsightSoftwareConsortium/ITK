# Supported Compilers

ITK requires a compiler with C++17 support.

The standard compilers for common operating systems are:

- Windows: [Visual Studio](#visual-studio)
- Linux: [GCC](#gnu-compiler-collection)
- macOS: [AppleClang](#appleclang)

## Visual Studio

Visual Studio includes the MSVC compiler. It can be installed for free with the [*Visual Studio Community Edition*](https://visualstudio.microsoft.com/vs/community/) application. The paid version is [*Visual Studio Professional Edition*](https://visualstudio.microsoft.com/vs/professional/). Note that [*Visual Studio Code (aka VS Code)*](https://code.visualstudio.com/) is a different project and **does not** include a C++ compiler by default.

* VS2017 and earlier: **NOT supported**
* MSVC toolset v142 (first shipped with VS2019): supported
* MSVC toolset v143 (first shipped with VS2022): supported

## GNU Compiler Collection

GCC is installed on Ubuntu Linux with:

```sh
apt install build-essential
```

* GCC 7 and later [should be supported](https://www.gnu.org/software/gcc/projects/cxx-status.html).

## LLVM Clang

* Clang 5 and later [should be supported](https://clang.llvm.org/cxx_status.html)

## AppleClang

AppleClang is included with the XCode application and is available from the Apple App Store.

To trigger installation of the Xcode Command Line tools, run:

```sh
xcode-select --install
```

* AppleClang 10.0.0 and later (from Xcode 10.0) [should be supported](https://en.wikipedia.org/wiki/Xcode#Version_history)

## Intel C++ Compiler

* Intel C++ 19.1 and later

## Future versions

Future versions of these compilers should be backwards compatible, and thus ITK is expected to work with them.

In the near term, ITK should continue compiling with new compiler versions, probably with some warnings. Eventually, ITK will stop compiling with newest compiler versions. But upgrading to a newer version of ITK is recommended long before then!
