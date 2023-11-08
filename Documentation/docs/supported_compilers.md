# Supported Compilers

ITK requires a compiler with C++17 support.

## Visual Studio

* VS2017 and earlier: **NOT supported**
* MSVC toolset v142 (first shipped with VS2019): supported
* MSVC toolset v143 (first shipped with VS2022): supported

## GNU Compile Collection

* GCC 7 and later [should be supported](https://www.gnu.org/software/gcc/projects/cxx-status.html).

## LLVM Clang

* Clang 5 and later [should be supported](https://clang.llvm.org/cxx_status.html)

## AppleClang
* AppleClang 10.0.0 and later (from Xcode 10.0) [should be supported](https://en.wikipedia.org/wiki/Xcode#Version_history)

## Intel C++ Compiler
* Intel C++ 19.1 and later

## Future versions

Future versions of these compilers should be backwards compatible, and thus ITK is expected to work with them.

In the near term, ITK should continue compiling with new compiler versions, probably with some warnings. Eventually, ITK will stop compiling with newest compiler versions. But upgrading to a newer version of ITK is recommended long before then!
