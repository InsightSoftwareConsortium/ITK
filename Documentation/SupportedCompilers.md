ITK requires a compiler with C++11 support.

# Visual Studio
* VS2013 and earlier: **NOT supported**
* MSVC toolset v140 (first shipped with VS2015): supported
* MSVC toolset v141 (first shipped with VS2017): supported
* MSVC toolset v142 (first shipped with VS2019): supported

# GNU Compile Collection
* GCC 4.8.1 and later [should be supported](https://www.gnu.org/software/gcc/projects/cxx-status.html).

# LLVM Clang
* Clang 3.3 and later [should be supported](https://clang.llvm.org/cxx_status.html)

# AppleClang
* AppleClang 7.0.2 and later (from Xcode 7.2.1) [should be supported](https://en.wikipedia.org/wiki/Xcode#Version_history)

# Intel C++ Compiler
* Intel C++ 15.0.4 and later

# Future versions
Future versions of these compilers should be backwards compatible, and thus ITK is expected to work with them.

In the near term, ITK should continue compiling with new compiler versions, probably with some warnings. Eventually, ITK will stop compiling with newest compiler versions. But upgrading to a newer version of ITK is recommended long before then!
