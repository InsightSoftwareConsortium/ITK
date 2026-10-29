# Build and test ITK

The [CMake] build system was initially created to support development of ITK; ITK is built following standard CMake practices.

ITK's tests are defined with CTest, and the tests can be executed via `ctest`.

## FFTW

Enable `ITK_USE_FFTWF` for single precision or `ITK_USE_FFTWD` for double
precision. Either option adds the `ITKFFTW` third-party module to `ITKFFT`.
Enabling `Module_ITKFFTW` with neither option is a configuration error.
FFTW is GPL licensed; linking it changes the licensing requirements of
the resulting binaries.

By default, `ITKFFTW` downloads FFTW 3.3.10 at configuration time with CMake
`FetchContent`. Both precisions share one source download and use separate
build directories. Set `FETCHCONTENT_SOURCE_DIR_FFTW` to an unpacked FFTW
source directory to build offline, or set `ITK_USE_SYSTEM_FFTW=ON` to use an
installed library instead.

`FFTW_ENABLE_NEON`, `FFTW_ENABLE_SSE`, `FFTW_ENABLE_SSE2`,
`FFTW_ENABLE_AVX`, and `FFTW_ENABLE_AVX2` control SIMD codelets. Defaults
follow the target architecture and compiler flags, not the build host.
Link consumers to `ITK::ITKFFTModule` to inherit FFTW dependencies.

Set `ITK_USE_MKL=ON` to use the FFTW interface of the Intel oneAPI Math
Kernel Library (oneMKL) 2021.3 or newer instead; it turns on both
precisions and `ITK_USE_SYSTEM_FFTW`. ITK finds oneMKL's CMake package
from `MKL_DIR`, or from `MKLROOT` (the oneAPI `setvars` script sets the
environment variable), and links `MKL::MKL` statically with the ILP64
interface and sequential threading unless `MKL_LINK`, `MKL_INTERFACE`, or
`MKL_THREADING` is set. oneMKL is 64-bit only and no longer supports
macOS. Projects that use ITK reuse ITK's `MKL_DIR` unless they set their
own `MKL_DIR` or `MKLROOT`.

## Pixi

An easy and reproducible way to build and test ITK for development is via [Pixi]. Pixi will provide locally-scoped build tools such as toolchains, `cmake`, `ninja`,  and `python`.

First, install `pixi`,

::::{tab-set}

:::{tab-item} Linux & macOS
```shell
curl -fsSL https://pixi.sh/install.sh | bash
```
:::

:::{tab-item} Windows
`PowerShell`:
```powershell
iwr -useb https://pixi.sh/install.ps1 | iex
```
`winget`:
```
winget install prefix-dev.pixi
```
:::

::::

or use [another pixi installation option].

You might need to restart your terminal or source your shell for the changes to take effect.

ITK's Pixi configuration uses the [conda-forge compiler] and the [Ninja] CMake generator.

### Test C++

To configure, build, and run ITK's C++ tests,

```shell
# Change to the ITK source directory
cd src/ITK

pixi run test
```

### Test Python

To configure, build, and run ITK's Python bindings and run the Python and C++ tests,

```shell
# Change to the ITK source directory
cd src/ITK

pixi run test-python
```

To run a python interpreter with the locally built `itk` Python package:

```shell
pixi run python-exe
```

A Python script can also be passed in, e.g.

```shell
pixi run python-exe ./test.py
```

To run a python interpreter with a locally built `itk` Python package with debug symbols,

```shell
pixi run python-exe-debug
```

### Further testing and development

Additional pixi tasks to run specific steps of the `configure`, `build`, `test` development process or create builds with other [CMake build types] are listed with

```shell
pixi task list
```

To start up a shell environment with all build dependencies:

```shell
pixi shell
```

Once in the pixi shell, `cmake`, `ninja`, `ctest`, etc can be executed in the local `build/` directory containing the build to perform targeted development.

[another pixi installation option]: https://pixi.sh/latest/#installer-script-options
[CMake]: https://cmake.org/
[CMake build types]: https://cmake.org/cmake/help/latest/variable/CMAKE_BUILD_TYPE.html
[conda-forge compiler]: https://anaconda.org/conda-forge/cxx-compiler
[Ninja]: https://ninja-build.org/
[Pixi]: https://pixi.sh/
