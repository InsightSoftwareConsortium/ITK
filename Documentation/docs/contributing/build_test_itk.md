# Build and test ITK

The [CMake] build system was initially created to support development of ITK; ITK is built following standard CMake practices.

ITK's tests are defined with CTest, and the tests can be executed via `ctest`.

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
