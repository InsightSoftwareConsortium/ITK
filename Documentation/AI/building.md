# ITK Build System

## Recommended: Pixi

Pixi is the preferred build tool for development. It manages the toolchain and
runs CMake + Ninja internally.

```bash
pixi run --as-is build          # Build C++ tests
pixi run --as-is build-python   # Build Python tests
pixi run --as-is test           # Run C++ tests
pixi run --as-is test-python    # Run Python tests

pixi shell -e cxx               # Interactive C++ dev shell
pixi shell -e python            # Interactive Python dev shell
```

See `pixi.toml` for the full list of tasks and environment definitions.

## Direct CMake

ITK requires CMake 3.22.1+.

**Standard build:**
```bash
cmake -B build -S . \
  -DCMAKE_BUILD_TYPE=Release \
  -DITK_BUILD_DEFAULT_MODULES=ON \
  -DBUILD_TESTING=ON \
  -DBUILD_EXAMPLES=OFF
cmake --build build -j$(nproc)
```

**Python wrapping:**
```bash
cmake -B build-python -S . \
  -DITK_WRAP_PYTHON=ON \
  -DITK_WRAP_unsigned_short=ON \
  -DITK_WRAP_IMAGE_DIMS="2;3;4"
```

**Select specific modules (faster builds):**
```cmake
set(ITK_BUILD_DEFAULT_MODULES OFF)
set(Module_ITKCommon ON)
set(Module_ITKIOImageBase ON)
```

## Finding Required Modules

```bash
python Utilities/Maintenance/WhatModulesITK.py /path/to/ITK/source file1.cxx file2.h
```

## External Projects

```cmake
find_package(ITK COMPONENTS ITKCommon ITKIOImageBase REQUIRED)
```

```bash
cmake -B build-external -S path/to/module -DITK_DIR=/path/to/ITK-build
```
