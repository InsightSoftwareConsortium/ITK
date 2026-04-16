# ITK Conda Packages Reference

This document provides context for working on the `Utilities/conda-packages/` directory,
which contains a rattler-build recipe producing three conda packages from a
single ITK build: `libitk`, `libitk-devel`, and `libitk-wrapping`.

## Why This Exists

The conda-forge `libitk-feedstock` (external repo, ITK 5.4.5) maintains
`libitk` and `libitk-devel` as separate packages from the main ITK repo.
This causes recipes to lag releases and requires external maintainer
coordination. The `libitk-wrapping` package (new — not on conda-forge) bundles
compiled SWIG artifacts so that downstream remote module CI can skip ITK
recompilation, cutting build times from ~1–2 hours to ~15 minutes.

Consensus from InsightSoftwareConsortium/ITKPythonPackage#302: move recipe infrastructure to the main ITK repo so
recipes stay in sync with releases automatically.

## Package Split

| Package | Contents | Install components |
|---------|----------|--------------------|
| `libitk` | Shared libraries (`.so`/`.dylib`/`.dll`) | `Runtime`, `RuntimeLibraries`, `Libraries`, `Unspecified`, `libraries` |
| `libitk-devel` | Headers, CMake config (`ITKConfig.cmake`, `ITKTargets.cmake`), static libs | `Development`, `Headers` |
| `libitk-wrapping` | Python extension modules (`.so`), `itk` Python package files | `PythonWrappingRuntimeLibraries` |

The wrapping component name comes from the CMake flag
`-DWRAP_ITK_INSTALL_COMPONENT_IDENTIFIER:STRING=PythonWrapping`, which causes
all wrapping install rules to use component `PythonWrappingRuntimeLibraries`
(pattern: `${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}RuntimeLibraries`).

## Recipe Format: rattler-build vs conda-build

The feedstock uses `meta.yaml` (conda-build format). This repo uses
`recipe.yaml` (rattler-build format). Key differences:

| Feature | conda-build (feedstock) | rattler-build (this repo) |
|---------|------------------------|--------------------------|
| Format | `meta.yaml` | `recipe.yaml` |
| Build cache | Rebuilds ITK 3× (once per output) | Single build via `staging:` + `inherit:` |
| Selectors | `# [condition]` inline comments | `if/then/else` blocks |
| Jinja | `{{ compiler('cxx') }}` | `${{ compiler('cxx') }}` |
| Pin subpackage | `{{ pin_subpackage(...) }}` | `${{ pin_subpackage(...) }}` |

The `staging:` + `inherit: build-cache` pattern in `recipe.yaml` builds ITK
once and reuses the build tree for all three output install phases. This is
the primary reason for using rattler-build over conda-build for this recipe.

## CMake Flag Alignment with conda-forge Feedstock

Reference: `conda-forge/libitk-feedstock` recipe at ITK 5.4.5.

### Flags shared with feedstock (both use these)

```cmake
-DBUILD_SHARED_LIBS:BOOL=ON
-DBUILD_TESTING:BOOL=OFF
-DBUILD_EXAMPLES:BOOL=OFF
-DITK_BUILD_DEFAULT_MODULES:BOOL=ON
-DITK_USE_KWSTYLE:BOOL=OFF
-DITK_DEFAULT_THREADER:STRING=Pool
-DITK_USE_SYSTEM_HDF5:BOOL=ON
-DITK_USE_SYSTEM_JPEG:BOOL=ON
-DITK_USE_SYSTEM_TIFF:BOOL=ON
-DITK_USE_SYSTEM_EIGEN:BOOL=ON
-DITK_USE_SYSTEM_FFTW:BOOL=ON
-DITK_USE_FFTWD:BOOL=ON
-DITK_USE_FFTWF:BOOL=ON
-DModule_ITKReview:BOOL=ON
-DModule_MGHIO:BOOL=ON
-DModule_ITKTBB:BOOL=OFF   # macOS; ON for Linux/Windows
```

### Flags in our recipe not in feedstock

```cmake
-DITK_FORBID_DOWNLOADS:BOOL=ON       # prevents remote module downloads at configure time
-DITK_WRAP_PYTHON:BOOL=ON            # required for libitk-wrapping output
-DWRAP_ITK_INSTALL_COMPONENT_IDENTIFIER:STRING=PythonWrapping
-DPython3_EXECUTABLE:FILEPATH="${PYTHON}"
```

### Flags in feedstock NOT in our recipe (investigate before upstreaming)

```cmake
# Feedstock sets CMAKE_FIND_ROOT_PATH and CMAKE_FIND_ROOT_PATH_MODE_* to
# constrain all find_package/find_library calls to the conda PREFIX.
# We use CMAKE_PREFIX_PATH instead. These may matter for cross-compilation.
-DCMAKE_FIND_ROOT_PATH:PATH="${PREFIX}"
-DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE:STRING=ONLY
-DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY:STRING=ONLY
-DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM:STRING=NEVER
-DCMAKE_FIND_ROOT_PATH_MODE_PACKAGE:STRING=ONLY
-DCMAKE_FIND_FRAMEWORK:STRING=NEVER
-DCMAKE_FIND_APPBUNDLE:STRING=NEVER

# macOS-specific GDCM fix (feedstock has this, we do not)
-DGDCM_USE_COREFOUNDATION_LIBRARY:BOOL=OFF

# NIFTI math lib — feedstock passes empty string to disable
-DNIFTI_SYSTEM_MATH_LIB=

# Module_SimpleITKFilters — feedstock enables this, we do not
# It requires SimpleITK to be available; needs investigation for 6.0.0
-DModule_SimpleITKFilters=ON
```

### Modules omitted from our recipe that the feedstock enables

These are in the feedstock (5.4.5) but cannot be included here:

| Module | Why omitted |
|--------|-------------|
| `Module_ITKIOTransformMINC` | Marked `EXCLUDE_FROM_DEFAULT` in `itk-module.cmake`; depends on `ITKMINC` (libminc), which is not in the recipe host requirements. The library appears in `ITKTargets.cmake` but isn't installed, breaking downstream `find_package(ITK)`. Needs a `libminc` host dep and install component investigation before re-enabling. |
| `Module_GenericLabelInterpolator` | Remote module (uses `itk_fetch_module()` to clone from GitHub). Incompatible with `-DITK_FORBID_DOWNLOADS=ON`. |
| `Module_AdaptiveDenoising` | Same as above — remote module requiring git fetch at configure time. |

## Bug Fixes Required for System Library Builds

These five commits were added to the branch to make the conda system-library
build work. All are legitimate fixes for issues that appear when
`ITK_USE_SYSTEM_*=ON` and do not regress the bundled-library path.

| Commit | File(s) | Root cause |
|--------|---------|------------|
| `cdc0eaaeb7` | `Modules/ThirdParty/HDF5/CMakeLists.txt` | `itk_module_impl()` prepends `ITK_LIBRARY_NAMESPACE::` to all dep names. System HDF5 exports bare targets (`hdf5-shared`, not `ITK::hdf5-shared`). Bundled path creates wrappers via `itk_module_target()`; system path (`ITKHDF5_NO_SRC=1`) skips that. Fix: create `ITK::hdf5-*` INTERFACE IMPORTED wrapper targets when `ITK_USE_SYSTEM_HDF5=ON`. |
| `db09bd1371` | `Modules/ThirdParty/Eigen3/CMakeLists.txt` | Modules with only `COMPILE_DEPENDS` on ITKEigen3 (not link deps) rely on `include_directories(${ITKEigen3_INCLUDE_DIRS})` from `_itk_module_use_recurse()`. A prior commit dropped the `get_target_property()` that populated this variable. Fix: restore it. |
| `63a9c6ba41` | `Modules/Nonunit/Review/itk-module.cmake` | `itkVoxBoCUBImageIO.cxx` calls `gzopen/gzclose/gzread/gzwrite` directly. Missing explicit dep masked by Linux lazy dynamic linking; fatal on macOS ARM64. Fix: add `ITKZLIB` to `PRIVATE_DEPENDS`. |
| `090106d316` | `CMake/ITKModuleMacros.cmake` | BUILD backward-compat shims had `if(NOT TARGET)` guard; EXPORT_CODE_INSTALL shims did not. When a downstream project calls `find_package(HDF5)` before `find_package(ITK)`, bare HDF5 targets already exist and the shim `add_library` call fails. Fix: add the same guard to EXPORT_CODE_INSTALL. |
| `a8d7a90430` | `CMake/ITKConfig.cmake.in`, `Modules/ThirdParty/HDF5/CMakeLists.txt` | **Root cause of libitk-devel test failure.** `ITKTargets.cmake` sets `INTERFACE_LINK_LIBRARIES` on `ITK::ITKHDF5Module` to `ITK::hdf5-shared` etc. CMake policy CMP0028 (NEW) requires those `::` targets to exist at `include()` time. Fix part 1: reorder `ITKConfig.cmake.in` so `itk_module_config()` runs before `include(ITKTargets.cmake)`. Fix part 2: extend `ITKHDF5_EXPORT_CODE_INSTALL` to also create the `ITK::hdf5-*` wrappers in the installed config (so downstream `find_package(ITK)` in the libitk-devel test works). |

## Platform Matrix

| Platform | Runner | Notes |
|----------|--------|-------|
| `linux-64` | `ubuntu-latest` | Native; all system deps ON |
| `linux-aarch64` | `ubuntu-24.04-arm` | Native ARM runner |
| `osx-arm64` | `macos-latest` | TBB disabled (`use_tbb=OFF`) |
| `osx-64` | `macos-13` | Last Intel runner; TBB disabled |
| `win-64` | `windows-latest` | EXPAT, PNG, ZLIB bundled (not system); TBB ON |

Cross-compilation (e.g. osx-64 host building osx-arm64) uses
`Utilities/conda-packages/cross-compile/TryRunResults.cmake` with pre-computed
`TRY_RUN` results. Sourced from the conda-forge feedstock's
`TryRunResults-osx-arm64.cmake`.

## Build Guard (Iteration Helper)

`Utilities/conda-packages/build.sh` exits early if `CMakeCache.txt` already exists in
`$SRC_DIR/build/`. This lets you iterate on install scripts and tests without
recompiling ITK. Use `rattler-build build --keep-build` to preserve the work
directory between runs. Delete `$SRC_DIR/build/CMakeCache.txt` to force a
clean rebuild.

## libitk-devel Test

The CMake build test in `Utilities/conda-packages/tests/example/` calls
`find_package(ITK)` on the installed `libitk-devel` package. This test
validates the complete install chain: ITKConfig.cmake ordering, HDF5
EXPORT_CODE wrapper creation, and ITKTargets.cmake integrity.

The test cmake project uses `find_package(ITK REQUIRED)` and
`include(${ITK_USE_FILE})`, so any broken target reference in
ITKTargets.cmake surfaces here first.

## Relationship to ITKPythonPackage PR #302

InsightSoftwareConsortium/ITKPythonPackage#302 is a Python build system
refactor for generating ITK Python wheels. The relevant overlap:

- The PR introduces `libitk-wrapping` as a build dependency concept: remote
  modules would detect a pre-built `libitk-wrapping` conda package via
  `CONDA_PREFIX/lib/cmake/ITK-*/ITKConfig.cmake` and skip recompiling ITK.
- Consensus (thewtex, 2026-04-14): move conda recipe infrastructure to the
  main ITK repo. This branch implements that consensus.
- The `libitk-wrapping` package in this recipe is the C++ side of the
  speedup described in that PR (Python wrapping artifacts pre-built and
  installable without recompiling ITK from source).

## conda-forge Feedstock Relationship

The conda-forge `libitk-feedstock` (https://github.com/conda-forge/libitk-feedstock)
is currently at ITK 5.4.5 using `meta.yaml` (conda-build format). Once this
recipe is merged and stable, the feedstock should be updated to:
1. Migrate from `meta.yaml` to `recipe.yaml` (rattler-build format)
2. Reference or inherit from the canonical recipe in this repo
3. Add `libitk-wrapping` as a third output

The feedstock maintainers are @blowekamp and @bluescarni.

## Running Locally

Two equivalent interfaces. Both require `preview = ["pixi-build"]` in
`[tool.pixi.workspace]` of `pyproject.toml` (already set). The `--experimental`
flag is required for direct `rattler-build` invocations because this recipe
uses the `staging:` + `inherit:` build-cache pattern, which is gated behind
rattler-build's experimental feature flag.

```bash
# Via pixi build (staging outputs enabled by workspace preview flag)
pixi build \
    --path Utilities/conda-packages/recipe.yaml \
    --output-dir /tmp/itk-conda-test/

# Via rattler-build directly (also supports --keep-build for fast iteration)
pixi run --environment dev rattler-build build --experimental \
    --recipe Utilities/conda-packages/recipe.yaml \
    --output-dir /tmp/itk-conda-test/ \
    --keep-build

# Render only — validate YAML without building
pixi run --environment dev rattler-build build --experimental \
    --recipe Utilities/conda-packages/recipe.yaml \
    --render-only
```

`rattler-build build --keep-build` is preferred during development because it
preserves the work directory so subsequent runs skip the cmake compile step
(see build guard in `build.sh`).

For opt-in ccache passthrough (fast incremental rebuilds when the work
directory is discarded between runs), see
[`README_Advanced.md`](./README_Advanced.md).
