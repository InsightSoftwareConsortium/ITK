# Branch Audit: `add-conda-recipe`

## What This Branch Does

Adds `Utilities/conda-packages/` to the ITK main repo with a rattler-build `recipe.yaml`
that produces three conda packages from a single ITK build: `libitk` (runtime
libraries), `libitk-devel` (headers + CMake config), and `libitk-wrapping`
(Python SWIG artifacts). Also contains five CMake bug fixes required to make
ITK build correctly against system (conda-forge) libraries.

Full context: `Utilities/conda-packages/README.md`.

---

## Bug Fixes (5 commits)

Each fix targets a failure mode specific to `ITK_USE_SYSTEM_*=ON` builds.
None affect the bundled-library path.

### `cdc0eaaeb7` — HDF5 namespace mismatch
**File:** `Modules/ThirdParty/HDF5/CMakeLists.txt`

When `ITK_USE_SYSTEM_HDF5=ON`, the bundled HDF5 source (`src/CMakeLists.txt`)
is skipped (`ITKHDF5_NO_SRC=1`), so `itk_module_target()` never runs. That
function is what normally creates `ITK::hdf5-shared` etc. as alias targets.
Meanwhile, `itk_module_impl()` prepends `ITK_LIBRARY_NAMESPACE::` to all
dependency names, so it looks for `ITK::hdf5-shared` — which doesn't exist.

**Fix:** Create `ITK::hdf5-*` INTERFACE IMPORTED wrapper targets around the
bare system HDF5 targets when `ITK_USE_SYSTEM_HDF5=ON`.

**Verify:** Build with `-DITK_USE_SYSTEM_HDF5=ON` and confirm no "target not
found" CMake errors for `ITK::hdf5-shared`.

---

### `db09bd1371` — Eigen3 include dirs missing for COMPILE_DEPENDS modules
**File:** `Modules/ThirdParty/Eigen3/CMakeLists.txt`

Modules that declare only `COMPILE_DEPENDS ITKEigen3` (e.g.
`ITKDiffusionTensorImage`) are not linked through the target chain, so they
don't receive transitive `INTERFACE_INCLUDE_DIRECTORIES` from `Eigen3::Eigen`.
They rely on `include_directories(${ITKEigen3_INCLUDE_DIRS})` called by
`_itk_module_use_recurse()`. A prior upstream commit dropped the
`get_target_property()` that populated this variable.

**Fix:** Restore the `get_target_property()` call that extracts
`INTERFACE_INCLUDE_DIRECTORIES` from `Eigen3::Eigen` into `ITKEigen3_INCLUDE_DIRS`.

**Verify:** Build with `-DITK_USE_SYSTEM_EIGEN=ON`; confirm no "Eigen/Eigenvalues
not found" compile errors in modules that only have COMPILE_DEPENDS on ITKEigen3.

---

### `63a9c6ba41` — Missing ITKZLIB dependency in ITKReview
**File:** `Modules/Nonunit/Review/itk-module.cmake`

`itkVoxBoCUBImageIO.cxx` calls `gzopen`, `gzclose`, `gzread`, `gzwrite`
directly via `itk_zlib.h`. ITKReview had no explicit dependency on ITKZLIB.
This was masked on Linux by lazy dynamic linking but produced a link failure
on macOS ARM64 (which requires all symbols resolved at link time).

**Fix:** Add `ITKZLIB` to `PRIVATE_DEPENDS` in ITKReview's `itk-module.cmake`.

**Verify:** Build on macOS ARM64 with `Module_ITKReview=ON`; confirm no
undefined symbol errors for `gzopen` etc.

---

### `090106d316` — Missing `if(NOT TARGET)` guard in EXPORT_CODE_INSTALL shims
**File:** `CMake/ITKModuleMacros.cmake`

The BUILD backward-compatibility shims (created at ITK build time for
downstream projects building against the build tree) already had an
`if(NOT TARGET ${dep})` guard. The EXPORT_CODE_INSTALL shims (embedded in
the installed `ITKConfig.cmake` for installed-package consumers) did not.
When a downstream project calls `find_package(HDF5)` before
`find_package(ITK)`, the bare HDF5 targets already exist and the shim
`add_library()` call fails with "target already exists".

**Fix:** Add the same `if(NOT TARGET)` guard to the EXPORT_CODE_INSTALL shims.

**Verify:** In a downstream CMake project, call `find_package(HDF5)` then
`find_package(ITK)` and confirm no "cannot create imported target" error.

---

### `a8d7a90430` — ITKConfig.cmake ordering + HDF5 EXPORT_CODE in installed config
**Files:** `CMake/ITKConfig.cmake.in`, `Modules/ThirdParty/HDF5/CMakeLists.txt`

**Root cause of libitk-devel test failure.**

`ITKTargets.cmake` sets `INTERFACE_LINK_LIBRARIES` on `ITK::ITKHDF5Module` to
`ITK::hdf5-shared` etc. CMake policy CMP0028 (NEW) requires all `::` targets
to exist at the time `set_target_properties()` is called (i.e. at
`include(ITKTargets.cmake)` time). The `ITK::hdf5-*` wrappers are created
inside module EXPORT_CODE, which runs during `itk_module_config()`.

**Fix part 1 (`ITKConfig.cmake.in`):** Reorder so `itk_module_config()` runs
before `include(ITKTargets.cmake)`.

**Fix part 2 (`HDF5/CMakeLists.txt`):** The build-time fix (`cdc0eaaeb7`)
creates wrappers during the ITK build. The installed config also needs them.
Extend `ITKHDF5_EXPORT_CODE_INSTALL` (the code embedded in the installed
`ITKConfig.cmake`) to also create `ITK::hdf5-*` INTERFACE IMPORTED wrappers
in downstream CMake sessions.

**Verify:** Install libitk-devel, then run the `Utilities/conda-packages/tests/example/`
CMake build test: `cmake -DCMAKE_PREFIX_PATH=$PREFIX ../tests/example &&
cmake --build .` — must complete without errors.

---

## Recipe Implementation

### Structure
Single rattler-build `recipe.yaml` with a `staging:` build cache (compiles ITK
once) and three `outputs:` that run separate install scripts against the shared
build tree. See `Utilities/conda-packages/README.md` for the full package split and
install component mapping.

### Known gaps vs conda-forge feedstock (non-blocking)
These are in the feedstock but not yet in this recipe. They should be
investigated before or after upstreaming:

| Item | Notes |
|------|-------|
| `CMAKE_FIND_ROOT_PATH_MODE_*` flags | Feedstock constrains all CMake finds to conda PREFIX. We use `CMAKE_PREFIX_PATH` instead. May matter for cross-compilation edge cases. |
| `GDCM_USE_COREFOUNDATION_LIBRARY:BOOL=OFF` | macOS-specific GDCM fix; unclear if still needed in 6.0.0. |
| ~~`NIFTI_SYSTEM_MATH_LIB=`~~ | **Implemented in `build.sh`.** `find_library(… m)` in `Modules/ThirdParty/NIFTI/src/nifti/CMakeLists.txt` was baking the rattler-build sandbox sysroot path into exported `ITKTargets.cmake`, causing downstream consumers (e.g. libitk-devel test) to fail with `ninja: error: '…/build_env/…/sysroot/usr/lib/libm.so' missing`. |
| `Module_SimpleITKFilters` | Feedstock enables this; requires SimpleITK available at configure time. Not investigated for 6.0.0 compatibility. |

### Modules intentionally omitted
`Module_ITKIOTransformMINC` — `EXCLUDE_FROM_DEFAULT`, needs `libminc` host dep
not in recipe; caused missing-dylib error in libitk-devel test. Removed.

`Module_GenericLabelInterpolator`, `Module_AdaptiveDenoising` — remote modules
using `itk_fetch_module()` (git clone at configure time); incompatible with
`-DITK_FORBID_DOWNLOADS=ON`. Removed.

---

## Open Investigation: "Overdepending" warnings on system libraries

`rattler-build build` emits warnings of the form:

```
⚠ warning Overdepending against libzlib
⚠ warning Overdepending against libexpat
⚠ warning Overdepending against libtiff
⚠ warning Overdepending against tbb
⚠ warning Overdepending against libpng
⚠ warning Overdepending against libjpeg-turbo
⚠ warning Overdepending against hdf5
⚠ warning Overdepending against fftw
```

Per rattler-build's definition, "overdepending" means the package declares a
run-dep that no binary inside the package actually links against
(no `NEEDED` entry in any ELF scan).

**Partial fix applied:** `libitk_install.sh` and `libitk_install.bat` now
include the `Applications` component in the install loop, which was the only
runtime-flavored component name found in the ITK source that the previous
loop missed (covers bundled OpenJPEG and DoubleConversion executables).

**What needs confirming:** whether the ITKIO* module shared libraries
(`libITKIOTIFF-6.0.so`, `libITKIOPNG-6.0.so`, `libITKIOJPEG-6.0.so`,
`libITKIOHDF5-6.0.so`, `libITKIOMeta-6.0.so`, `libITKFFT-6.0.so`) are
present in the packaged `libitk-*.conda` archive and have the expected
`NEEDED` entries. Per `CMake/ITKModuleMacros.cmake:823`, ITK modules
install their shared libraries under the `RuntimeLibraries` component,
which is already in the loop — so either (a) the install is correct and
rattler-build's overdepending check has a false positive, or (b) some
modules aren't being built or captured.

**How to investigate:**
```bash
# After a build that preserved the output:
unzip -l output/<channel>/*/libitk-6.0.0-*.conda | grep ITKIO
# For any that are present, confirm they link against system libs:
cd $(mktemp -d) && unzip /path/to/libitk-6.0.0-*.conda
tar -xf pkg-libitk-*.tar.zst
find lib -name 'libITKIO*.so*' -exec sh -c \
  'echo "=== $1 ==="; readelf -d "$1" | grep NEEDED' _ {} \;
```

Tracked as a separate follow-up; not blocking initial recipe upstreaming.

---

## Suggested ITK CMake Modernizations to Simplify Conda Packaging

These are forward-looking suggestions for reducing the complexity of the
install scripts in `Utilities/conda-packages/`. None are blocking for the initial
recipe upstreaming; they are potential follow-up patches for the ITK
repository itself. Ordered by expected payoff vs. effort.

### 1. Consolidate install component names (highest payoff)

ITK currently uses nine distinct component names across the tree:

```
Runtime, RuntimeLibraries, Libraries, libraries (lowercase!),
Unspecified, Applications, Development, Headers,
PythonWrappingRuntimeLibraries, PythonWheelRuntimeLibraries
```

This is why `libitk_install.sh` is a component loop rather than a one-line
`cmake --install --component Runtime` — bundled third-party CMakeLists
(MINC uses `libraries` lowercase, OpenJPEG uses `Applications`/`Libraries`/
`Headers`) do not agree with ITK core (`RuntimeLibraries`/`Runtime`/
`Development`).

**Proposal:** normalize to three components across the entire tree
(including vendored third-party), matching what conda-forge feedstocks for
VTK and OpenCV use:

| Component | Goes to conda package | Contents |
|---|---|---|
| `Runtime` | `libitk` | shared libs + executables |
| `Development` | `libitk-devel` | headers, static libs, CMake config |
| `PythonWrapping` | `libitk-wrapping` | SWIG `.py`, compiled `_*.so` modules |

With that, each conda install script collapses to:

```bash
cmake --install "${BUILD_DIR}" --component Runtime        # libitk
cmake --install "${BUILD_DIR}" --component Development    # libitk-devel
cmake --install "${BUILD_DIR}" --component PythonWrapping # libitk-wrapping
```

**Implementation notes:** changes to vendored third-party CMakeLists need
to be applied as local patches (the originals may change on re-import).
One approach is to set `CMAKE_INSTALL_DEFAULT_COMPONENT_NAME Runtime`
before `add_subdirectory()` on each bundled dep, overriding bare
`install(COMPONENT ...)` calls. An alternative is to add a CMake include
that post-processes install rules at configure time.

**Trade-offs:** this is a ~weeks-of-review-discussion change touching many
files. High payoff for conda packaging; low payoff for other distribution
methods (CPack DEB/RPM users would need to adapt their spec files).

### 2. Switch to `cmake --install --component X`

**Current (legacy, CMake < 3.15):**
```bash
cmake -DCOMPONENT=RuntimeLibraries -P "${BUILD_DIR}/cmake_install.cmake"
```

**Modern (CMake 3.15+, which ITK 6.0 already requires via its 3.16.3 floor):**
```bash
cmake --install "${BUILD_DIR}" --component RuntimeLibraries
```

Functionally identical, more idiomatic, plays better with `--verbose` and
`--dry-run`. Trivial cleanup; could be applied to
`libitk_install.sh`, `libitk-devel_install.sh`, and `libitk-wrapping_install.sh`
in a single commit. Not ITK-side — this is a `Utilities/conda-packages/` cleanup.

### 3. Make `FILE_SET HEADERS` adoption unconditional

`CMake/ITKModuleMacros.cmake:793-812` conditionally uses
`target_sources(TARGET PUBLIC FILE_SET HEADERS ...)` only for CMake >= 3.23:

```cmake
if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.23")
  ...
endif()
```

Bumping the minimum CMake to 3.23 (released 2022-03) would:
- Remove the conditional fork in `ITKModuleMacros.cmake`
- Let `install(TARGETS … FILE_SET HEADERS …)` replace most per-module
  `install(FILES … DESTINATION … COMPONENT Development)` calls
- Consolidate header install logic into the module macro

**Trade-offs:** platforms still on CMake 3.16.3 (some LTS distros) would
need to install a newer CMake. The Kitware apt repository and pypi
`cmake` package cover this on affected platforms.

### 4. What NOT to adopt for conda-compatible builds

**`install(RUNTIME_DEPENDENCY_SET)` (CMake 3.21+)** — CMake's mechanism
for copying transitive runtime deps into the install tree. Useful for
standalone installers, **counterproductive for conda** because conda wants
its own packaged copies of the deps, not duplicates bundled by the build.

**`install(IMPORTED_RUNTIME_ARTIFACTS)` (CMake 3.21+)** — same category;
duplicates deps conda already tracks.

**`CMAKE_INSTALL_DO_STRIP` / `cmake --install --strip`** — conda-build and
rattler-build already strip binaries post-install. No-op.

### 5. What CMake fundamentally cannot help with

- **Generating `recipe.yaml`** — no CPack Conda generator exists upstream.
  Writing one would duplicate rattler-build's metadata schema, which is
  still evolving. Not worth the maintenance cost.
- **Conda activation / deactivation scripts** (`$PREFIX/etc/conda/activate.d/`).
  A conda concept, not a CMake concept.
- **RPATH rewriting between build-prefix and install-prefix** — handled
  by `patchelf` (Linux), `install_name_tool` (macOS), binary string
  rewrites (Windows) inside conda-build / rattler-build. CMake
  configures the initial RPATH via `CMAKE_INSTALL_RPATH` and
  `CMAKE_BUILD_WITH_INSTALL_RPATH` (already set correctly by ITK).
- **`run_exports`, `pin_subpackage()`, dep version constraints** — pure
  conda recipe metadata; lives in `recipe.yaml`, outside CMake's scope.

### Summary

| Suggestion | Effort | Payoff | Where the change lives |
|---|---|---|---|
| #1 consolidate components | high | high (one-line install scripts) | ITK repo, many files |
| #2 `cmake --install --component X` | trivial | low (cosmetic) | `Utilities/conda-packages/` |
| #3 unconditional `FILE_SET HEADERS` | low | medium | ITK repo, ~1 file |
| #4 avoid `RUNTIME_DEPENDENCY_SET` | n/a | n/a (don't do it) | — |

---

## Checklist for Reviewer

- [ ] All 5 BUG commits touch only CMake infrastructure files (no recipe files)
- [ ] libitk-devel CMake test passes: `cmake -DCMAKE_PREFIX_PATH=$PREFIX ../tests/example`
- [ ] `rattler-build build --render-only` parses all three outputs without error
- [ ] `libitk` package contains `lib/ITK-6.0/*.dylib` (or `.so`/`.dll`), no headers
- [ ] `libitk-devel` package contains `include/ITK-6.0/` and `lib/cmake/ITK-6.0/`
- [ ] `libitk-wrapping` package: `python -c "import itk"` succeeds
- [ ] No `itk-build/` or other local artifacts accidentally staged in any commit
