<!--
  ##################################################################
  ##  AUTO-GENERATED FILE. DO NOT EDIT.                            ##
  ##                                                              ##
  ##  Generated from Documentation/docs/migration_guides/records/itk6/
  ##  by Utilities/Maintenance/MigrationRecords.py generate.      ##
  ##  Edit or add a record instead; see                           ##
  ##  Documentation/AI/migration-records.md.                      ##
  ##################################################################
-->

# ITK 6 Migration Guide

Changes that downstream code may need to act on when moving to ITK 6.
The first part lists the changes that affect many projects and what to do
about each; the second lists every change by release, newest first.
This page is generated from the [migration records](https://github.com/InsightSoftwareConsortium/ITK/tree/main/Documentation/docs/migration_guides/records/itk6);
to correct or add an entry, edit a record.

## Most impactful changes

(itk6-ModernCMakeInterfaceLibraries)=
### Modern CMake Interface Libraries

{bdg-danger}`high` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cmake`

ITK 6 introduces modern CMake interface libraries for all ITK modules, providing improved dependency management and
simplified linking. Each module now exports a namespaced interface library following the pattern `ITK::{ModuleName}Module`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Key Changes**

**Interface Library Naming:**
- All ITK module libraries are now exported with the `ITK::` namespace
- Module interface libraries follow the pattern: `ITK::{ModuleName}Module`
- Example: `ITKCommon` library is accessed as `ITK::ITKCommonModule`

**Variable Behavior Changes:**
- `${ModuleName}_LIBRARIES` now contains **only** libraries produced by that specific module
- Transitive dependencies are automatically handled through the interface library
- Previously, `${ModuleName}_LIBRARIES` included some transitive dependencies

**Use Target-Specific Properties:**
- CMake global and directory scoped properties such and include directories and libraries have been replaced with target-specific properties.
- The interface libraries handle transitive dependencies, include paths, and compiler flags automatically through
CMake's `INTERFACE` properties.
- Eliminates the need for `UseITK.cmake` which modified global CMake state.

Benefits:
- Cleaner, more maintainable CMake code
- Prevents conflicts between different dependency requirements
- Explicit dependency relationships
- Better IDE integration and IntelliSense support

**Application Configuration**

The usage of `${ITK_USE_FILE}` or UseITK.cmake` is now deprecated, and provides the legacy compatibility of setting global properties for include directory, CXX flags etc.

For applications using ITK, use the interface library for proper dependency linking. See the
[Installation Example](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Examples/Installation)
for a complete working example.

**Before:**
```cmake
find_package(ITK REQUIRED COMPONENTS MyModule)
include(${ITK_USE_FILE})
add_executable(Example Example.cxx)
target_link_libraries(Example ${ITK_LIBRARIES})
```

**After:**
```cmake
find_package(ITK REQUIRED COMPONENTS MyModule)
itk_generate_factory_registration()
add_executable(Example Example.cxx)
target_link_libraries(Example ITK::MyModuleModule)
```

Alternatively, the CMake variable `ITK_INTERFACE_LIBRARIES` can be used to link against all loaded modules.

**Factory Registration with Meta-Modules:**

ITK uses factory registration to load IO formats and pluggable components at runtime. ITK 6 uses
meta-modules that simplify factory registration by grouping related modules together.

Factory meta-modules include:
- `ITKImageIO` - All image IO modules (JPEG, PNG, NIFTI, DICOM, etc.)
- `ITKMeshIO` - All mesh IO modules
- `ITKTransformIO` - Transform IO modules
- `ITKFFTImageFilterInit` - FFT implementations

To register all factories:

```cmake
find_package(ITK REQUIRED COMPONENTS ITKCommon ITKImageIO )
itk_generate_factory_registration( )
...
target_link_libraries(MyTarget ${ITK_INTERFACE_LIBRARIES} ITK::ITKImageIO)
```

The `itk_generate_factory_registration()` macro *must* be called **before** adding executables. It optionally takes the factory types to register. The CMake macro generates registration code for the IO modules and factory-enabled components from the modules loaded in `find_package()`. The meta-module *must* be linked to the target to include and enable the generated registration code.

In the above example, because `ITKImageIO` is provided as a required component, all available ImageIO modules are registered. Note that if no components are requested in `find_package()`, then all components are loaded and registered.

To explicitly register factories:

```cmake
find_package(ITK REQUIRED COMPONENTS ITKCommon ITKIOGDCM ITKIONRRD )
itk_generate_factory_registration( ImageIO )
...
target_link_libraries(MyTarget ${ITK_INTERFACE_LIBRARIES} ITK::ITKImageIO)
```

In this example, the GDCM and NRRD ImageIO modules are explicitly loaded. Only the ImageIO factory type is registered and generated.

**Determining Required Modules:**

To identify which ITK modules your code depends on, use the `WhatModulesITK.py` utility:

```bash
python Utilities/Maintenance/WhatModulesITK.py /path/to/ITK/source file1.cxx file2.h
```

This script analyzes your source files and reports the required ITK modules based on the headers included.
The output lists the modules you should specify in `find_package(ITK REQUIRED COMPONENTS ...)`.

Use the `--link` option to generate `target_link_libraries()` commands with the interface library names.
This outputs the namespaced interface libraries (e.g., `ITK::ITKCommonModule`) suitable for direct use in
`target_link_libraries()` commands.

**Migration for ITK Modules**

Remote modules should be updated to use modern CMake patterns for better integration with ITK 6.

**Using itk_module_add_library**

Replace direct `add_library()` calls with `itk_module_add_library()`:

**Before:**
```cmake
set(MyModule_SRCS
  itkClass1.cxx
  itkClass2.cxx
)
add_library(MyModule ${ITK_LIBRARY_BUILD_TYPE} ${MyModule_SRCS})
itk_module_link_dependencies(MyModule)
itk_module_target(MyModule)
```

**After:**
```cmake
set(MyModule_SRCS
  itkClass1.cxx
  itkClass2.cxx
)
itk_module_add_library(MyModule ${MyModule_SRCS})
```

The `itk_module_add_library()` macro automatically:
- Sets the appropriate library type (SHARED/STATIC)
- Configures include directories using generator expressions
- Links dependencies via the interface library
- Sets up export targets

In some cases when the dependencies of the module are not needed for the library, only `itk_module_target()` is needed.

**Using Interface Libraries for Executables**

When linking executables or examples, use the interface library instead of module variables:

**Before:**
```cmake
add_executable(MyExample MyExample.cxx)
target_link_libraries(MyExample ${MyModule_LIBRARIES})
```

**After:**
```cmake
add_executable(MyExample MyExample.cxx)
target_link_libraries(MyExample ITK::MyModuleModule)
```

**Backward Compatibility**

For backward compatibility, non-namespaced aliases are created with deprecation warnings. However, new code should use the namespaced `ITK::` targets exclusively.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/tree/master/Examples/Installation>

[PR #5721](https://github.com/InsightSoftwareConsortium/ITK/pull/5721) · [`93df383cb9b`](https://github.com/InsightSoftwareConsortium/ITK/commit/93df383cb9b)

Record: [`20260205_ModernCMakeInterfaceLibraries.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260205_ModernCMakeInterfaceLibraries.yaml)

:::::

(itk6-NumPyImageFromArrayTransposeNotEquivalent)=
### NumPy bridge: `image_from_array(arr.T)` is no longer transpose-equivalent to `image_from_array(arr)`

{bdg-danger}`high` {bdg-info-line}`ResultsChange` {bdg-primary-line}`python`

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Rationale**

Previously, `itk.image_from_array` (and the underlying
`itk.PyBuffer.GetImageViewFromArray` / `GetImageFromArray`) special-cased
F-contiguous inputs, with the result that `image_from_array(arr)` and
`image_from_array(arr.T)` produced ITK images with **identical** sizes and
content -- the transpose was effectively a no-op through this API. The
docstrings explicitly documented this behavior and pointed users at
`np.reshape` for any actual flipping.

This was buggy in practice: the C++ side performed its own shape reversal
on F-contiguous buffers, so size handling double-reversed and `image.shape`
disagreed with `array.shape[::-1]` for F-contiguous inputs. Round-tripping
through `array_from_image(image_from_array(arr))` could silently transpose
the data.

**What changed in ITKv6**

`GetImageViewFromArray` / `GetImageFromArray` now **deep-copy** any
non-C-contiguous input via `np.ascontiguousarray()` before constructing
the image. The resulting image owns an independent buffer that does
**not** reference the user's original array; mutations to the original
will no longer be visible through the image. The shape rule is now
uniform regardless of memory layout:

```
itk.size(image)            ==  array.shape[::-1]
image.GetPixel((i, j, k))  ==  array[k, j, i]
```

As a consequence, `arr` and `arr.T` produce ITK images with **different
(reversed) sizes** -- `np.transpose` / `.T` is now the correct operation
to flip the dimension order through this API.

**What you need to do**

Audit any code that relied on `image_from_array(arr)` and
`image_from_array(arr.T)` returning the same image. The most common idioms
to inspect:

- Code that called `itk.image_from_array(some_view.T)` expecting the
  transpose to be ignored. Drop the `.T` to recover the prior behavior,
  or accept the new transposed-image semantics if that was the intent.
- Code that round-trips arrays through ITK: previously
  `array_from_image(image_from_array(arr_f))` could differ from `arr_f`
  for F-contiguous inputs; now it equals `np.ascontiguousarray(arr_f)`,
  preserving values exactly with the original `arr_f.shape`.
- A new warning fires when `GetImageViewFromArray` is called on a
  non-C-contiguous input (since a deep-copy is performed). Set
  `need_contiguous=False` to silence it if the deep-copy is intended.
  `GetImageFromArray` already silences the warning internally because
  it always deep-copies via `ImageDuplicator`; the kwarg is not exposed
  on that API.

[PR #5775](https://github.com/InsightSoftwareConsortium/ITK/pull/5775) · [`fdee9cb6d89`](https://github.com/InsightSoftwareConsortium/ITK/commit/fdee9cb6d89)

Record: [`20260613_NumPyImageFromArrayTransposeNotEquivalent.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260613_NumPyImageFromArrayTransposeNotEquivalent.yaml)

:::::

(itk6-RemoveDeprecatedModule)=
### Remove support for ITKv5

{bdg-danger}`high` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

*First released in ITK 6.0a01.*

All contents of the Deprecated module were removed. This includes TreeContainer
and related classes; atomic primitives, mutexes and related classes which are
now part of C++ standard; specialized Vector filters - specialized versions are
no longer needed, as regular filters can work with vector images. For details, see
[ITKv5 Migration Guide](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/itk_5_migration_guide.md).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/itk_5_migration_guide.md>

[PR #4729](https://github.com/InsightSoftwareConsortium/ITK/pull/4729) · [`91251de596f`](https://github.com/InsightSoftwareConsortium/ITK/commit/91251de596f)

Record: [`20240620_RemoveDeprecatedModule.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20240620_RemoveDeprecatedModule.yaml)

:::::

## All changes

| Impact | Change | Release |
| --- | --- | --- |
| high | [Modern CMake Interface Libraries](#itk6-ModernCMakeInterfaceLibraries) | unreleased |
| high | [NumPy bridge: `image_from_array(arr.T)` is no longer transpose-equivalent to `image_from_array(arr)`](#itk6-NumPyImageFromArrayTransposeNotEquivalent) | unreleased |
| medium | [`GetIndex()` replaced with `ComputeIndex()`, for iterators without index](#itk6-ComputeIndexReplacesGetIndex) | unreleased |
| medium | [IO modules write floating-point metadata with lossless precision](#itk6-LosslessFloatMetadata) | unreleased |
| medium | [Minimum supported VTK version: 9.1](#itk6-MinimumVtk91) | unreleased |
| medium | [Vnl FFT backend replaced by PocketFFT](#itk6-VnlFFTReplacedByPocketFFT) | unreleased |
| medium | [`itkDCMTKFileReader.h` is now a private header of `ITKIODCMTK`](#itk6-DCMTKFileReaderPrivateHeader) | unreleased |
| medium | [`GDCMSeriesFileNames` reimplemented on `gdcm::Scanner`/`IPPSorter`](#itk6-GDCMSeriesFileNamesOnScanner) | unreleased |
| medium | [`JointHistogramMutualInformationImageToImageMetricv4` corrected](#itk6-JointHistogramMutualInformationMetricCorrected) | unreleased |
| medium | [`HDF5ImageIO` no longer compresses by default](#itk6-HDF5ImageIONoDefaultCompression) | unreleased |
| medium | [Projection and accumulate filters: collapsed-axis output origin centered](#itk6-ProjectionCollapsedAxisOriginCentered) | unreleased |
| medium | [Minimum wrapped Python raised to 3.11](#itk6-MinimumWrappedPython311) | unreleased |
| medium | [`NiftiImageIO` preserves NaN and infinite pixel values](#itk6-NiftiImageIOPreservesNonFinite) | unreleased |
| medium | [Python spatial key migration](#itk6-PythonSpatialKeysOrderExplicit) | unreleased |
| medium | [Eigen-backed eigendecompositions replace netlib EISPACK `vnl_*_eigensystem`](#itk6-EigenEigendecompositionsReplaceEispack) | unreleased |
| medium | [FFTW](#itk6-FftwThroughItkTargets) | unreleased |
| medium | [`GradientStructureTensorImageFilter`: feature-scale smoothing is now isotropic](#itk6-GradientStructureTensorIsotropicSmoothing) | unreleased |
| medium | [`MINCImageIO` reads a MINC time dimension as a fourth image axis](#itk6-MINCTimeDimensionFourthAxis) | unreleased |
| medium | [`MultiThreaderBase` global default work-unit-per-thread ratio](#itk6-WorkUnitPerThreadRatio) | unreleased |
| low | [Remove support for Python wrapped `long double` types](#itk6-PythonLongDoubleWrappingRemoved) | unreleased |
| low | [Legacy GoogleTest Target Names Removed](#itk6-GoogleTestLegacyTargetsRemoved) | unreleased |
| low | [Python lazy loading is always on](#itk6-PythonLazyLoadingAlwaysOn) | unreleased |
| low | [`itk::Math::sqrteps` changed by 1 ULP for accuracy](#itk6-MathSqrtepsOneUlp) | unreleased |
| low | [`FixedPointInverseDisplacementFieldImageFilter::OutputIterator` is now `ImageRegionIteratorWithIndex`](#itk6-FixedPointInverseDisplacementFieldOutputIterator) | unreleased |
| low | [ITKFEM and ITKFEMRegistration modules removed](#itk6-ItkFemModulesRemoved) | unreleased |
| low | [Deprecate Is Number](#itk6-DeprecateIsNumber) | unreleased |
| low | [`RenyiEntropyThresholdCalculator`/`MaximumEntropyThresholdCalculator`: max-entropy seed corrected](#itk6-EntropyThresholdMaxEntropySeedCorrected) | unreleased |
| low | [Optional Eigen-backed `itk::bridge::Math::SVD`](#itk6-BridgeMathSVD) | unreleased |
| low | [The `itk::bridge` namespace: a migration aid, not supported ITK API](#itk6-BridgeNamespaceMigrationAid) | unreleased |
| low | [`GetInverseTransform()` returns the concrete transform type for more 3D transforms](#itk6-GetInverseTransformConcreteType3D) | unreleased |
| low | [Indent Time Stamp New Delete Deprecated](#itk6-IndentTimeStampNewDeleteDeprecated) | unreleased |
| low | [`GradientImageFilter::OverrideBoundaryCondition` deprecated in favor of `SetBoundaryCondition`](#itk6-GradientImageFilterSetBoundaryCondition) | unreleased |
| low | [Python Global Interpreter Lock (GIL) Release](#itk6-PythonGilRelease) | unreleased |
| low | [`ITK_AUTOLOAD_PATH` is read once; use `ObjectFactoryBase::SetAutoloadPaths`](#itk6-AutoloadPathReadOnce) | unreleased |
| low | [Anisotropic Diffusion LBRGradient Structure Tensor Rename](#itk6-AnisotropicDiffusionLBRGradientStructureTensorRename) | unreleased |
| low | [IsotropicWavelets `StructureTensorImageFilter` is now `RieszStructureTensorImageFilter`](#itk6-RieszStructureTensorImageFilterRename) | unreleased |
| low | [Byte-order macros are deprecated in `itkConfigure.h`](#itk6-ByteOrderMacrosDeprecated) | unreleased |
| low | [`ITK_HAS_CPP11_TYPETRAITS` is removed](#itk6-HasCpp11TypeTraitsRemoved) | unreleased |
| low | [ITK headers present one form in every C++ standard](#itk6-HeadersOneFormEveryCxxStandard) | unreleased |
| low | [`ITK_HAVE_FENV_H` is removed](#itk6-ItkHaveFenvHRemoved) | unreleased |
| low | [Minimum Visual Studio raised to 2019 16.2](#itk6-MinimumMsvc1922) | unreleased |
| low | [32-bit x86 requires SSE2 floating-point math](#itk6-RequireSse2On32BitX86) | unreleased |
| medium | [Prefer itk::AnatomicalOrientation over itk::SpatialOrientation](#itk6-PreferAnatomicalOrientation) | 6.0b01 |
| low | [ITKVNLInstantiation library is removed](#itk6-ItkVnlInstantiationRemoved) | 6.0b01 |
| medium | [Coord Rep Type To Coordinate Type](#itk6-CoordRepTypeToCoordinateType) | 6.0a02 |
| high | [Remove support for ITKv5](#itk6-RemoveDeprecatedModule) | 6.0a01 |
| medium | [Prefer standard CXX language features rather than ITK macros](#itk6-PreferStandardCxxOverItkMacros) | 6.0a01 |
| medium | [Point Set Clone Deep Copy](#itk6-PointSetCloneDeepCopy) | 6.0a01 |
| low | [Remove support for ITKv4 interfaces](#itk6-RemoveItkV4Compatibility) | 6.0a01 |
| low | [Require modern C++ language feature use](#itk6-RemoveItkV5ConstMacro) | 6.0a01 |
| low | [Removed ITKv5 migration/maintenance scripts](#itk6-RemoveItkV5MigrationScripts) | 6.0a01 |

### Unreleased (on `main`)

High impact, described under Most impactful changes:

- [Modern CMake Interface Libraries](#itk6-ModernCMakeInterfaceLibraries)
- [NumPy bridge: `image_from_array(arr.T)` is no longer transpose-equivalent to `image_from_array(arr)`](#itk6-NumPyImageFromArrayTransposeNotEquivalent)

(itk6-ComputeIndexReplacesGetIndex)=
#### `GetIndex()` replaced with `ComputeIndex()`, for iterators without index

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

`ImageConstIterator::GetIndex()` is marked to be removed in the future. This iterator does not have an index,
internally, so its `GetIndex()` member function does a potentially expensive computation. For an iterator that is
derived from `ImageConstIterator`, please use its newly introduced `ComputeIndex()` member function instead. Or use an
iterator _with_ index (like `ImageIteratorWithIndex`).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #5803](https://github.com/InsightSoftwareConsortium/ITK/pull/5803) · [`2b7c0770f96`](https://github.com/InsightSoftwareConsortium/ITK/commit/2b7c0770f96)

Record: [`20260215_ComputeIndexReplacesGetIndex.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260215_ComputeIndexReplacesGetIndex.yaml)

:::::

(itk6-LosslessFloatMetadata)=
#### IO modules write floating-point metadata with lossless precision

{bdg-warning}`medium` {bdg-info-line}`FileFormat` {bdg-info-line}`ResultsChange` {bdg-primary-line}`data`

`float` and `double` metadata values are now serialized using
`itk::ConvertNumberToString()`, which produces the shortest decimal string
that round-trips exactly — replacing the previous 6-significant-digit default.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Affected modules: **NIfTI** (`scl_slope`, `scl_inter`, `pixdim`, spacing,
and offset fields), **MetaImage**, **NRRD**, **VoxBoCUB**, **SpatialObject**,
and **Mesh IO**.

Tests that compare raw header text against golden baselines may fail because
values previously written as e.g. `"1"` are now written as `"1.0000001"`.
Replace exact string matches with numeric comparisons or regenerate baselines.
Files not written by ITK are unaffected.

[PR #6003](https://github.com/InsightSoftwareConsortium/ITK/pull/6003) · [`cd9ec887ed0`](https://github.com/InsightSoftwareConsortium/ITK/commit/cd9ec887ed0)

Record: [`20260408_LosslessFloatMetadata.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260408_LosslessFloatMetadata.yaml)

:::::

(itk6-MinimumVtk91)=
#### Minimum supported VTK version: 9.1

{bdg-warning}`medium` {bdg-info-line}`Dependency` {bdg-primary-line}`cmake`

:::::{dropdown} Details and examples
:class-container: sd-mb-4

`ITKVtkGlue` (and any code that links it) now requires **VTK 9.1 or
newer**. The previous floor of VTK 8.1 has been raised because:

* VTK 9.0 was the first stable release of the new module system, which
  replaces the legacy `vtk_module_config(NS …)` macro with imported
  targets such as `VTK::CommonCore` and `VTK::IOImage`.
* VTK 9.0 removed the `OpenGL` rendering backend; only `OpenGL2` is
  shipped.
* VTK 9.1 stabilised the imported-target names and the
  `vtk_module_autoinit` helper that ITKVtkGlue now relies on
  unconditionally, removing a significant amount of legacy
  compatibility code from `Modules/Bridge/VtkGlue/`.

**VTK 9.4 or newer is recommended** for current security/CVE patches
and bug fixes, but **not required** — every imported-target name and
CMake helper that ITKVtkGlue depends on was already stable in 9.1, so
ITK 6 is happy to build against any 9.1.x release. Use 9.4+ if you
have the choice.

**What you need to do**

If your project links `ITKVtkGlue` (directly or via `find_package(ITK)`):

1. **Build / link against VTK 9.1 or newer.** Distribution packages
   that ship a supported VTK as of this release: Ubuntu 22.04 LTS and
   24.04 LTS (`libvtk9.1`), Fedora 38+ (9.2+), Homebrew (`brew install
   vtk` tracks current — currently 9.5). Anaconda's `conda-forge::vtk`
   is at 9.4+. 3D Slicer and ParaView build VTK 9.4+ from source as
   part of their superbuild.

2. **Remove any local `vtk_module_config(...)` calls in your CMake.**
   The shim that ITKVtkGlue used to inject when consuming legacy VTK
   has been deleted. Replace such calls with a direct
   `target_link_libraries(<your-target> PUBLIC VTK::IOImage VTK::…)`,
   then call `vtk_module_autoinit` so VTK's object factories and I/O
   readers/writers register at startup. Without this call, factory-
   dispatched features (e.g., `vtkImageReader2Factory`) silently
   produce empty results:
   ```cmake
   target_link_libraries(<your-target>
     PUBLIC
       VTK::IOImage
       VTK::ImagingSources
       # …other VTK modules you use
   )
   vtk_module_autoinit(
     TARGETS <your-target>
     MODULES VTK::IOImage VTK::ImagingSources # …same list as above
   )
   ```

3. **Remove any `VTK_RENDERING_BACKEND` selection logic that fell back
   to `OpenGL`.** Only `OpenGL2` is supported now. Code such as
   ```cmake
   if(VTK_RENDERING_BACKEND STREQUAL "OpenGL")
     # ...
   endif()
   ```
   can be deleted; the special cases for `OpenGL` no longer exist
   upstream.

4. **Update `find_package(VTK …)` calls** to require 9.1 explicitly:
   ```cmake
   find_package(VTK 9.1 NO_MODULE REQUIRED)
   ```
   If a project also exports its own `<Project>Config.cmake`, mirror
   the version pin so downstream consumers get a clear failure rather
   than a confusing missing-target error. Bump to `9.4` instead of
   `9.1` if you want to enforce the recommended-not-required floor in
   your own build.

**Note for downstream projects that pin both ITK and VTK**

ITK 6 + VTK 9.1+ is the supported combination, with VTK 9.4+
recommended. If you maintain a SuperBuild that pins both
(e.g., 3D Slicer, BRAINSTools, ANTs, ITK Remote Modules), bump the
pinned VTK tag to **at least `v9.1.0`** (or, preferably, the latest
9.4.x) before bumping the ITK pin to 6. Building ITK 6 against VTK
8.x or VTK 9.0 will fail at `find_package` time with a clear
version-mismatch error.

Distributions still on VTK 8.x or VTK 9.0 should track ITK 5.4.x
until their VTK is updated; ITK 5.4 patch releases continue to
support the wider VTK 8.1+/9.0+ range.

[PR #6144](https://github.com/InsightSoftwareConsortium/ITK/pull/6144) · [`73e469fc432`](https://github.com/InsightSoftwareConsortium/ITK/commit/73e469fc432)

Record: [`20260427_MinimumVtk91.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260427_MinimumVtk91.yaml)

:::::

(itk6-VnlFFTReplacedByPocketFFT)=
#### Vnl FFT backend replaced by PocketFFT

{bdg-warning}`medium` {bdg-info-line}`ApiRemoved` {bdg-info-line}`ApiRenamed` {bdg-info-line}`ResultsChange` {bdg-info-line}`Performance` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake` {bdg-primary-line}`data`

The `VnlFFT*` image filter family, `vnl_fft`/`vnl_convolve` in the vendored
vxl, and the Temperton GPFA sources (`v3p/netlib/temperton/`) have been
removed. The default (non-FFTW) FFT backend is now PocketFFT
(`PocketFFT*ImageFilter`), which supports images of **any** size — the
2^a·3^b·5^c size restriction is gone.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

1. Replace `Vnl<X>FFTImageFilter` with `PocketFFT<X>FFTImageFilter` (Forward, Inverse, RealToHalfHermitian, HalfHermitianToReal, ComplexToComplex, and the 1D variants). Factory-based instantiation (`ForwardFFTImageFilter::New()` etc.) requires no change.
1. `VnlFFTImageFilterInitFactory` is now `PocketFFTImageFilterInitFactory`; the CMake backend key changed from `FFTImageFilterInit::Vnl` to `FFTImageFilterInit::Pocket`.
1. Code calling `vnl_fft_1d`/`vnl_fft_2d`/`vnl_convolve` directly must move to another FFT (e.g. `pocketfft_hdronly.h` via `itkPocketFFTCommon.h`, as `itkN4BiasFieldCorrectionImageFilter.hxx` now does).
1. `vnl_fft_1d` is retained as a deprecated PocketFFT-backed shim, but it now pulls in `pocketfft_hdronly.h`: any consumer that includes `vnl/algo/vnl_fft_1d.h` must depend on the ITKFFT module.

**Concerns**

- Numerical results differ from the Vnl backend at the ULP level;
  bit-exact baselines must move to tolerance-based comparison.
- One-platform precision concession (`aarch64`-Linux only): for a `float`
  image, PocketFFT's half-Hermitian (r2c/c2r) reconstruction and its
  full-complex reconstruction of the same transform agree to ~1e-4 rather
  than ~1e-5 — a handful of pixels (1–13 of thousands) cross 1e-5 from the
  aarch64-Linux libm/FMA rounding of the two distinct algorithms. The same
  builds on x86-64 Linux and on aarch64 macOS agree exactly (0 ULP), and the
  result is bit-identical regardless of thread count (PocketFFT parallelizes
  independent 1-D lines), so this is neither a multithreading effect nor a
  layout error — the reconstruction is correct (round-trip ~1e-14). Only
  `FrequencyIterators.{Even2D,Even3D,Odd3D}` are affected; they use a 1e-4
  Hermitian tolerance, matching the pre-existing `Odd2D` test.
- PocketFFT real transforms use native r2c/c2r (half-complex), not the
  full-complex emulation Vnl used — faster and less memory, same results.
- The deletions under `Modules/ThirdParty/VNL/src/vxl/` (temperton/,
  vnl_fft*, vnl_convolve*) diverge from upstream vxl and must be re-applied
  or upstreamed on the next vnl pin update.
- `vnl_fft_prime_factors`, `gpfa`-family symbols, and the vxl
  `vnl_algo_test_fft*`/`test_convolve` tests are gone from the vendored tree.

[PR #6441](https://github.com/InsightSoftwareConsortium/ITK/pull/6441) · [`d355ad35b57`](https://github.com/InsightSoftwareConsortium/ITK/commit/d355ad35b57)

Record: [`20260619_VnlFFTReplacedByPocketFFT.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260619_VnlFFTReplacedByPocketFFT.yaml)

:::::

(itk6-DCMTKFileReaderPrivateHeader)=
#### `itkDCMTKFileReader.h` is now a private header of `ITKIODCMTK`

{bdg-warning}`medium` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

`itkDCMTKFileReader.h` and `itkDCMTKFileReader.cxx` moved from the module's
installed `include/` directory to `src/`, making `ITKDCMTK` a private dependency
of `ITKIODCMTK`. The reader (`itk::DCMTKFileReader`, `itk::DCMTKSequence`,
`itk::DCMTKItem`) is no longer installed and is not part of the public ITK API.
Downstream code that did `#include "itkDCMTKFileReader.h"` no longer resolves.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Because the header is now private, its contents may change without notice or
API-stability guarantees. Do not depend on it as an installed header. Instead,
vendor a private copy of both files into your own project from the last commit
where they were the public interface:

```
ITK commit db492d9b092a6ca585ab6b12e896ca63229ad8e8
  Modules/IO/DCMTK/src/itkDCMTKFileReader.h
  Modules/IO/DCMTK/src/itkDCMTKFileReader.cxx
```

Pin to that exact hash rather than tracking `main`; the file lives outside the
public interface now and may diverge substantially in future ITK versions.
Compile the vendored `.cxx` into your target and neutralize the
`ITKIODCMTK_EXPORT` macro (define it empty) since the reader is no longer
exported from a shared library. This is the approach taken by the Slicer
`SUVFactorCalculatorCLI` and `PETStandardUptakeValueComputation` CLIs and by
BRAINSTools `DWIConvert`.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/commit/db492d9b092a6ca585ab6b12e896ca63229ad8e8>

[PR #6553](https://github.com/InsightSoftwareConsortium/ITK/pull/6553) · [`5c9987a5844`](https://github.com/InsightSoftwareConsortium/ITK/commit/5c9987a5844)

Record: [`20260705_DCMTKFileReaderPrivateHeader.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260705_DCMTKFileReaderPrivateHeader.yaml)

:::::

(itk6-GDCMSeriesFileNamesOnScanner)=
#### `GDCMSeriesFileNames` reimplemented on `gdcm::Scanner`/`IPPSorter`

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-info-line}`DefaultChanged` {bdg-info-line}`FileFormat` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

`itk::GDCMSeriesFileNames` no longer uses the GDCM-deprecated
`gdcm::SerieHelper`. It now enumerates with `gdcm::Directory`, groups series
with `gdcm::Scanner` (replicating `SerieHelper::CreateUniqueSeriesIdentifier`),
and orders slices geometrically with `gdcm::IPPSorter`. The public API is
unchanged; consumers compile without modification.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

1. Nothing for the common case: single-series directories enumerate, group, and order as before.
1. If you relied on file ordering for **duplicate-`ImagePositionPatient`** or **gantry-tilted** acquisitions, re-check it. `gdcm::IPPSorter` is strict and fails to sort those; on failure an exception is thrown by default. Call `SetFailOnAmbiguousOrdering(false)` to instead accept the legacy `SerieHelper` heuristics (Instance Number when unique, else lexicographic filename order) — a non-standards-conforming fallback retained only for determinism and backward compatibility; do not trust its output. First-class support is tracked in [#6468](https://github.com/InsightSoftwareConsortium/ITK/issues/6468).
1. Tags passed to `AddSeriesRestriction` now refine the series identifier (sub-dividing a `SeriesInstanceUID`, the documented intent, e.g. `"0008|0021"`) instead of `SerieHelper`'s largely-inert file-restriction list. Malformed tags are warned-and-ignored rather than honored.
1. `SetLoadSequences`/`SetLoadPrivateTags` have no effect with the `gdcm::Scanner` backend (the scan reads only the grouping/ordering tags). They are retained for source compatibility but no longer alter enumeration.
1. Non-image DICOM objects (structured reports, RTSTRUCT, DICOMDIR, presentation states — anything without Rows `(0028,0010)`) are excluded from enumeration, matching `SerieHelper`'s `ImageReader`-based acceptance.
1. `GetUseSeriesDetails()` now defaults to `false`, consistent with the documented opt-in ("you may want to try calling `SetUseSeriesDetails(true)`") and with DICOM series identity: by default series are grouped by their raw `SeriesInstanceUID` `(0020,000e)`, and `GetFileNames(<raw UID>)` matches. Grouping behavior is unchanged — the previous backend never applied the detail tags until `SetUseSeriesDetails(true)` was called, even though the getter misreported `true`. Only code that read `GetUseSeriesDetails()` before calling the setter sees a different (now truthful) value.

**Concerns**

- Repeated `GetSeriesUIDs`/`GetFileNames`/`GetInputFileNames` calls no longer
  re-scan the directory (lazy parse cached by a `TimeStamp`); a directory
  mutated on disk between calls without an intervening `Modified()` is not
  re-read.
- The vendored `gdcm::SerieHelper` is untouched (GDCM still uses it
  internally); it may be removed once upstream GDCM drops it.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/issues/6468>

[PR #6469](https://github.com/InsightSoftwareConsortium/ITK/pull/6469) · [`50dfb136658`](https://github.com/InsightSoftwareConsortium/ITK/commit/50dfb136658)

Record: [`20260715_GDCMSeriesFileNamesOnScanner.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260715_GDCMSeriesFileNamesOnScanner.yaml)

:::::

(itk6-JointHistogramMutualInformationMetricCorrected)=
#### `JointHistogramMutualInformationImageToImageMetricv4` corrected

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

The JointHistogram mutual-information metric's marginal PDFs were swapped
(since introduction) and its analytic derivative was not an MI gradient; both
are fixed, and the derivative is now unit-consistent with a bounded per-sample
weight. The public API is unchanged and code compiles unmodified, but
registration values and convergence with this metric change (toward the true
optimum), and hand-tuned fixed learning rates must be re-tuned. Details, the
downstream-consumer impact survey (SimpleITK, BRAINSFit, ANTs), and the
re-baselining checklist are in the companion guide:
JointHistogramMutualInformation metric correction.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

This report documents a behavior change in
`itk::JointHistogramMutualInformationImageToImageMetricv4` introduced by
[PR #6569](https://github.com/InsightSoftwareConsortium/ITK/pull/6569), and
the downstream consumers whose numerical output — and stored regression
baselines — change as a result.

It is a companion to the ITK v6 Migration Guide:
these are *correctness* fixes, but registration results computed with the
affected metric change (they move toward the true optimum), and any test that
pins the old numbers must be re-baselined. One protected API is removed:
`JointHistogramMutualInformationGetValueAndDerivativeThreader::ComputeFixedImageMarginalPDFDerivative`
(and its per-thread `FixedImageMarginalPDFInterpolator`) had no remaining
callers after the marginal-pairing fix; subclasses that called it should use
`ComputeMovingImageMarginalPDFDerivative` against the correct marginal.

**Summary**

Three correctness defects are corrected (the related
`ScaleLogarithmicTransform` Jacobian fix was split out to
[PR #6572](https://github.com/InsightSoftwareConsortium/ITK/pull/6572)):

1. **`JointHistogramMutualInformation` marginal PDFs were swapped.**
   The joint PDF is indexed `(fixed bin, moving bin)`. Summing over the fixed
   axis yields the *moving* marginal, but that result was stored into
   `m_FixedImageMarginalPDF` (and vice-versa). `GetValue()` then divided
   `p(a,b)` by `p_moving(a) * p_fixed(b)`. The value is therefore wrong for any
   image pair whose two intensity marginals differ; it is (nearly) invisible
   when the two marginals are the same, e.g. registering an image to a shifted
   or rotated copy of itself.

2. **The metric derivative was not an MI gradient.**
   The per-sample weight is now the standard Parzen-window (Viola & Wells)
   form `(dJPDF/J - dMmPDF/Pm) / log(2)`, replacing a weight that mixed a
   log-ratio-weighted joint term with a wrong-signed marginal term. Because the
   derivative reads the *moving* marginal, fix (2) depends on fix (1): the two
   must land together. On the ITK registration test the corrected derivative
   converges to within &lt;1% of the true shift versus ~35% for the old formula,
   and its direction agrees with the independently-correct Mattes metric
   (cosine 0.95–0.99).

3. **The derivative was not unit-consistent and could spike on sparse bins.**
   `ComputeJointPDFPoint()` maps intensities to a `[0,1]` bin axis, so the
   per-sample gradient needs the chain-rule factor
   `d(bin)/d(intensity) = 1/(TrueMax - TrueMin)`. With it, `GetDerivative()`
   matches a central finite difference of `GetValue()` and no longer scales
   with the moving image's intensity range (previously an 8-bit vs 16-bit
   encoding of the same data produced a 256x different derivative). Each
   log-slope term is additionally clamped to `|ln(eps)|/binwidth` — the
   steepest slope the `eps`-guarded PDF can represent across one bin — which
   never engages on well-posed data and only bounds the worst-case per-sample
   weight.

Fixes (1) and (2) are coupled and change the JointHistogram MI metric's value
**and** gradient; fix (3) further rescales the gradient.

**Derivative magnitude and learning rates**

The corrected, unit-consistent derivative differs in magnitude from the old
formula — typically by orders of magnitude, depending on the moving image's
intensity range.

- Pipelines using a **scales estimator** (e.g.
  `RegistrationParameterScalesFromPhysicalShift`, the
  `ImageRegistrationMethodv4` default, or SimpleITK's
  `SetOptimizerScalesFromPhysicalShift()` +
  `EstimateLearningRate`) absorb the rescale automatically — ITK's own
  registration tests converge to results within existing tolerances on all
  platforms except one float configuration.
- Pipelines with a **hand-tuned fixed learning rate** and no scales estimator
  must re-tune it: the old rate was calibrated against a derivative that was
  neither a gradient nor unit-consistent. Prefer switching to a scales
  estimator over re-tuning a constant.
- The derivative remains the gradient of the per-sample average estimator; it
  can differ from a finite difference of the plug-in histogram sum reported by
  `GetValue()` by the Parzen-smoothing mismatch (measured 0.68x–1.03x across
  histogram configurations). Direction is exact.

**Who is affected**

Only workflows that **explicitly select the JointHistogram MI metric** change.
The Mattes MI metric (`itk::MattesMutualInformationImageToImageMetricv4`),
correlation, mean-squares, and demons metrics are untouched. No public
signature changes, so all downstream code compiles unchanged.

**ITK**

| Test | Nature | Status |
|------|--------|--------|
| `itkSimpleImageRegistrationTestFloat` / `Double` | baseline-image compare | **Baselines regenerated in PR [#6569](https://github.com/InsightSoftwareConsortium/ITK/pull/6569)** (primary double, `.1` float, `.2` x86_64 float). The pre-fix `.3` variant is intentionally dropped: it encoded a platform output of the incorrect metric that no corrected build reproduces; the three regenerated variants cover the CI matrix. |
| `itkJointHistogramMutualInformationImageToImageMetricv4Test` | value/derivative consistency + finite-difference gradient check (hardened in PR [#6569](https://github.com/InsightSoftwareConsortium/ITK/pull/6569); failures now fail the test) | Passes. |
| `itkImageToImageMetricv4RegistrationTest`, `…Test2`, `itkSimpleImageRegistrationTest2/3`, `…WithMaskAndSampling`, `itkQuasiNewtonOptimizerv4RegistrationTest`, `itkMultiStartImageToImageMetricv4RegistrationTest` | convergence / no-exception | Pass (convergence improves). |

**SimpleITK**

`SetMetricAsJointHistogramMutualInformation()` maps directly to the affected
class.

- `Testing/Unit/sitkImageRegistrationMethodTests.cxx` pins a metric value:
  `EXPECT_NEAR(-0.52624100016564002, MetricEvaluate, 2e-6)` for
  `SetMetricAsJointHistogramMutualInformation(20, 1.5)`. The pair is asymmetric
  (its JHMI value differs from its Mattes value), so the marginal fix moves the
  value past the `2e-6` tolerance. **The expected constant must be updated.**
- The `ImageRegistrationMethod2`, `ImageRegistrationMethodBSpline3`, and
  `ImageRegistrationMethodDisplacement1` examples (C++/Python/Java/C#/R) are the
  canonical JHMI examples; their final metric/transform shifts.
- Requires a follow-up SimpleITK change after this lands in ITK.

**BRAINSTools (BRAINSFit `--costMetric MIH`)**

Three baseline-comparing tests in `BRAINSFit/TestSuite/CMakeLists.txt`:

- `BRAINSFitTest_MIHAffineRotationMasks`
- `BRAINSFitTest_MIHScaleSkewVersorRotationMasks`
- `BRAINSFitTest_MIHMetricBrainToItself`

These are mono-modal (brain to a rotated brain / to itself), so the two
marginals are nearly identical and fix (2) barely moves them; fix (3) may still
shift the converged transform. Run them against a before/after ITK to see
whether they breach tolerance. The BRAINSFit default `MMI` (Mattes) and the
`NMIH` metric (which uses the v3
`itk::NormalizedMutualInformationHistogramImageToImageMetric`, a different
class) are **not** affected.

**ANTs (`antsRegistration --metric MI[...]`)**

In `itkantsRegistrationHelper.hxx` the `MI` metric enum builds the affected
JointHistogram class (the separate `Mattes` metric does not). However:

- ANTs' own CTest regression suite exercises only the **legacy `ANTS`
  binary** (`ANTS_MI_1/2_test.cmake`, `COMMAND ANTS … -m MI[…] -r Gauss`),
  which uses ANTs' *internal* MI implementation, not the ITK class — **not
  affected**. No `antsRegistration`/`antsAI`/`MeasureImageSimilarity`
  exact-baseline test was found.
- The user-facing impact is in the pipeline scripts that call
  `antsRegistration MI` — `antsRegistrationSyNQuick.sh`,
  `antsBrainExtraction.sh`, `antsCorticalThickness.sh`,
  `antsMultivariateTemplateConstruction2.sh`, `antsJointLabelFusion2.sh` — whose
  outputs change (converging closer to the true alignment). These are not
  pinned baselines, so no CI turns red, but results drift for users.

**Not affected**

- **elastix / ITKElastix** — no use of the ITK JointHistogram metric (their MI
  is `AdvancedMattesMutualInformation`).
- Non-registration toolkits (teem, VTK, DCMTK, …) — no metric consumers.
- `ScaleLogarithmicTransform` — its Jacobian fix moved to
  [PR #6572](https://github.com/InsightSoftwareConsortium/ITK/pull/6572); no
  forest consumer optimizes it (BRAINSTools references it only for
  transform-file I/O).

**Re-baselining checklist**

| Project | Item | Action |
|---------|------|--------|
| ITK | `itkSimpleImageRegistrationTest{Float,Double}` baselines | Done in PR [#6569](https://github.com/InsightSoftwareConsortium/ITK/pull/6569) (`.nii` content links regenerated, blobs in ITKTestingData). |
| SimpleITK | `sitkImageRegistrationMethodTests.cxx` JHMI constant | Recompute `MetricEvaluate` and update the expected value. |
| SimpleITK | `ImageRegistrationMethod2` / `BSpline3` / `Displacement1` example outputs | Refresh any pinned example results. |
| BRAINSTools | `BRAINSFitTest_MIH{AffineRotationMasks,ScaleSkewVersorRotationMasks,MetricBrainToItself}` | Run before/after; regenerate `*.result.nii.gz` if tolerance is breached. |
| ANTs | pipeline-script outputs (no pinned baselines) | Optional smoke comparison to quantify the (beneficial) drift. |

**Verification notes**

The correction moves results **toward** the true optimum, not away — the
corrected derivative agrees in direction with ITK's independently-correct
Mattes metric, and on the tested pairs the metric value's argmin is essentially
unchanged (the marginal swap offsets the value by a term that varies only
weakly with alignment). Downstream
baselines that shift are encoding the *previous, incorrect* behavior and should
be regenerated rather than treated as regressions.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/6572>

[PR #6569](https://github.com/InsightSoftwareConsortium/ITK/pull/6569) · [`bef284f41cd`](https://github.com/InsightSoftwareConsortium/ITK/commit/bef284f41cd)

Record: [`20260715_JointHistogramMutualInformationMetricCorrected.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260715_JointHistogramMutualInformationMetricCorrected.yaml)

:::::

(itk6-HDF5ImageIONoDefaultCompression)=
#### `HDF5ImageIO` no longer compresses by default

{bdg-warning}`medium` {bdg-info-line}`DefaultChanged` {bdg-info-line}`FileFormat` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

`HDF5ImageIO` previously applied deflate compression unconditionally,
ignoring the `UseCompression` flag. The flag is now respected.
`ImageFileWriter` defaults `UseCompression` to `false`, so HDF5 files
written through the standard writer pipeline are **uncompressed by default**.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Call `writer->UseCompressionOn()` to restore compressed output. To control the
deflate level (1–9, default 5), also call `writer->SetCompressionLevel(N)`.

[PR #6649](https://github.com/InsightSoftwareConsortium/ITK/pull/6649) · [`fb2117cb6c8`](https://github.com/InsightSoftwareConsortium/ITK/commit/fb2117cb6c8)

Record: [`20260716_HDF5ImageIONoDefaultCompression.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260716_HDF5ImageIONoDefaultCompression.yaml)

:::::

(itk6-ProjectionCollapsedAxisOriginCentered)=
#### Projection and accumulate filters: collapsed-axis output origin centered

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

`ProjectionImageFilter` (and its `Maximum`/`Mean`/`Minimum`/`Sum`/`Median`/
`StandardDeviation`/`Binary` subclasses), `AccumulateImageFilter`, and
`GetAverageSliceImageFilter` place the single output pixel of the collapsed
axis at the physical center of the collapsed input extent.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The previous
`inOrigin[i] + (i - 1) * inSpacing[i] / 2` expression did not depend on the
collapsed axis and produced an incorrect origin. For a same-dimension
projection the output origin now shifts by `(size - 1) / 2 * spacing` along
the collapsed axis, transformed through the input direction cosines.
`AccumulateImageFilter` now also throws on an out-of-range
`AccumulateDimension`, matching `ProjectionImageFilter`'s long-standing
behavior.

**What you need to do**

Nothing in your code needs to change. If you read the physical origin of a
projected or accumulated output, or compare it against a stored `.mha`/`.nrrd`
baseline that encodes origin, re-verify that value -- it now reflects the
corrected collapsed-axis center. Formats that do not store an origin (`.tif`,
`.png`) and dimension-reducing projections (output dimension less than input)
are unaffected.

[PR #6594](https://github.com/InsightSoftwareConsortium/ITK/pull/6594) · [`993f3108622`](https://github.com/InsightSoftwareConsortium/ITK/commit/993f3108622)

Record: [`20260716_ProjectionCollapsedAxisOriginCentered.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260716_ProjectionCollapsedAxisOriginCentered.yaml)

:::::

(itk6-MinimumWrappedPython311)=
#### Minimum wrapped Python raised to 3.11

{bdg-warning}`medium` {bdg-info-line}`Dependency` {bdg-primary-line}`python` {bdg-primary-line}`wrapping`

ITK 6 requires Python 3.11 or newer for Python wrapping; the previous floor was 3.10. Configuring the wrapping against an older interpreter now fails early.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Use Python 3.11+ when building or installing the ITK Python wrapping. The prebuilt `itk` wheels already target 3.11 and newer.

[PR #6677](https://github.com/InsightSoftwareConsortium/ITK/pull/6677) · [`3c7145d69f5`](https://github.com/InsightSoftwareConsortium/ITK/commit/3c7145d69f5)

Record: [`20260721_MinimumWrappedPython311.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260721_MinimumWrappedPython311.yaml)

:::::

(itk6-NiftiImageIOPreservesNonFinite)=
#### `NiftiImageIO` preserves NaN and infinite pixel values

{bdg-warning}`medium` {bdg-info-line}`FileFormat` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

Reading a floating-point or complex NIfTI image previously replaced every
non-finite value with `0`. A file whose voxels were `1.5 NaN Inf -Inf 2.5`
was returned as `1.5 0 0 0 2.5`, with no warning. Non-finite values are now
read verbatim.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

NaN and ±Inf are valid IEEE-754 values that carry meaning — the out-of-mask
voxels of a statistical map are the common case — and `0` is a valid
intensity, so the substitution was both lossy and indistinguishable from
real data.

**What you need to do**

For most pipelines, nothing: the values that now survive were previously
destroyed, so any file that read correctly before still does. The change is
only observable for images that actually contain non-finite voxels, which
previously could not be read faithfully at all.

Code that relied on the reader to sanitize its input should filter
explicitly, which is clearer than depending on an I/O side effect:

```cpp
using ThresholdType = itk::ThresholdImageFilter<ImageType>;
auto sanitizer = ThresholdType::New();
sanitizer->SetInput(reader->GetOutput());
sanitizer->ThresholdOutside(lowerBound, upperBound); // non-finite values fail both tests
sanitizer->SetOutsideValue(0);
```

To restore the previous behavior at the reader instead, set the new flag on
the IO object:

```cpp
auto imageIO = itk::NiftiImageIO::New();
imageIO->ZeroNonFinitePixelsOn();
reader->SetImageIO(imageIO);
```

Note that `NaN` never compares equal to itself, so a test written as
`pixel == std::numeric_limits<PixelType>::quiet_NaN()` is always false;
use `std::isnan(pixel)`.

[PR #6683](https://github.com/InsightSoftwareConsortium/ITK/pull/6683) · [`0d8c7039216`](https://github.com/InsightSoftwareConsortium/ITK/commit/0d8c7039216)

Record: [`20260722_NiftiImageIOPreservesNonFinite.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260722_NiftiImageIOPreservesNonFinite.yaml)

:::::

(itk6-PythonSpatialKeysOrderExplicit)=
#### Python spatial key migration

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`python`

The bare `image['origin']`, `image['spacing']`, and `image['direction']`
string keys on `itk.Image` are deprecated in favor of order-explicit keys.
The bare keys are order-ambiguous across toolkits: ITK returns NumPy
`(z, y, x)` order while SimpleITK returns `(x, y, z)` order for the same key
names, so generic code consuming these keys silently corrupts geometry when
handed the other toolkit's image
([issue #6706](https://github.com/InsightSoftwareConsortium/ITK/issues/6706)).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**The order-explicit keys**

| Key | Order | Value |
|---|---|---|
| `origin_xyz`, `spacing_xyz` | ITK `(x, y, z)` | `GetOrigin()` / `GetSpacing()` |
| `direction_xyz` | ITK | `GetDirection()` as a NumPy matrix |
| `index_xyz`, `size_xyz` | ITK | LargestPossibleRegion index / size |
| `origin_zyx`, `spacing_zyx`, `index_zyx`, `size_zyx` | NumPy `(z, y, x)` | `np.flip` of the `_xyz` value |
| `direction_zyx` | NumPy | `np.flip(direction_xyz, axis=None)` (= P·D·P) |

The `_zyx` frame is a complete coordinate frame matching `np.array(image)`
indexing, with physical coordinates also listed in `(z, y, x)` order:

```python
# forward, equals np.flip(TransformContinuousIndexToPhysicalPoint(i)):
p = image['origin_zyx'] + image['direction_zyx'] @ (image['spacing_zyx'] * i)
# inverse, equals np.flip(TransformPhysicalPointToContinuousIndex(p)):
i = np.linalg.inv(image['direction_zyx']) @ (p - image['origin_zyx']) / image['spacing_zyx']
# np.array(image)[0, 0, 0] sits at continuous index image['index_zyx']
```

Writing `index_*` or `size_*` calls `SetRegions()` followed by `Allocate()`, so
the pixel buffer always matches the advertised extent. A size change discards
the previous pixel data.

**Migrating**

The bare keys returned `(z, y, x)` order, so the drop-in replacement is the
`_zyx` key:

```python
spacing = image['spacing']      # deprecated
spacing = image['spacing_zyx']  # identical values
spacing = image['spacing_xyz']  # GetSpacing() order, matches SimpleITK
```

**Warning behavior**

- By default, bare-key access emits a `DeprecationWarning` (hidden outside
  `__main__` unless enabled with `-W` or `warnings.simplefilter`).
- When ITK is configured with `ITK_FUTURE_LEGACY_REMOVE=ON`, the warning is a
  `FutureWarning`, which Python displays everywhere by default.
- The `ITK_PYTHON_FUTURE_LEGACY_REMOVE` environment variable overrides the
  configured default at runtime (`itkConfig.FutureLegacyRemove`).

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/issues/6706>

[PR #6710](https://github.com/InsightSoftwareConsortium/ITK/pull/6710) · [`8acbdb51228`](https://github.com/InsightSoftwareConsortium/ITK/commit/8acbdb51228) · Old form available until `ITK_FUTURE_LEGACY_REMOVE` is set

Record: [`20260805_PythonSpatialKeysOrderExplicit.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260805_PythonSpatialKeysOrderExplicit.yaml)

:::::

(itk6-EigenEigendecompositionsReplaceEispack)=
#### Eigen-backed eigendecompositions replace netlib EISPACK `vnl_*_eigensystem`

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

`vnl_real_eigensystem`, `vnl_symmetric_eigensystem`, and
`vnl_generalized_eigensystem` (netlib EISPACK `rg`/`rs`/`rsg`), and the only
other client `vnl_scatter_3x3`, are deprecated in favor of Eigen-backed
`itk::bridge::RealEigenDecomposition`, `itk::bridge::SymmetricEigenDecomposition`, and
`itk::bridge::GeneralizedEigenDecomposition`. Under `ITK_FUTURE_LEGACY_REMOVE` these
classes and the entire `v3p/netlib/eispack/` subdirectory are excluded from the
build.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

1. Replace `vnl_symmetric_eigensystem<T>` with `itk::bridge::SymmetricEigenDecomposition<T>` (it mirrors the `V`/`D` members and `get_eigenvector`/`get_eigenvalue` accessors, so most call sites change only the type name and the include: `itkBridgeSymmetricEigenDecomposition.h`). Use `itk::bridge::RealEigenDecomposition` and `itk::bridge::GeneralizedEigenDecomposition` for the non-symmetric and symmetric-definite generalized problems.
1. For the eigenvalues/eigenvectors of a 3x3 scatter matrix, build the matrix directly and feed `itk::bridge::SymmetricEigenDecomposition`.

**A different correct representation — not a regression**

An eigenvector is only defined up to sign (and a complex one up to phase), so the
sign returned is mathematically arbitrary. The new symmetric/generalized classes
**canonicalize** real eigenvector columns to a deterministic convention
(largest-magnitude entry positive), which is reproducible across platforms and
SIMD widths — unlike the EISPACK signs the old baselines were implicitly locked
to. Opt out per call with the `canonicalizeSigns = false` constructor argument.

Because of this, a few filters that consume an eigenvector's raw direction emit
**the same geometric result expressed from a different, equally-valid
representative**:

- `LabelGeometryImageFilter`: the oriented bounding-box *image* is anchored at
  the opposite corner (its origin shifts); the rendered region is identical.
- `ShapeLabelMapFilter` / `StatisticsLabelMapFilter`:
  `GetOrientedBoundingBoxOrigin()` reports the opposite corner for
  non-axis-aligned directions.

In every case the underlying quantity is unchanged — bounding-box **size**,
principal **moments**, elongation, eccentricity, orientation angle, and the
principal **axes** themselves (which flip sign consistently) are all the same.
The origin, axes, and size together still describe the identical box. **If a
baseline or expected value changed, it is a selection of a different correct
representation of the same result, not an indication of a failure.** Update such
expected values to the new (now platform-stable) representatives; the migrated
ITK tests have been updated this way.

[PR #6475](https://github.com/InsightSoftwareConsortium/ITK/pull/6475) · [`073cf3e999b`](https://github.com/InsightSoftwareConsortium/ITK/commit/073cf3e999b) · Old form available until `ITK_FUTURE_LEGACY_REMOVE` is set

Record: [`20260821_EigenEigendecompositionsReplaceEispack.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260821_EigenEigendecompositionsReplaceEispack.yaml)

:::::

(itk6-FftwThroughItkTargets)=
#### FFTW

{bdg-warning}`medium` {bdg-info-line}`ApiRemoved` {bdg-info-line}`Dependency` {bdg-primary-line}`cmake`

FFTW is built or found by the `ITKFFTW` third-party module, and its include directories, libraries, and compile
options are carried by the `ITK::ITKFFTModule` interface library. Link that target, rather than naming FFTW
directories or libraries. Code that includes `fftw3.h` directly is covered too, because `ITK::ITKFFTModule` provides
the FFTW include directory.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Before:**
```cmake
find_package(ITK REQUIRED COMPONENTS ITKFFT)
include(${ITK_USE_FILE})
include_directories(${ITK_FFTW_INCLUDE_PATH})
link_directories(${ITK_FFTW_LIBDIR})
add_executable(Example Example.cxx)
target_link_libraries(Example ${ITK_LIBRARIES} fftw3 fftw3f)
```

**After:**
```cmake
find_package(ITK REQUIRED COMPONENTS ITKFFT)
add_executable(Example Example.cxx)
target_link_libraries(Example PRIVATE ITK::ITKFFTModule)
```

To use FFTW without the FFT filters, request the module directly with `find_package(ITK COMPONENTS ITKFFTW)` and link
`ITK::ITKFFTWModule`.

`ITK_FFTW_INCLUDE_PATH` and `ITK_FFTW_LIBDIR` are no longer defined by `ITKConfig.cmake`, and FFTW directories are no
longer appended to `ITK_INCLUDE_DIRS` and `ITK_LIBRARY_DIRS`. The bundled libraries are installed as `libitkfftw3*`
instead of `libfftw3*`, so linking `fftw3` or `-lfftw3` by name fails to find the library. Only the precisions ITK was
built with (`ITK_USE_FFTWF` and `ITK_USE_FFTWD`) are provided.

When ITK is built against a system FFTW (`ITK_USE_SYSTEM_FFTW=ON`), downstream `find_package(ITK)` runs ITK's
installed `FindFFTW.cmake`, seeded with the include directory and library files ITK found. Set `FFTW_ROOT` to search
for a different installation instead. The ITK build options `FFTW_INCLUDE_PATH`, `FFTWD_BASE_LIB`, `FFTWF_BASE_LIB`,
`FFTWD_THREADS_LIB`, and `FFTWF_THREADS_LIB` are still honored; the library results are now full paths rather than
file names. A separate FFTW threads library is optional.

[PR #6999](https://github.com/InsightSoftwareConsortium/ITK/pull/6999) · [`b5c5da0dea9`](https://github.com/InsightSoftwareConsortium/ITK/commit/b5c5da0dea9)

Record: [`20261027_FftwThroughItkTargets.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261027_FftwThroughItkTargets.yaml)

:::::

(itk6-GradientStructureTensorIsotropicSmoothing)=
#### `GradientStructureTensorImageFilter`: feature-scale smoothing is now isotropic

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

`GradientStructureTensorImageFilter` computes the structure tensor
`J_rho = K_rho * (grad(u_sigma) (x) grad(u_sigma))`, where `K_rho` is a Gaussian
of standard deviation `rho` (the *feature scale*). The defining property of that
smoothing step is that it is **isotropic**: the outer-product tensor field is
convolved with the *same* Gaussian along every spatial axis, so that the tensor
at each voxel aggregates gradient orientations from a rotationally-symmetric
neighborhood. That symmetry is what lets the tensor's eigenvectors report the
locally-dominant structure direction irrespective of how the image is oriented
on the grid.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The implementation built the `K_rho` smoother — and, on one unreachable branch,
the noise-scale `K_sigma` smoother — from `RecursiveGaussianImageFilter`. That
filter derives from `RecursiveSeparableImageFilter` and smooths a **single**
axis, the one named by `SetDirection()`, which was never called and defaults to
`0`. The tensor field was therefore blurred along the first axis only. The
result was not a rotationally-symmetric structure tensor at all: it was
axis-dependent, and rotating the input image changed the tensor field by more
than the rotation alone. The fix replaces both smoothers with
`SmoothingRecursiveGaussianImageFilter`, which chains one recursive Gaussian per
axis and so realizes the intended isotropic `K_rho`. (The gradient smoother in
the same filter already used `GradientRecursiveGaussianImageFilter`, which loops
`SetDirection()` over every axis, and was always correct — the defect was
confined to the two feature/noise-scale smoothers.)

**What you need to do**

- **Expect different output** from `GradientStructureTensorImageFilter` and from
  the two public filters built on it —
  `CoherenceEnhancingDiffusionImageFilter` (CED/EED) and
  `AnisotropicDiffusionLBRImageFilter`. The new tensors are isotropic, so
  coherence-enhancing and edge-enhancing diffusion now steer along genuinely
  orientation-independent structure directions. On most inputs the change is
  visible but modest; on strongly anisotropic or diagonally-oriented structures
  it can be large, because the previous axis-0-only smoothing biased the flow
  toward the first grid axis.
- **Re-baseline** any regression tests or downstream pipelines that pin the exact
  output of these filters. All 16 `AnisotropicDiffusionLBR` image-comparison
  baselines were regenerated for this change; if you maintain your own baselines
  for CED/EED or LBR diffusion, regenerate them the same way.
- If you *depended* on the old single-axis behavior (for example, to smooth the
  tensor field along one axis only), it was never expressible through the public
  API — there is no setter for the smoothing direction — so no supported code
  path is removed.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/7061>

[PR #6602](https://github.com/InsightSoftwareConsortium/ITK/pull/6602) · [`0b55b82aef0`](https://github.com/InsightSoftwareConsortium/ITK/commit/0b55b82aef0)

Record: [`20261031_GradientStructureTensorIsotropicSmoothing.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261031_GradientStructureTensorIsotropicSmoothing.yaml)

:::::

(itk6-MINCTimeDimensionFourthAxis)=
#### `MINCImageIO` reads a MINC time dimension as a fourth image axis

{bdg-warning}`medium` {bdg-info-line}`FileFormat` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

A MINC file whose `time` dimension has more than one frame now reads as a 4D
image.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Time is always axis 3: the time step becomes `GetSpacing()[3]` and the
time start becomes `GetOrigin()[3]`. Missing spatial axes read as size 1, so a
2D series reads as `[x, y, 1, t]`. In ITKv5, each time frame was a pixel
component, so the file read as a `VectorImage` with one component per frame. A
MINC `vector_dimension` still maps to pixel components.

When writing, the fourth axis of a 4D image becomes the MINC `time` dimension,
with its start and step taken from `GetOrigin()[3]` and `GetSpacing()[3]`. The
writer no longer reads the `tstart` and `tstep` metadata entries. Pixel
components are always written as `vector_dimension`; the writer no longer turns
more than three components into a `time` dimension.

**What you need to do**

Read MINC time series into a 4D image type:

```cpp
using ImageType = itk::Image<float, 4>;
const auto image = itk::ReadImage<ImageType>("series.mnc");
const double frameStep = image->GetSpacing()[3];
```

Use `itk::VectorImage<T, 4>` if the file also has a `vector_dimension`.
Reading a time series into a 3D image type returns only the first frame, with
identity direction cosines, because `ImageFileReader` uses default directions
when the file has more dimensions than the image type. To get one frame with
its geometry, read the 4D image and use `ExtractImageFilter` with
`SetDirectionCollapseToSubmatrix()`.

To write a time series, write a 4D image instead of a `VectorImage` with one
component per frame, and set the time start and step through the image origin
and spacing rather than `tstart` and `tstep`.

[PR #6910](https://github.com/InsightSoftwareConsortium/ITK/pull/6910) · [`de4abe61cc9`](https://github.com/InsightSoftwareConsortium/ITK/commit/de4abe61cc9)

Record: [`20261104_MINCTimeDimensionFourthAxis.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261104_MINCTimeDimensionFourthAxis.yaml)

:::::

(itk6-WorkUnitPerThreadRatio)=
#### `MultiThreaderBase` global default work-unit-per-thread ratio

{bdg-warning}`medium` {bdg-info-line}`DefaultChanged` {bdg-info-line}`Performance` {bdg-primary-line}`cxx`

`PoolMultiThreader` and `TBBMultiThreader` now initialize their number of work
units as `GetGlobalDefaultNumberOfThreads()` times the new
`MultiThreaderBase::GetGlobalDefaultWorkUnitPerThreadRatio()`, instead of a
hard-coded factor. The default ratio is 4, which matches the previous
`PoolMultiThreader` behavior but reduces the `TBBMultiThreader` default from 16
to 4 work units per thread. Fewer, larger work units lower scheduling overhead
for most filters.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

To restore the previous `TBBMultiThreader` granularity, or to tune either
threader, set the ratio before the multi-threaders are constructed:

```cpp
itk::MultiThreaderBase::SetGlobalDefaultWorkUnitPerThreadRatio(16);
```

or set the `ITK_GLOBAL_DEFAULT_WORK_UNIT_PER_THREAD_RATIO` environment variable
(for example `ITK_GLOBAL_DEFAULT_WORK_UNIT_PER_THREAD_RATIO=16`). An explicit
call to the setter takes precedence over the environment variable.

The value is clamped to at least 1 and does not affect instances that already
exist. A single-threaded configuration still uses one work unit.

[PR #7122](https://github.com/InsightSoftwareConsortium/ITK/pull/7122) · [`fa8d90b15dc`](https://github.com/InsightSoftwareConsortium/ITK/commit/fa8d90b15dc)

Record: [`20261106_WorkUnitPerThreadRatio.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261106_WorkUnitPerThreadRatio.yaml)

:::::

(itk6-PythonLongDoubleWrappingRemoved)=
#### Remove support for Python wrapped `long double` types

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`python` {bdg-primary-line}`wrapping`

The swig wrapping of `long double` types into python
resulted in implicit type conversions to `double`,
which results in silent loss of precision.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

There has never been an option for "ITK_WRAP_long_double"
configuration, and manually wrapped functions in vnl for
for long double were never needed by ITK based functions.

Given the undefined behavior of wrapping long double types
with swig, and given that there is no use case for long double
support directly from the wrapped ITK API, these manual vnl
wrappings could not be reached from ITK interfaces.

The handful of manually wrapped long double functions were
removed from python wrapping.

[PR #5752](https://github.com/InsightSoftwareConsortium/ITK/pull/5752) · [`61ea2c7bfd3`](https://github.com/InsightSoftwareConsortium/ITK/commit/61ea2c7bfd3)

Record: [`20260130_PythonLongDoubleWrappingRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260130_PythonLongDoubleWrappingRemoved.yaml)

:::::

(itk6-GoogleTestLegacyTargetsRemoved)=
#### Legacy GoogleTest Target Names Removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cmake`

ITK 6 now uses the standard CMake `FindGTest` target names for GoogleTest libraries, aligning with upstream project and CMake.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Target Name Changes**

**Before (ITK 5):**
```cmake
target_link_libraries(MyTest
  GTest::GTest      # legacy target
  GTest::Main       # legacy target
)
```

**After (ITK 6):**
```cmake
target_link_libraries(MyTest
  GTest::gtest      # compatible target
  GTest::gtest_main # compatible target
)
```

**Rationale**

These names were deprecated in CMake 3.20 and removed in CMake 4.1.0. Additionally, the GoogleTest project itself uses the lowercase target names (`GTest::gtest` and `GTest::gtest_main`), meaning the old ITK-specific aliases were not compatible when using GoogleTest directly from its upstream repository. ITK 6 adopts the standard lowercase target names to ensure compatibility with modern CMake versions, the GoogleTest project, and consistency with other projects.

[PR #5791](https://github.com/InsightSoftwareConsortium/ITK/pull/5791) · [`1648e8e25d7`](https://github.com/InsightSoftwareConsortium/ITK/commit/1648e8e25d7)

Record: [`20260211_GoogleTestLegacyTargetsRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260211_GoogleTestLegacyTargetsRemoved.yaml)

:::::

(itk6-PythonLazyLoadingAlwaysOn)=
#### Python lazy loading is always on

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`python`

The Python wrapping layer's lazy-loading mechanism has been rewritten on top
of the standard-library [PEP 562](https://peps.python.org/pep-0562/)
module-level `__getattr__` / `__dir__` protocol. Lazy first-touch resolution
is now the only mode of operation: there is no eager-import alternative.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Removed**

- `itkConfig.LazyLoading` — previously a boolean attribute on `itkConfig`
  that, when set to `False`, forced every wrapped submodule to be imported
  at `import itk` time.
- `ITK_PYTHON_LAZYLOADING` — the environment variable that seeded the value
  of `itkConfig.LazyLoading` at startup.
- The custom `itk.support.lazy.LazyITKModule` `types.ModuleType` subclass
  (and its `ITKLazyLoadLock`, `_lazy_itk_module_reconstructor`, and
  `not_loaded` sentinel) that the previous implementation relied on.

**What you need to do**

If your code or test suite contains either of the following, remove
them:

- `itkConfig.LazyLoading = False` — assignment alone is silently ignored
  (module attributes are mutable, but no ITK code reads the value
  anymore). On a fresh `import itkConfig` with no prior assignment, a
  *read* of `itkConfig.LazyLoading` raises `AttributeError` because the
  attribute is no longer defined. Delete both the writes and any reads:

  ```python
  import itkConfig

  # Remove any assignments — they are silently no-ops:
  itkConfig.LazyLoading = False

  # Remove any reads — without a prior assignment they raise
  # AttributeError, since itkConfig no longer defines this attribute:
  if itkConfig.LazyLoading:
      ...
  ```

- `ITK_PYTHON_LAZYLOADING` — the environment variable is no longer
  consulted. Any export is silently ignored; remove it from launch
  scripts and CI configurations:

  ```bash
  ITK_PYTHON_LAZYLOADING=0 python my_script.py  # drop the env var
  ```

The observable contract from a caller's point of view is otherwise
unchanged: `import itk` returns the package, attribute access on `itk`
or any `itk.<Submodule>` resolves on first touch, `dir(itk)` lists the
full set of lazily-discoverable attributes, and pickle / cloudpickle
round-trips through `sys.modules` continue to work.

**Why this changed**

The previous `LazyITKModule.__getattribute__` intercepted *every* attribute
read on the `itk` package just to check a sentinel; the PEP 562 hook only
fires on a miss, so steady-state attribute access now hits the standard
module fast path. The legacy `ITKLazyLoadLock` also combined
`threading.RLock` and `multiprocessing.RLock`, and constructing the
multiprocessing half pinned the multiprocessing start method at
`import itk` time — breaking downstream callers that wanted to call
`multiprocessing.set_start_method(...)` after import. The replacement uses
only `threading.RLock`.

[PR #6183](https://github.com/InsightSoftwareConsortium/ITK/pull/6183) · [`94491a6ce41`](https://github.com/InsightSoftwareConsortium/ITK/commit/94491a6ce41)

Record: [`20260504_PythonLazyLoadingAlwaysOn.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260504_PythonLazyLoadingAlwaysOn.yaml)

:::::

(itk6-MathSqrtepsOneUlp)=
#### `itk::Math::sqrteps` changed by 1 ULP for accuracy

{bdg-secondary}`low` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx`

`itk::Math::sqrteps` is now `0x1p-26`, the exact `sqrt(eps)`;
`vnl_math::sqrteps` (`1.490116119384766e-08`) was 1 ULP higher.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

A 1 ULP change affects only comparisons at the limit of double precision.

[PR #6427](https://github.com/InsightSoftwareConsortium/ITK/pull/6427) · [`589d68004ab`](https://github.com/InsightSoftwareConsortium/ITK/commit/589d68004ab)

Record: [`20260610_MathSqrtepsOneUlp.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260610_MathSqrtepsOneUlp.yaml)

:::::

(itk6-FixedPointInverseDisplacementFieldOutputIterator)=
#### `FixedPointInverseDisplacementFieldImageFilter::OutputIterator` is now `ImageRegionIteratorWithIndex`

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

The public nested type alias
`FixedPointInverseDisplacementFieldImageFilter<...>::OutputIterator` changed from
`ImageRegionIterator<OutputImageType>` to
`ImageRegionIteratorWithIndex<OutputImageType>`. The filter calls `GetIndex()` on
this iterator, which `ImageRegionIterator` only provides under the removed
`ITK_FUTURE_LEGACY_REMOVE` API; `ImageRegionIteratorWithIndex` supplies it
natively.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Downstream code that names the concrete alias explicitly -- e.g. a template
specialization or overload keyed on `ImageRegionIterator<OutputImageType>`, or a
variable declared as `FilterType::OutputIterator` and then passed where an
`ImageRegionIterator` is required -- must accept
`ImageRegionIteratorWithIndex<OutputImageType>` instead. The two iterators are
siblings in the iterator hierarchy, not subclasses, so such uses do not convert
implicitly. Code that only iterates (`GoToBegin`/`IsAtEnd`/`Get`/`Set`) needs no
change.

[PR #6504](https://github.com/InsightSoftwareConsortium/ITK/pull/6504) · [`60640e009a3`](https://github.com/InsightSoftwareConsortium/ITK/commit/60640e009a3)

Record: [`20260626_FixedPointInverseDisplacementFieldOutputIterator.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260626_FixedPointInverseDisplacementFieldOutputIterator.yaml)

:::::

(itk6-ItkFemModulesRemoved)=
#### ITKFEM and ITKFEMRegistration modules removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cmake` {bdg-primary-line}`cxx`

The `ITKFEM` (`Modules/Numerics/FEM`) and `ITKFEMRegistration`
(`Modules/Registration/FEM`) modules are removed from the ITK main tree.
Both were opt-in (`EXCLUDE_FROM_DEFAULT`) and no in-tree module depended
on them.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The modules, with their full history, are preserved as the unmaintained
remote module `InsightSoftwareConsortium/ITKFEM`. Projects that enabled
`Module_ITKFEM` / `Module_ITKFEMRegistration`, linked `ITKFEM` /
`ITKFEMRegistration`, or used `itk::fem::*` classes (including
`FEMRegistrationFilter` and the `LinearSystemWrapper*` family) can re-enable
it via `Module_FEM` (a `Modules/Remote/FEM.remote.cmake` breadcrumb). It is
unmaintained and scheduled for removal in ITK 7 (see
[InsightSoftwareConsortium/ITK#6511](https://github.com/InsightSoftwareConsortium/ITK/issues/6511));
if you depend on it, comment on that issue.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/issues/6511>, <https://github.com/InsightSoftwareConsortium/ITKFEM>

[PR #6524](https://github.com/InsightSoftwareConsortium/ITK/pull/6524) · [`caf908a1a5a`](https://github.com/InsightSoftwareConsortium/ITK/commit/caf908a1a5a)

Record: [`20260626_ItkFemModulesRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260626_ItkFemModulesRemoved.yaml)

:::::

(itk6-DeprecateIsNumber)=
#### Deprecate Is Number

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**Class changes**

itkIsNumber.h got deprecated. To modernize your code base, replace `mpl::IsNumber<X>::Value`
with `std::is_arithmetic_v<X>`, and `#include "itkIsNumber.h"` with `#include <type_traits>`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #6573](https://github.com/InsightSoftwareConsortium/ITK/pull/6573) · [`956a801e180`](https://github.com/InsightSoftwareConsortium/ITK/commit/956a801e180)

Record: [`20260710_DeprecateIsNumber.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260710_DeprecateIsNumber.yaml)

:::::

(itk6-EntropyThresholdMaxEntropySeedCorrected)=
#### `RenyiEntropyThresholdCalculator`/`MaximumEntropyThresholdCalculator`: max-entropy seed corrected

{bdg-secondary}`low` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

Both calculators seeded their running-maximum entropy accumulator with
`NumericTraits<double>::min()` -- the smallest *positive* double, not the
most negative representable value.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

These entropy formulas are provably
non-negative, but legitimately reach exactly `0` for a degenerate or
tied sub-histogram split (common with `AutoMinimumMaximum` off, since
the fixed bin range then often extends past the image's actual data) --
and `0` does not exceed `min()`. The reported threshold could then stick
at its pre-loop initial value instead of the bin that actually maximizes
entropy. The seed is now `NumericTraits<double>::NonpositiveMin()`, which
is guaranteed to compare less than any real entropy value, including `0`.

**What you need to do**

Nothing in your code needs to change -- the fix is internal to the
calculators. But if you depend on an exact threshold from either
calculator, or from `RenyiEntropyThresholdImageFilter` /
`MaximumEntropyThresholdImageFilter`, with `AutoMinimumMaximum` off and a
configured min/max range wider than the image's actual intensity range,
re-verify that threshold -- it may now differ (correctly) from the
previous ITK release. The common case, `AutoMinimumMaximum` on (the
default) with a non-degenerate histogram, is unaffected.

[PR #6635](https://github.com/InsightSoftwareConsortium/ITK/pull/6635) · [`3095cceb444`](https://github.com/InsightSoftwareConsortium/ITK/commit/3095cceb444)

Record: [`20260715_EntropyThresholdMaxEntropySeedCorrected.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260715_EntropyThresholdMaxEntropySeedCorrected.yaml)

:::::

(itk6-BridgeMathSVD)=
#### Optional Eigen-backed `itk::bridge::Math::SVD`

{bdg-secondary}`low` {bdg-info-line}`Performance` {bdg-primary-line}`cxx`

A new opt-in, header-only `itk::bridge::Math::SVD` (`itkBridgeMathSVD.h`) provides an
Eigen-backed singular value decomposition.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

It returns `U`, `W`, `V` as vnl types
plus `PseudoInverse()`, `Solve()`, `Rank()` and `Recompose()` -- the dominant `vnl_svd`
operations. `vnl_svd` is unchanged by this addition, but the direction is to
migrate ITK and downstream code onto `itk::bridge::Math::SVD` and ultimately **deprecate
and remove** the `vnl_svd` path. Prefer `itk::bridge::Math::SVD` in new code.

A runtime-sized `vnl_matrix` of any shape is accepted: square inputs take a fast
JacobiSVD/BDCSVD path; rectangular inputs use BDCSVD with thin U/V (`U` is m x k,
`V` is n x k, `k = min(m, n)`). The square-vs-rectangular choice is a single
runtime dimension comparison, so the square fast path is unaffected. The
fixed-size overloads (`vnl_matrix_fixed`, `itk::Matrix`) are square-only by
signature.

**What you need to do**

Nothing is required immediately, but prefer `itk::bridge::Math::SVD` in new code. The
`vnl_svd`-only operations (`nullvector`, `well_condition`, `singularities`) have
no remaining callers in ITK, so the new API already covers every ITK use; the few
remaining `vnl_svd` call sites are slated for migration as part of the broader
vnl-to-Eigen deprecation effort.

**Performance: faster on the common and critical paths, at equal accuracy**

Accuracy is equivalent to `vnl_svd` -- reconstruction and singular values agree
to machine precision (the newer BDCSVD divide-and-conquer engine is marginally
more accurate at large sizes). Speed is size-dependent (idle, single-thread,
AppleClang arm64; ratio > 1 means the Eigen path is faster):

| Square size | Eigen vs vnl_svd |
|---|---|
| 3x3 | ~2.2x faster |
| 4x4 | ~2.0x faster |
| 6x6 | ~parity |
| 8x8 | slower (~0.5-0.8x) -- `vnl_svd`/LINPACK wins (rarely used) |
| 16x16 | slower (~0.5-0.8x) -- `vnl_svd`/LINPACK wins (rarely used) |
| >= 50x50 | ~1.5-2.3x faster (BDCSVD) |

LINPACK may have a hardware-alignment advantage at these mid sizes that has not
been explored.

The **rare** cases where Eigen is similar or slightly slower are mid-sized dense
square matrices (8x8 and 16x16). These are seldom -- perhaps never -- exercised in
practice across the ITK and downstream call sites, which are dominated by 3x3/4x4
transform and image-orientation problems (where Eigen is ~2x faster) plus
occasional large dense systems (where BDCSVD wins). On the common and critical
paths the Eigen path is a significant performance improvement with no loss of
accuracy.

**Deterministic signs -- a different correct representation**

Like the eigendecomposition classes above, `itk::bridge::Math::SVD` canonicalizes the
singular-vector signs by default (reproducible across platforms and SIMD width),
whereas `vnl_svd`'s signs depend on solver internals. Sign-invariant operations
-- `U * V^T`, `PseudoInverse()`, `Solve()` -- are stable across the swap; code that
reads individual `U`/`V` columns as directions may see the same geometry from a
different, equally-valid representative. Pass `canonicalizeSigns = false` to
reproduce `vnl_svd`'s raw signs (e.g. for equivalence testing).

[PR #6487](https://github.com/InsightSoftwareConsortium/ITK/pull/6487) · [`5c4ebebf1a1`](https://github.com/InsightSoftwareConsortium/ITK/commit/5c4ebebf1a1)

Record: [`20260821_BridgeMathSVD.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260821_BridgeMathSVD.yaml)

:::::

(itk6-BridgeNamespaceMigrationAid)=
#### The `itk::bridge` namespace: a migration aid, not supported ITK API

{bdg-secondary}`low` {bdg-info-line}`Policy` {bdg-primary-line}`cxx`

The Eigen-backed replacements named throughout this guide live in
`itk::bridge`. That namespace holds **convenience wrappers intended to ease the
initial burden of moving off the deprecated VNL algorithms** — a `vnl_svd` call
site can become `itk::bridge::Math::SVD` as a mechanical edit, without the
caller first learning Eigen's API.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

These wrappers are **not part of the core ITK mission to maintain or validate**:

- The API and ABI may change, or be removed, without the deprecation cycle that
  core ITK API changes receive.
- The numerical behavior is the backend's. ITK does not independently validate
  accuracy, conditioning, or convergence, and does not guarantee bit-for-bit
  stability across backend versions.
- Coverage extends only to what ITK's own migration required; missing
  functionality will not necessarily be added.

**Downstream users should prefer depending on Eigen (or another numerical
library) directly** rather than on the API/ABI of these wrappers. Using
`itk::bridge` as a transitional step while retiring VNL is reasonable; treating
it as a permanent dependency is not. Code that needs a long-lived, supported
numerical interface should call the backend directly, where the API contract,
versioning policy, and documentation are the numerical library's own.

[PR #6768](https://github.com/InsightSoftwareConsortium/ITK/pull/6768) · [`a2c58cc9bc4`](https://github.com/InsightSoftwareConsortium/ITK/commit/a2c58cc9bc4)

Record: [`20260821_BridgeNamespaceMigrationAid.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260821_BridgeNamespaceMigrationAid.yaml)

:::::

(itk6-GetInverseTransformConcreteType3D)=
#### `GetInverseTransform()` returns the concrete transform type for more 3D transforms

{bdg-secondary}`low` {bdg-info-line}`ResultsChange` {bdg-info-line}`FileFormat` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

`Transform::GetInverseTransform()` returns an inverse wrapped in an
`InverseTransformBasePointer`. For several 3D transforms the *runtime* type of
that inverse was the parent class rather than the transform's own type: an
`Euler3DTransform` inverse came back as a `MatrixOffsetTransformBase`, a
`ScaleLogarithmicTransform` inverse as a `ScaleTransform`, and so on. The 2D
transform family (`Rigid2DTransform`, `Euler2DTransform`, `Similarity2DTransform`,
and the centered variants) already overrode `GetInverseTransform()` to preserve
the concrete type; their 3D counterparts did not.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The following transforms now return an inverse of their own concrete type:
`Euler3DTransform`, `QuaternionRigidTransform`, `Similarity3DTransform`,
`VersorRigid3DTransform`, `VersorTransform`,
`FixedCenterOfRotationAffineTransform`, and `ScaleLogarithmicTransform`. The
inverse is still computed by the inherited `GetInverse()`, which re-extracts the
constrained parameters (Euler angles, versor, log-scales) through the virtual
`ComputeMatrixParameters()`; only the returned pointer's concrete type changed.

`BSplineTransform` (no closed-form global inverse; returns `nullptr`) and
`AzimuthElevationToCartesianTransform` (nonlinear; its inverse is an affine
approximation) are unchanged and intentionally do not return their own type.

**What you need to do**

- **Nothing in most code.** Calls that use the inverse through the
  `InverseTransformBasePointer` base interface, or that `dynamic_cast`/`static_cast`
  the inverse to a *base* type (for example `MatrixOffsetTransformBase` to read
  `GetMatrix()`), continue to work: the returned object is now *more* derived, so
  every upcast still succeeds.
- **Re-baseline serialized inverses.** A transform inverse written to disk
  (`.tfm`, `.h5`) now records the derived class name (e.g.
  `Euler3DTransform_double_3_3` instead of `MatrixOffsetTransformBase_double_3_3`).
  Factory-based readers load either form, but regression tests that byte- or
  string-compare stored transform files must be regenerated.
- If you relied on the inverse being *exactly* the parent type (an exact
  `typeid` comparison, or a `static_cast` to a sibling type), update that code to
  the new concrete type. Such a dependency is uncommon and was not found in a
  survey of downstream ITK consumers.

[PR #6689](https://github.com/InsightSoftwareConsortium/ITK/pull/6689) · [`13ebafff53f`](https://github.com/InsightSoftwareConsortium/ITK/commit/13ebafff53f)

Record: [`20260821_GetInverseTransformConcreteType3D.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260821_GetInverseTransformConcreteType3D.yaml)

:::::

(itk6-IndentTimeStampNewDeleteDeprecated)=
#### Indent Time Stamp New Delete Deprecated

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**Class changes**

The member functions `New()` and `Delete()` of `itk::Indent` and `itk::TimeStamp` are
deprecated. If your application is still using them, you may replace their use with
constructing instances of these types directly.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/6897>

[PR #6888](https://github.com/InsightSoftwareConsortium/ITK/pull/6888) · [`f5eab1e261d`](https://github.com/InsightSoftwareConsortium/ITK/commit/f5eab1e261d)

Record: [`20260923_IndentTimeStampNewDeleteDeprecated.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20260923_IndentTimeStampNewDeleteDeprecated.yaml)

:::::

(itk6-GradientImageFilterSetBoundaryCondition)=
#### `GradientImageFilter::OverrideBoundaryCondition` deprecated in favor of `SetBoundaryCondition`

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx` {bdg-primary-line}`python`

`GradientImageFilter` is the only ITK class whose `OverrideBoundaryCondition` *takes
ownership* of its argument; every other class of that name stores a non-owning
pointer. Because the two share a signature, passing a stack object to a
`GradientImageFilter` compiled silently and then double-freed.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The owning API now says so in its name and its type:

```cpp
// ITKv6
filter->SetBoundaryCondition(std::make_unique<itk::PeriodicBoundaryCondition<ImageType>>());

// ITKv5 (deprecated, removed when ITK_FUTURE_LEGACY_REMOVE is enabled)
filter->OverrideBoundaryCondition(new itk::PeriodicBoundaryCondition<ImageType>);
```

`GetBoundaryCondition()` and `ResetBoundaryCondition()` were added to complete the
interface, matching the rest of the family.

**What you need to do**

Replace `OverrideBoundaryCondition(new X)` with `SetBoundaryCondition(std::make_unique<X>())`
on `GradientImageFilter`. In Python, pass the boundary condition to
`SetBoundaryCondition`; ownership moves to the filter, and re-using that object
afterwards raises `RuntimeError` rather than crashing.

Calls to `OverrideBoundaryCondition` on any *other* class are unaffected — those
never took ownership and keep their current behavior.

[PR #6714](https://github.com/InsightSoftwareConsortium/ITK/pull/6714) · [`62033711d5f`](https://github.com/InsightSoftwareConsortium/ITK/commit/62033711d5f) · Old form available until `ITK_FUTURE_LEGACY_REMOVE` is set

Record: [`20261023_GradientImageFilterSetBoundaryCondition.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261023_GradientImageFilterSetBoundaryCondition.yaml)

:::::

(itk6-PythonGilRelease)=
#### Python Global Interpreter Lock (GIL) Release

{bdg-secondary}`low` {bdg-info-line}`Performance` {bdg-primary-line}`python` {bdg-primary-line}`cmake`

ITK releases the Python Global Interpreter Lock (GIL) during `Update()` and
`UpdateLargestPossibleRegion()` by default. Other wrapped methods retain the GIL.
This allows independent pipelines to execute concurrently from Python threads.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Key Changes**

**New CMake Option:**
- `ITK_PYTHON_RELEASE_GIL` (default: `ON`) - Controls GIL release during pipeline execution
- When enabled, SWIG's `-threads` flag supports GIL release in the update wrappers

**Benefits:**
- Multiple Python threads can execute ITK operations concurrently
- Improves performance in parallel computing scenarios
- Prevents thread blocking when using frameworks like Dask

**Example:**

```python
import itk
import threading

image_paths = ["image1.mha", "image2.mha"]

def process_image(image_path):
    # Pipeline updates release the GIL.
    image = itk.imread(image_path)
    smoothed = itk.median_image_filter(image, radius=5)
    return smoothed

# Multiple threads can now execute ITK operations concurrently
threads = [
    threading.Thread(target=process_image, args=(path,))
    for path in image_paths
]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
```

**Note:** Python callbacks reacquire the GIL. Concurrent threads must use independent
pipelines; GIL release does not make shared filters or images thread-safe.
Build with `-DITK_PYTHON_RELEASE_GIL=OFF` to retain the GIL during updates.

[PR #5776](https://github.com/InsightSoftwareConsortium/ITK/pull/5776) · [`f8b120f5205`](https://github.com/InsightSoftwareConsortium/ITK/commit/f8b120f5205)

Record: [`20261028_PythonGilRelease.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261028_PythonGilRelease.yaml)

:::::

(itk6-AutoloadPathReadOnce)=
#### `ITK_AUTOLOAD_PATH` is read once; use `ObjectFactoryBase::SetAutoloadPaths`

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

The `ITK_AUTOLOAD_PATH` environment variable lists the directories ITK scans for
shared libraries that register dynamically loaded object factories (IO plugins,
for example). Entries are separated by `:` (`;` on Windows). The scan happens
lazily, the first time a factory is used, and only when ITK is built with
`ITK_USE_AUTOLOAD_PATH` (dynamic loading enabled, not a Python-wrapped build).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The autoload paths are now stored in `ObjectFactoryBase` global state as a
`std::list<std::string>`, initialized by splitting the `ITK_AUTOLOAD_PATH`
environment variable when that state is created.
`ObjectFactoryBase::SetAutoloadPaths()` and `GetAutoloadPaths()` set and query
the list at runtime. `ReHash()` no longer re-reads the environment.

**What you need to do**

Code that calls `putenv("ITK_AUTOLOAD_PATH=...")` after startup and then
`ObjectFactoryBase::ReHash()` must build a `std::list<std::string>` of
directories and call `ObjectFactoryBase::SetAutoloadPaths(...)` instead. When
factories were already initialized, follow it with `ReHash()`.

[PR #6985](https://github.com/InsightSoftwareConsortium/ITK/pull/6985) · [`32419f36356`](https://github.com/InsightSoftwareConsortium/ITK/commit/32419f36356)

Record: [`20261030_AutoloadPathReadOnce.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261030_AutoloadPathReadOnce.yaml)

:::::

(itk6-AnisotropicDiffusionLBRGradientStructureTensorRename)=
#### Anisotropic Diffusion LBRGradient Structure Tensor Rename

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

1. **Only code that names the tensor class changes.** The AnisotropicDiffusionLBR class `StructureTensorImageFilter` is now `GradientStructureTensorImageFilter` (header `itkGradientStructureTensorImageFilter.h`), because the IsotropicWavelets module defined a different class with the same name. The class was never wrapped for Python. Its interface is otherwise unchanged, and `AnisotropicDiffusionLBRImageFilter` and `CoherenceEnhancingDiffusionImageFilter` users need no edits.

**`GradientStructureTensorImageFilter`: feature-scale smoothing is now isotropic**

**What you need to do**

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/issues/7057>

[PR #7061](https://github.com/InsightSoftwareConsortium/ITK/pull/7061) · [`3e464776516`](https://github.com/InsightSoftwareConsortium/ITK/commit/3e464776516)

Record: [`20261031_AnisotropicDiffusionLBRGradientStructureTensorRename.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261031_AnisotropicDiffusionLBRGradientStructureTensorRename.yaml)

:::::

(itk6-RieszStructureTensorImageFilterRename)=
#### IsotropicWavelets `StructureTensorImageFilter` is now `RieszStructureTensorImageFilter`

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx` {bdg-primary-line}`python`

The IsotropicWavelets module and the AnisotropicDiffusionLBR module both defined
`itk::StructureTensorImageFilter` with the same include guard, so a build that
enabled both modules could not use either class, and Python wrapping failed to
compile (issue [#7057](https://github.com/InsightSoftwareConsortium/ITK/issues/7057)). The two classes are different algorithms. The
IsotropicWavelets class takes two or more component images (for example Riesz
components) and outputs per-pixel eigenvectors and eigenvalues; the
AnisotropicDiffusionLBR class takes one image and outputs the tensor.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

1. Replace `itk::StructureTensorImageFilter` with `itk::RieszStructureTensorImageFilter` and include `itkRieszStructureTensorImageFilter.h`. Template parameters, setters, and output are unchanged.
1. In Python, replace `itk.StructureTensorImageFilter` with `itk.RieszStructureTensorImageFilter`; the wrapped instantiations are the same.
1. The old name is not kept as an alias, because an alias would reintroduce the name clash.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/issues/7057>

[PR #7061](https://github.com/InsightSoftwareConsortium/ITK/pull/7061) · [`3e464776516`](https://github.com/InsightSoftwareConsortium/ITK/commit/3e464776516)

Record: [`20261031_RieszStructureTensorImageFilterRename.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261031_RieszStructureTensorImageFilterRename.yaml)

:::::

(itk6-ByteOrderMacrosDeprecated)=
#### Byte-order macros are deprecated in `itkConfigure.h`

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-info-line}`CompilerEnvironment` {bdg-primary-line}`cxx`

`itkConfigure.h` no longer takes the byte order from CMake; `CMAKE_WORDS_BIGENDIAN`
and `ITK_WORDS_BIGENDIAN` are defined only when ITK is built without
`ITK_LEGACY_REMOVE`. `itk::ByteSwapper<T>::SystemIsBigEndian()` is `constexpr`
and determines the byte order of the architecture being compiled for from the
compiler (`std::endian` in C++20, otherwise the compiler's own macros), so it is
correct for each architecture of a universal binary or a cross-compile, which a
value detected at configure time is not.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Replace each preprocessor test of either macro with the `constexpr` query; its
result does not depend on the type argument. A test that selects a value:

```cpp
// Before
#ifdef ITK_WORDS_BIGENDIAN
  m_UseMSBForHashing = true;
#else
  m_UseMSBForHashing = false;
#endif

// After
#include "itkByteSwapper.h"
m_UseMSBForHashing = itk::ByteSwapper<TScalar>::SystemIsBigEndian();
```

A test that guards a statement:

```cpp
// Before
#ifndef ITK_WORDS_BIGENDIAN
  png_set_swap(png_ptr);
#endif

// After
#include "itkByteSwapper.h"
if constexpr (!itk::ByteSwapper<unsigned short>::SystemIsBigEndian())
{
  png_set_swap(png_ptr);
}
```

The same replacements apply to `CMAKE_WORDS_BIGENDIAN`. The query is usable in
`static_assert` and template arguments as well, since it is `constexpr`.

[PR #7112](https://github.com/InsightSoftwareConsortium/ITK/pull/7112) · [`174c2062d31`](https://github.com/InsightSoftwareConsortium/ITK/commit/174c2062d31) · Old form available until `ITK_LEGACY_REMOVE` is set

Record: [`20261105_ByteOrderMacrosDeprecated.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261105_ByteOrderMacrosDeprecated.yaml)

:::::

(itk6-HasCpp11TypeTraitsRemoved)=
#### `ITK_HAS_CPP11_TYPETRAITS` is removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

`itkConfigure.h` no longer defines `ITK_HAS_CPP11_TYPETRAITS`. It was always
defined in builds without `ITK_LEGACY_REMOVE`, and ITK itself never tested it.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Delete any `#ifdef ITK_HAS_CPP11_TYPETRAITS` test and keep the branch that uses
`<type_traits>`.

[PR #7121](https://github.com/InsightSoftwareConsortium/ITK/pull/7121) · [`2c23ea2df35`](https://github.com/InsightSoftwareConsortium/ITK/commit/2c23ea2df35)

Record: [`20261105_HasCpp11TypeTraitsRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261105_HasCpp11TypeTraitsRemoved.yaml)

:::::

(itk6-HeadersOneFormEveryCxxStandard)=
#### ITK headers present one form in every C++ standard

{bdg-secondary}`low` {bdg-info-line}`CompilerEnvironment` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

ITK headers no longer select declarations or inline code from `__cplusplus` or
feature-test macros. A project built as C++20 or newer now sees the same
definitions as ITK's own libraries, which are the C++17 forms. For example,
classes that use `ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION` keep their member
`operator!=` in a C++20 build.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The advanced CMake options `ITK_ALPHA_FEATURE_CXX20`,
`ITK_ALPHA_FEATURE_CXX23`, and `ITK_ALPHA_FEATURE_CXX26` select the newer
forms. They are off by default, unsupported, and for testing only.

**What you need to do**

Nothing, in most projects. If your code tests
`ITK_EXPERIMENTAL_CXX20_REWRITTEN_UNEQUAL_OPERATOR`, note that the macro is no
longer defined in a C++20 build.

[PR #7119](https://github.com/InsightSoftwareConsortium/ITK/pull/7119) · [`7397293bdd0`](https://github.com/InsightSoftwareConsortium/ITK/commit/7397293bdd0)

Record: [`20261105_HeadersOneFormEveryCxxStandard.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261105_HeadersOneFormEveryCxxStandard.yaml)

:::::

(itk6-ItkHaveFenvHRemoved)=
#### `ITK_HAVE_FENV_H` is removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

`itkConfigure.h` no longer defines `ITK_HAVE_FENV_H`. `<cfenv>` is part of
C++11, so the macro was defined on every toolchain that can build ITK, and ITK
itself never tested it.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Delete any `#ifdef ITK_HAVE_FENV_H` test and include `<cfenv>` unconditionally.

[PR #7123](https://github.com/InsightSoftwareConsortium/ITK/pull/7123) · [`d3137d39c24`](https://github.com/InsightSoftwareConsortium/ITK/commit/d3137d39c24)

Record: [`20261105_ItkHaveFenvHRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261105_ItkHaveFenvHRemoved.yaml)

:::::

(itk6-MinimumMsvc1922)=
#### Minimum Visual Studio raised to 2019 16.2

{bdg-secondary}`low` {bdg-info-line}`CompilerEnvironment` {bdg-primary-line}`cxx`

ITK 6 requires MSVC 19.22 (Visual Studio 2019 16.2) or newer; the previous floor was 19.20 (16.0). Releases 16.0 and 16.1 reject `using typename Superclass::X;` declarations with errors C2653 and C2886, which 16.2 fixed. Configuring with an older compiler now fails early.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

Update Visual Studio 2019 to 16.11, its final servicing baseline, with the Visual Studio Installer, or use Visual Studio 2022. ITK's own Windows CI already builds with Visual Studio 2022.

[PR #7129](https://github.com/InsightSoftwareConsortium/ITK/pull/7129) · [`bf26eb29d61`](https://github.com/InsightSoftwareConsortium/ITK/commit/bf26eb29d61)

Record: [`20261106_MinimumMsvc1922.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261106_MinimumMsvc1922.yaml)

:::::

(itk6-RequireSse2On32BitX86)=
#### 32-bit x86 requires SSE2 floating-point math

{bdg-secondary}`low` {bdg-info-line}`CompilerEnvironment` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

ITK 6 compiles one floating-point rounding path on x86. On 32-bit x86, `itkConfigure.h` stops with `#error` unless the compiler uses SSE2 floating-point math, and the x87 inline assembly is removed. ITK's targets and `UseITK.cmake` pass `-msse2 -mfpmath=sse` to GCC and Clang, and MSVC has defaulted to SSE2 since Visual Studio 2012, so most builds need no change. 64-bit x86 and ARM are not affected.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The CMake variable `ITK_SSE2_CFLAGS_IF_AVAILABLE` and the `check_sse2_flags()` function are removed. `ITK_COMPILER_SUPPORTS_SSE2_32` (any x86) and `ITK_COMPILER_SUPPORTS_SSE2_64` (x86_64) are still defined, but ITK no longer reads them.

**What you need to do**

A 32-bit x86 project that compiles ITK headers without ITK's imported targets or `UseITK.cmake` adds `-msse2 -mfpmath=sse` (GCC, Clang) or `/arch:SSE2` (MSVC). Remove any use of `ITK_SSE2_CFLAGS_IF_AVAILABLE`.

[PR #7125](https://github.com/InsightSoftwareConsortium/ITK/pull/7125) · [`970c3fa976e`](https://github.com/InsightSoftwareConsortium/ITK/commit/970c3fa976e)

Record: [`20261106_RequireSse2On32BitX86.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20261106_RequireSse2On32BitX86.yaml)

:::::

### ITK 6.0b01

(itk6-PreferAnatomicalOrientation)=
#### Prefer itk::AnatomicalOrientation over itk::SpatialOrientation

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx` {bdg-primary-line}`python`

The enumeration defined in `itk::SpatialOrientation`, including `itk::SpatialOrientation::CoordinateTerms`,
`itk::SpatialOrientation::CoordinateMajornessTerms`, and `itk::SpatialOrientation::ValidCoordinateOrientations` may be
deprecated in the future. These terms described an orientation of an axis in a coordinate system, by the "from" or
"negative" direction. For example `CoordinateTerms::ITK_COORDINATE_Right` describes an axis moving from the "Right" to
the "Left" side of the body.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

This is the opposite of the DICOM `Patient Orientation (0020,0020)` tag.

The `itk::AnatomicalOrientation` class now represents the anatomical orientation. The class can provide a representation
of itself as an unambiguous enumeration, string and a matrix. It provides both a `PositiveEnum` and a `NegativeEnum` for
three letter descriptions of the anatomical orientation, which is ambiguous.

The recommended unambiguous way to define an anatomical orientation is the following:

```cpp
itk::AnatomicalOrientation(itk::AnatomicalOrientation::CoordinateEnum::RightToLeft,
                           itk::AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior,
                           itk::AnatomicalOrientation::CoordinateEnum::InferiorToSuperior);
```

The `itk::SpatialOrientation::ValidCoordinateOrientations` enumerations can be explicitly or implicitly converted to the
new `AnatomicalOrientation` object:

```cpp
itk::AnatomicalOrientation orientation = itk::SpatialOrientation::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI;
orientation = itk::AnatomicalOrientation(itk::SpatialOrientation::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP;)
```

Implicit conversion is not available in ITK Python.  An error messaging like the following can occur:
```text
TypeError: in method 'itkOrientImageFilterID3ID3_SetDesiredCoordinateOrientation', argument 2 of type 'itkAnatomicalOrientation'
```

Explicitly converting to `AnatomicalOrientation` is required in ITK Python:
```python
reoriented_image = itk.orient_image_filter(
    image,
    use_image_direction=True,
    desired_coordinate_orientation=itk.AnatomicalOrientation(
        itk.SpatialOrientationEnums.ValidCoordinateOrientations_ITK_COORDINATE_ORIENTATION_RAS
    ),
)
```

[PR #5382](https://github.com/InsightSoftwareConsortium/ITK/pull/5382) · [`3c934669de9`](https://github.com/InsightSoftwareConsortium/ITK/commit/3c934669de9)

Record: [`20250617_PreferAnatomicalOrientation.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20250617_PreferAnatomicalOrientation.yaml)

:::::

(itk6-ItkVnlInstantiationRemoved)=
#### ITKVNLInstantiation library is removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cmake`

The usage of ITKVNLInstantiation library should be replaced directly with the
ITKVNL module. The ITKVNLInstantiation library was an empty library used for
compatibility and provided transitive linking to ITKVNL.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #5286](https://github.com/InsightSoftwareConsortium/ITK/pull/5286) · [`db104239cf8`](https://github.com/InsightSoftwareConsortium/ITK/commit/db104239cf8)

Record: [`20250617_ItkVnlInstantiationRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20250617_ItkVnlInstantiationRemoved.yaml)

:::::

### ITK 6.0a02

(itk6-CoordRepTypeToCoordinateType)=
#### Coord Rep Type To Coordinate Type

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Class changes**

For the sake of code readability, a new `CoordinateType` alias is added for
each nested `CoordRepType` alias. The old `CoordRepType` aliases will still be
available with ITK 6.0, but it is recommended to use `CoordinateType` instead.
The `CoordRepType` aliases will be removed when `ITK_FUTURE_LEGACY_REMOVE` is
enabled. Similarly, `InputCoordinateType`, `OutputCoordinateType`, and
`ImagePointCoordinateType` replace `InputCoordRepType`, `OutputCoordRepType`,
and `ImagePointCoordRepType`, respectively.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #5006](https://github.com/InsightSoftwareConsortium/ITK/pull/5006) · [`a68b8f67dae`](https://github.com/InsightSoftwareConsortium/ITK/commit/a68b8f67dae) · Old form available until `ITK_FUTURE_LEGACY_REMOVE` is set

Record: [`20250107_CoordRepTypeToCoordinateType.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20250107_CoordRepTypeToCoordinateType.yaml)

:::::

### ITK 6.0a01

High impact, described under Most impactful changes:

- [Remove support for ITKv5](#itk6-RemoveDeprecatedModule)

(itk6-PreferStandardCxxOverItkMacros)=
#### Prefer standard CXX language features rather than ITK macros

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

Replace ITK aliases (left column) with CXX standard feature (right column)

:::::{dropdown} Details and examples
:class-container: sd-mb-4

| Old | New |
| --- | --- |
| `ITK_FALLTHROUGH` | `[[fallthrough]]` |
| `ITK_DELETE_FUNCTION` | `= delete` |
| `ITK_CONSTEXPR_FUNC` | `constexpr` |
| `ITK_CONSTEXPR_VAR` | `constexpr` |
| `ITK_ALIGNAS(X)` | `alignas(X)` |
| `ITK_ALIGNOF(X)` | `alignof(X)` |
| `ITK_DEPRECATED` | `[[deprecated]]` |
| `ITK_DEPRECATED_MSG(MSG)` | `[[deprecated(MSG)]]` |
| `ITK_CONSTEXPR` | `constexpr` |
| `ITK_DELETED_FUNCTION` | `= delete` |
| `ITK_EXTERN_TEMPLATE` | `extern` |
| `ITK_FINAL` | `final` |
| `ITK_NOEXCEPT` | `noexcept` |
| `ITK_NOEXCEPT_EXPR(X)` | `noexcept(X)` |
| `ITK_NULLPTR` | `nullptr` |
| `ITK_OVERRIDE` | `override` |
| `ITK_STATIC_ASSERT(X)` | `static_assert(X, #X)` |
| `ITK_STATIC_ASSERT_MSG(X, MSG)` | `static_assert(X, MSG)` |
| `ITK_THREAD_LOCAL` | `thread_local` |

[PR #4737](https://github.com/InsightSoftwareConsortium/ITK/pull/4737) · [`f65606905b7`](https://github.com/InsightSoftwareConsortium/ITK/commit/f65606905b7)

Record: [`20240621_PreferStandardCxxOverItkMacros.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20240621_PreferStandardCxxOverItkMacros.yaml)

:::::

(itk6-PointSetCloneDeepCopy)=
#### Point Set Clone Deep Copy

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx`

**Class changes**

The `Clone()` member function of `itk::PointSet` now does a "deep copy" of its
data, creating a new instance that has a copy of the points, the point data and
the region information properties of the original PointSet object. With previous
ITK versions, `PointSet::Clone()` did not copy any data. (It previously just
created a default-constructed PointSet object, like `PointSet::CreateAnother()`
does.)

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #4925](https://github.com/InsightSoftwareConsortium/ITK/pull/4925) · [`54ddd09eb0d`](https://github.com/InsightSoftwareConsortium/ITK/commit/54ddd09eb0d)

Record: [`20241106_PointSetCloneDeepCopy.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20241106_PointSetCloneDeepCopy.yaml)

:::::

(itk6-RemoveItkV4Compatibility)=
#### Remove support for ITKv4 interfaces

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cmake` {bdg-primary-line}`cxx`

`ITKV4_COMPATIBILITY` is no longer a supported option.  Previously this option
was off by default.  Previously when enabled, the ITK API was modified to
provide support for ITKV4 functions.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #4715](https://github.com/InsightSoftwareConsortium/ITK/pull/4715) · [`da532bcbfff`](https://github.com/InsightSoftwareConsortium/ITK/commit/da532bcbfff)

Record: [`20240611_RemoveItkV4Compatibility.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20240611_RemoveItkV4Compatibility.yaml)

:::::

(itk6-RemoveItkV5ConstMacro)=
#### Require modern C++ language feature use

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-info-line}`CompilerEnvironment` {bdg-primary-line}`cxx`

Many backward compatible/ forward enabling compiler features are now required to be used.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**What you need to do**

1. Replace `ITKv5_CONST` with `const`

[PR #4704](https://github.com/InsightSoftwareConsortium/ITK/pull/4704) · [`58e84617ff3`](https://github.com/InsightSoftwareConsortium/ITK/commit/58e84617ff3)

Record: [`20240613_RemoveItkV5ConstMacro.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20240613_RemoveItkV5ConstMacro.yaml)

:::::

(itk6-RemoveItkV5MigrationScripts)=
#### Removed ITKv5 migration/maintenance scripts

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved`

The following scripts used for migrating to ITKv5 were removed from the ITKv6.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

```sh
  CheckForOutdatedDefines.sh
  EnumPrintFunction.py
  Move_DISALLOW_COPY_to_public_section.cpp
  ReplaceITK_NULLPTRMacroNames.sh
  ReplaceITK_OVERRIDEMacroNames.sh
  ReplaceitkGetObjectMacro.sh
  UpdateAllC++Headers.sh
  UseNativeC++Syntax.sh
  misc-unused-parameters.sh
  modernize-loop-convert.sh
  modernize-pass-by-value.sh
  modernize-return-braced-init-list.sh
  modernize-use-auto.sh
  modernize-use-bool-literals.sh
  modernize-use-default-member-init.sh
  modernize-use-emplace.sh
  modernize-use-equals-default.sh
  modernize-use-equals-delete.sh
  modernize-use-nullptr.sh
  modernize-use-override.sh
  performance-general.sh
  prefer-type-alias-over-typedef.sh
  prefer_constexpr_for_const_literals.sh
  readability-container-size-empty.sh
  replaceClassWithTypename.py
  replace_itkStaticConstMacro.sh
  replace_vnl_math_XXX.sh
```

**Accessing outdated ITKv5 migration scripts**

```
git worktree add .../ITKv5.4 v5.4.0
ls ../ITKv5/Utilities/ITKv5Preparation
```

[PR #4744](https://github.com/InsightSoftwareConsortium/ITK/pull/4744) · [`d31666271a0`](https://github.com/InsightSoftwareConsortium/ITK/commit/d31666271a0)

Record: [`20240622_RemoveItkV5MigrationScripts.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk6/20240622_RemoveItkV5MigrationScripts.yaml)

:::::
