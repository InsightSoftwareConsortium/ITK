ITK v6 Migration Guide
======================

This guide documents the changes required to migrate a code base
which uses ITK v5 to use ITK v6. The migration guide for transition
from v4 to v5 can be found [here](./itk_5_migration_guide.md).

Require modern C++ language feature use
---------------------------------------
Many backward compatible/ forward enabling compiler features are now required to be used.

Replace `ITKv5_CONST` with `const`

Remove support for ITKv4 interfaces
-----------------------------------

`ITKV4_COMPATIBILITY` is no longer a supported option.  Previously this option
was off by default.  Previously when enabled, the ITK API was modified to
provide support for ITKV4 functions.

Remove support for ITKv5
-----------------------------------

All contents of the Deprecated module were removed. This includes TreeContainer
and related classes; atomic primitives, mutexes and related classes which are
now part of C++ standard; specialized Vector filters - specialized versions are
no longer needed, as regular filters can work with vector images. For details, see
[ITKv5 Migration Guide](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/itk_5_migration_guide.md).

Prefer standard CXX language features rather than ITK macros
-------------------------------------------------------------

Replace ITK aliases (left column) with CXX standard feature (right column)

```txt
ITK_FALLTHROUGH                   [[fallthrough]]
ITK_DELETE_FUNCTION               = delete
ITK_CONSTEXPR_FUNC                constexpr
ITK_CONSTEXPR_VAR                 constexpr

ITK_ALIGNAS(X)                    alignas(X)
ITK_ALIGNOF(X)                    alignof(X)
ITK_DEPRECATED                    [[deprecated]]
ITK_DEPRECATED_MSG(MSG)           [[deprecated(MSG)]]
ITK_CONSTEXPR                     constexpr
ITK_DELETED_FUNCTION              = delete
ITK_EXTERN_TEMPLATE               extern
ITK_FINAL                         final
ITK_NOEXCEPT                      noexcept
ITK_NOEXCEPT_EXPR(X)              noexcept(X)
ITK_NULLPTR                       nullptr
ITK_OVERRIDE                      override
ITK_STATIC_ASSERT(X)              static_assert(X, #X)
ITK_STATIC_ASSERT_MSG(X, MSG)     static_assert(X, MSG)
ITK_THREAD_LOCAL                  thread_local
```

Removed ITKv5 migration/maintenance scripts
-----------------------

The following scripts used for migrating to ITKv5 were removed from the ITKv6.

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

Accessing outdated ITKv5 migration scripts
------------------------------------------

```
git worktree add .../ITKv5.4 v5.4.0
ls ../ITKv5/Utilities/ITKv5Preparation
```

Class changes
-------------

The `Clone()` member function of `itk::PointSet` now does a "deep copy" of its
data, creating a new instance that has a copy of the points, the point data and
the region information properties of the original PointSet object. With previous
ITK versions, `PointSet::Clone()` did not copy any data. (It previously just
created a default-constructed PointSet object, like `PointSet::CreateAnother()`
does.)

For the sake of code readability, a new `CoordinateType` alias is added for
each nested `CoordRepType` alias. The old `CoordRepType` aliases will still be
available with ITK 6.0, but it is recommended to use `CoordinateType` instead.
The `CoordRepType` aliases will be removed when `ITK_FUTURE_LEGACY_REMOVE` is
enabled. Similarly, `InputCoordinateType`, `OutputCoordinateType`, and
`ImagePointCoordinateType` replace `InputCoordRepType`, `OutputCoordRepType`,
and `ImagePointCoordRepType`, respectively.


ITKVNLInstantiation library is removed
--------------------------------------

The usage of ITKVNLInstantiation library should be replaced directly with the
ITKVNL module. The ITKVNLInstantiation library was an empty library used for
compatibility and provided transitive linking to ITKVNL.


Prefer itk::AnatomicalOrientation over itk::SpatialOrientation
------------------------------------------------

The enumeration defined in `itk::SpatialOrientation`, including `itk::SpatialOrientation::CoordinateTerms`,
`itk::SpatialOrientation::CoordinateMajornessTerms`, and `itk::SpatialOrientation::ValidCoordinateOrientations` may be
deprecated in the future. These terms described an orientation of an axis in a coordinate system, by the "from" or
"negative" direction. For example `CoordinateTerms::ITK_COORDINATE_Right` describes an axis moving from the "Right" to
the "Left" side of the body.

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

Remove support for Python wrapped `long double` types
-----------------------------------------------------

The swig wrapping of `long double` types into python
resulted in implicit type conversions to `double`,
which results in silent loss of precision.

There has never been an option for "ITK_WRAP_long_double"
configuration, and manually wrapped functions in vnl for
for long double were never needed by ITK based functions.

Given the undefined behavior of wrapping long double types
with swig, and given that there is no use case for long double
support directly from the wrapped ITK API, these manual vnl
wrappings could not be reached from ITK interfaces.

The handful of manually wrapped long double functions were
removed from python wrapping.


Legacy GoogleTest Target Names Removed
--------------------------------------

ITK 6 now uses the standard CMake `FindGTest` target names for GoogleTest libraries, aligning with upstream project and CMake.

### Target Name Changes

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

### Rationale

These names were deprecated in CMake 3.20 and removed in CMake 4.1.0. Additionally, the GoogleTest project itself uses the lowercase target names (`GTest::gtest` and `GTest::gtest_main`), meaning the old ITK-specific aliases were not compatible when using GoogleTest directly from its upstream repository. ITK 6 adopts the standard lowercase target names to ensure compatibility with modern CMake versions, the GoogleTest project, and consistency with other projects.

Python Global Interpreter Lock (GIL) Release
---------------------------------------------

ITK now releases the Python Global Interpreter Lock (GIL) during C++ operations by default,
allowing for true multi-threaded execution of ITK operations from Python. This enables
parallel filter invocation when using ITK in parallel computing frameworks like Dask, Ray, or
Python's standard `threading` module.

### Key Changes

**New CMake Option:**
- `ITK_PYTHON_RELEASE_GIL` (default: `ON`) - Controls whether the GIL is released during ITK operations
- When enabled, the `-threads` flag is passed to SWIG to generate thread-safe wrappers

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
    # GIL is released during ITK operations
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

**Note:** ITK callbacks and event monitoring may be affected by GIL release. If you encounter
issues with callbacks, you can disable GIL release by setting `-DITK_PYTHON_RELEASE_GIL=OFF`
when building ITK.

Modern CMake Interface Libraries
---------------------------------

ITK 6 introduces modern CMake interface libraries for all ITK modules, providing improved dependency management and
simplified linking. Each module now exports a namespaced interface library following the pattern `ITK::{ModuleName}Module`.

### Key Changes

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

#### Application Configuration

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


### Migration for ITK Modules

Remote modules should be updated to use modern CMake patterns for better integration with ITK 6.

#### Using itk_module_add_library

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

#### Using Interface Libraries for Executables

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

#### Backward Compatibility

For backward compatibility, non-namespaced aliases are created with deprecation warnings. However, new code should use the namespaced `ITK::` targets exclusively.
