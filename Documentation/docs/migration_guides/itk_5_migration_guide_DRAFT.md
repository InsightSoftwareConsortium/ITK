<!--
  ##################################################################
  ##  AUTO-GENERATED FILE. DO NOT EDIT.                            ##
  ##                                                              ##
  ##  Generated from Documentation/docs/migration_guides/records/itk5/
  ##  by Utilities/Maintenance/MigrationRecords.py generate.      ##
  ##  Edit or add a record instead; see                           ##
  ##  Documentation/AI/migration-records.md.                      ##
  ##################################################################
-->

# ITK 5 Migration Guide

Changes that downstream code may need to act on when moving to ITK 5.
The first part lists the changes that affect many projects and what to do
about each; the second lists every change by release, newest first.
This page is generated from the [migration records](https://github.com/InsightSoftwareConsortium/ITK/tree/main/Documentation/docs/migration_guides/records/itk5);
to correct or add an entry, edit a record.

## Most impactful changes

(itk5-StronglyTypedEnumerations)=
### Strongly Typed Enumerations

{bdg-danger}`high` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx` {bdg-primary-line}`python` {bdg-primary-line}`cmake` {bdg-primary-line}`wrapping`

*First released in ITK 5.1rc02.*

In ITK 5, enumerations are strongly typed, declared with `enum class`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

A best
practice in modern C++, strongly typed enum's [offer the following improvements](https://www.modernescpp.com/index.php/strongly-typed-enums):

- The enumerators can only be accessed in the scope of the enumeration.
- The enumerators don't implicitly convert to `int`.
- The enumerators aren't imported in the enclosing scope.
- The type of the enumerators is by default `int`. Therefore, you can forward the enumeration.

In order to preserve as much backwards compatibility as possible, appropriate
scoping, provide clean, readable code, facilitate wrapping in languages such
as Python, and enable printing enum values to `std::ostream` with
`operator<<`, and support templates, enums that we previously declared as:

```cpp
// itkClassName.h
namespace itk
{

class ClassName
{
public:
  enum Choices
  {
    One,
    Two,
    Three
  };
};

} // namespace itk
```

are now declared as:

```cpp
// itkClassName.h
namespace itk
{

class ClassNameEnums
{
public:
  enum class Choices : uint8_t
  {
    One,
    Two,
    Three
  };
};
extern ITKModuleName_EXPORT std::ostream &
operator<<(std::ostream & out, const ClassNameEnums::Choices value);

class ClassName
{
public:
  using ChoicesEnum = ClassNameEnums::Choices;
#if !defined(ITK_LEGACY_REMOVE)
  using Choices = ChoicesEnum;
  static constexpr Choices One = ChoicesEnum::One;
  static constexpr Choices Two = ChoicesEnum::Two;
  static constexpr Choices Three = ChoicesEnum::Three;
#endif
};

} // namespace itk

// itkClassName.cxx
namespace itk
{

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ClassNameEnums::Choices value)
{
  return out << [value] {
    switch (value)
    {
      case ClassNameEnums::Choices::One:
        return "itk::ClassNameEnums::Choices::One";
      case ClassNameEnums::Choices::Two:
        return "itk::ClassNameEnums::Choices::Two";
      case ClassNameEnums::Choices::Three:
        return "itk::ClassNameEnums::Choices::Three";
      default:
        return "INVALID VALUE FOR itk::ClassNameEnums::Choices";
    }
  }();
}
```

for a class called `ClassName` and an enum called `Choices`.

Wrapping is configured with `itk_wrap_simple_class("itk::ClassNameEnums")`,
which results in access to the enums in Python as
`itk.ClassNameEnums.Choices_One`, `itk.ClassNameEnums.Choices_Two`, and
`itk.ClassNameEnums.Choices_Three`.

While backwards compatible type aliases and `static constexpr` backwards
compatible values were introduced, the names of the enums were generally made
more consistent, and existing code should migrate to new enum names. Since
strongly typed enums do not implicitly cast to int's like classic enums,
explicit `static_cast`'s may be required in migrated code.

Common enums used in classes across the toolkit are now available in the
`itk::CommonEnums` class.

As a consequence of improved enum support, a new library binary to provide
enum class `operator<<` definitions is required for many ITK modules that were
previously header-only. As a result, remote modules may encounter build-time
link errors such as:

```
itk::RecursiveGaussianImageFilter<itk::Image<float, 3u>, itk::Image<float, 3u> >::PrintSelf(std::ostream&, itk::Indent) const: error: undefined reference to 'itk::operator<<(std::ostream&, itk::RecursiveGaussianImageFilterEnums::GaussianOrder)' `itk::CommonEnums` class.
```

To resolve these errors, update the remote module's dependency specification in
`itk-module.cmake`. List the dependent module under `DEPENDS` instead of
`COMPILE_DEPENDS`; this will link to the new dependent library.

**Links:** <https://www.modernescpp.com/index.php/strongly-typed-enums>, <https://github.com/InsightSoftwareConsortium/ITK/pull/1782>

[PR #1471](https://github.com/InsightSoftwareConsortium/ITK/pull/1471) · [`0dd31a62f85`](https://github.com/InsightSoftwareConsortium/ITK/commit/0dd31a62f85) · Old form available until `ITK_LEGACY_REMOVE` is set

Record: [`20220214_StronglyTypedEnumerations.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220214_StronglyTypedEnumerations.yaml)

:::::

(itk5-NumberOfThreadsToWorkUnits)=
### Number Of Threads To Work Units

{bdg-danger}`high` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

*First released in ITK 5.0b01.*

**Multithreading refactored**

`Get/SetGlobalMaximumNumberOfThreads()`, and `GlobalDefaultNumberOfThreads()`
now reside in `itk::MultiThreaderBase`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

With a warning, they are still
available in `itk::PlatformMultiThreader`.
`GetGlobalDefaultNumberOfThreadsByPlatform()` has also been moved from
`itk::ThreadPool` to `itk::MultiThreaderBase`.  In image filters and other
descendents of `itk::ProcessObject`, method `SetNumberOfThreads` has been
renamed into `SetNumberOfWorkUnits`.  For `itk::MultiThreaderBase` and
descendents, `SetNumberOfThreads` has been split into
`SetMaximumNumberOfThreads` and `SetNumberOfWorkUnits`.  Load balancing is
possible when `NumberOfWorkUnits` is greater than the number of threads. The
common case of `innerFilter->SetNumberOfThreads(1);` should be replaced by
`innerFilter->SetNumberOfWorkUnits(1);`. Generally, in most places where
threads were being manipulated before, work units should be accessed or
changed now.

[`ce15429a230`](https://github.com/InsightSoftwareConsortium/ITK/commit/ce15429a230)

Record: [`20190104_NumberOfThreadsToWorkUnits.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190104_NumberOfThreadsToWorkUnits.yaml)

:::::

(itk5-DynamicThreadedGenerateData)=
### Dynamic Threaded Generate Data

{bdg-danger}`high` {bdg-info-line}`DefaultChanged` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

*First released in ITK 5.0a02.*

**Multithreading refactored**

For filter multi-threading, a new signature has been introduced:
`void DynamicThreadedGenerateData( const OutputRegionType& threadRegion )`.
By default, this new signature is invoked instead of the classic
`void ThreadedGenerateData( const OutputRegionType& threadRegion, ThreadIdType threadId )`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

To temporarily obtain the old behavior (classic signature invoked by default),
set `ITKV4_COMPATIBILITY` to `ON` in ITK's CMake configuration.
To permanently have your filter use the classic threading model,
invoke `this->DynamicMultiThreadingOff();` in the filter constructor.
That is required if any of the following is true:
 * Your filter needs a constant number of threads (known in advance)
 * Your filter uses `threadId` parameter in `ThreadedGenerateData()`
 * Your filter uses a custom region splitting method

Additionally, replace `itk::MultiThreader` by `itk::PlatformMultiThreader`
if any of the following is true:
 * Your filter uses cross-thread synchronization e.g. `itk::Barrier`
 * Your filter uses `MultipleMethodExecute()`
 * Your filter uses `SpawnThread`/`TerminateThread`

It is strongly advised to not explicitly use `itk::PlatformMultiThreader`.
`SpawnThread`/`TerminateThread` and `MultipleMethodExecute` can be
replaced by C++11 `std::thread`. Code in the example below shows
how to remove dependence on barrier by using ParallelizeImageRegion.

- Pattern for Multiple Parallel Operations:
```cpp
ThreadedGenerateData()
{
  // code1 (parallel)
  myBarrier->Wait();
  if (threadId == 0)
  {
    // code2 single-threaded
  }
  // code3 (parallel)
}
```

after refactoring to not use barrier:
```cpp
GenerateData() // Not Threaded
{
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();
  ParallelizeImageRegion(code1 as lambda)
  // code2 single-threaded
  ParallelizeImageRegion(code3 as lambda)
  this->AfterThreadedGenerateData();
}
```

- Pattern for Parallel Counting:
Previously parallel counting was often storing per-thread counts in an `itk::Array`
and aggregating the result in a filter's `AfterThreadedGenerateData()`.
With C++11, you might want to instead use `std::atomic`.
An external module example that demonstrates this can be found in
[this commit](https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry/pull/32/commits/a8014c186ac53837362a0cb9db46ae224b8e9584).

Before, using `itk::Array`:
```cpp
// Members:
Array<SizeValueType> m_NumVoxelsInsideMask;
BeforeThreadedGenerateData()
{
  // Resize the thread temporaries
  m_NumVoxelsInsideMask.SetSize(this->GetNumberOfThreads());
  m_NumVoxelsInsideMask.Fill(0);
}

ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId)
{
  // Do algorithm per threadId
  // Store the results per thread at the end
  m_NumVoxelsInsideMask[threadId] = numVoxelsForThisRegion;
}

AfterThreadedGenerateData()
{
  // Retrieve and sum all the results per thread.
  ThreadIdType  numberOfThreads = this->GetNumberOfThreads();
  SizeValueType numVoxelsInsideMask = 0;
  for (unsigned int i = 0; i < numberOfThreads; ++i)
  {
    numVoxelsInsideMask += m_NumVoxelsInsideMask[i];
  }
}
```
After, using `std::atomic`:
```cpp
// Members:
std::atomic<SizeValueType> m_NumVoxelsInsideMask;
BeforeThreadedGenerateData()
{
  // Initialize atomics
  m_NumVoxelsInsideMask.store(0);
}

DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  // Do algorithm without handling threadId
  m_NumVoxelsInsideMask.fetch_add(numVoxelsForThisRegion, std::memory_order_relaxed);
}

AfterThreadedGenerateData()
{
  // Get the value from the atomic
  SizeValueType numVoxelsInsideMask = m_NumVoxelsInsideMask.load();
}
```

To transition to the new threading model, it is usually enough to rename
`ThreadedGenerateData` into `DynamicThreadedGenerateData`, remove the
`threadId` parameter, and remove progress reporting which uses `threadId`.
Progress is being reported by multi-threaders on behalf of filters which
use `DynamicThreadedGenerateData` signature.

If your class needs to also work with legacy code where
`ITKV4_COMPATIBILITY` is enabled, invoke
`this->DynamicMultiThreadingOn();` in the filter constructor. An example of
an external module that transitioned to the new threading model can be found in
[this commit](https://github.com/InsightSoftwareConsortium/ITKTextureFeatures/commit/f794baa7546f9bb8b7d89ae3a083c9a432d55df0).

**Links:** <https://github.com/InsightSoftwareConsortium/ITKTextureFeatures/commit/f794baa7546f9bb8b7d89ae3a083c9a432d55df0>, <https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry/pull/32/commits/a8014c186ac53837362a0cb9db46ae224b8e9584>

[`e0899a1ee00`](https://github.com/InsightSoftwareConsortium/ITK/commit/e0899a1ee00)

Record: [`20220214_DynamicThreadedGenerateData.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220214_DynamicThreadedGenerateData.yaml)

:::::

(itk5-RequireCxx11Compiler)=
### Require Cxx 11 Compiler

{bdg-danger}`high` {bdg-info-line}`CompilerEnvironment` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

*First released in ITK 5.0a01.*

**C++11**

A major improvement in ITK 5 is to fully adopt C++11.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Prior to ITK 5, a limited subset of C++11 functionalities were available in ITK
through back-ports and macros. This functionality was enabled when ITK was built with the C++11 standard enabled.
ITK 5.0.0 deprecates or removes these macros (e.g. `ITK_NULLPTR`, `ITK_DELETED_FUNCTION`,
`ITK_NOEXCEPT`, `ITK_CONSTEXPR`) and directly uses C++11 keywords such as
[delete](https://github.com/InsightSoftwareConsortium/ITK/commit/02128abbd0bf790deadc86a28c62c4a25e23518b),
[constexpr](https://github.com/InsightSoftwareConsortium/ITK/commit/b8e41d0d1652a8f6ddb84328faef67b207e77430),
[nullptr](https://github.com/InsightSoftwareConsortium/ITK/commit/3c6372b80ac2900e2e197899989c4cd151f1695f),
[override](https://github.com/InsightSoftwareConsortium/ITK/commit/3ceacc0ad4ec699b094d96e23d33f9467c2a63c6),
[noexcept](https://github.com/InsightSoftwareConsortium/ITK/commit/af4f65519abb32b59cebb2d72f0186a96efe3b4e).
The keywords [auto](https://github.com/InsightSoftwareConsortium/ITK/commit/de713e7ac52f7815a35754de885795ff0a1c4981)
and  `using` [[1](https://github.com/InsightSoftwareConsortium/ITK/commit/66e5d6b3bcc28f1a85b702086b6cedc8cab6723b),
[2](https://github.com/InsightSoftwareConsortium/ITK/commit/f21c1d27025575167ea9194214aa5bf17a0a5495)]
as well as [range-based loops](https://github.com/InsightSoftwareConsortium/ITK/commit/48daed0751df99bcb5fd1077e78ceb1b47546ccc)
are also now used in ITK. As a consequence, due to limitations in C++11 support
Visual Studio 2013 (MSVC 12.0) and other older C++ complilers cannot be used to build ITK from 5.0 and forward.

[`babb6779b67`](https://github.com/InsightSoftwareConsortium/ITK/commit/babb6779b67)

Record: [`20180905_RequireCxx11Compiler.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20180905_RequireCxx11Compiler.yaml)

:::::

(itk5-LegacyCodeRemoved)=
### Legacy code removed

{bdg-danger}`high` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

*First released in ITK 5.0a01.*

All code which was marked as legacy in ITK 4.13 has been removed.
External code which previously required
`ITK_LEGACY_REMOVE` CMake option to be `OFF` in order to build will now fail
to compile.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Before starting migration to v5 (as explained in this guide),
migration to v4 should be finished. Dependent code should build and pass tests
with `ITK_LEGACY_REMOVE` turned `ON` when compiling against
[ITK v4.13](https://github.com/InsightSoftwareConsortium/ITK/releases/tag/v4.13.1).

Code which was marked as `ITK_FUTURE_LEGACY_REMOVE` has been now
re-flagged as `ITK_LEGACY_REMOVE`. There have been some other
deprecations and API changes. The new behavior is activated by setting
`ITK_LEGACY_REMOVE` to `ON`. By default, compatibility with v4 is retained
(`ITK_LEGACY_REMOVE=OFF`).

The first step is to get the external code building against ITK compiled with
`ITKV4_COMPATIBILITY` set to `ON`. Once that is accomplished, turn this
option `OFF`. It is `OFF` by default.

Once external code builds successfully with the default configuration
(`ITKV4_COMPATIBILITY` and `ITK_LEGACY_REMOVE` both `OFF`),
`ITK_LEGACY_REMOVE` should be set to `ON`.  Possible additional build errors
and/or warnings should be addressed.  Once dependent code builds and passes
tests without legacy options, migration to v5 is complete.

**Links:** <https://itk.org/migrationv4/>

[`01580b37063`](https://github.com/InsightSoftwareConsortium/ITK/commit/01580b37063) · Old form available until `ITK_LEGACY_REMOVE` is set

Record: [`20181024_LegacyCodeRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20181024_LegacyCodeRemoved.yaml)

:::::

## All changes

| Impact | Change | Release |
| --- | --- | --- |
| medium | [ITKv6 will prefer itk::AnatomicalOrientation over itk::SpatialOrientation](#itk5-AnatomicalOrientationBackport) | 5.4.5 |
| low | [Numeric Traits Complex Float Only](#itk5-NumericTraitsComplexFloatOnly) | 5.4rc01 |
| medium | [Spatial Orientation Enums](#itk5-SpatialOrientationEnums) | 5.3rc04 |
| low | [Explicit Scalar Fill Constructors](#itk5-ExplicitScalarFillConstructors) | 5.3rc04 |
| low | [Orientation Adapter Base Deprecated](#itk5-OrientationAdapterBaseDeprecated) | 5.3rc04 |
| low | [Mpl Type Traits Replaced By Std](#itk5-MplTypeTraitsReplacedByStd) | 5.3rc02 |
| low | [BSpline Number Of Weights Constexpr](#itk5-BSplineNumberOfWeightsConstexpr) | 5.3rc01 |
| high | [Strongly Typed Enumerations](#itk5-StronglyTypedEnumerations) | 5.1rc02 |
| medium | [Nifti Analyze Flavors](#itk5-NiftiAnalyzeFlavors) | 5.1rc02 |
| low | [Tree Container Deprecated](#itk5-TreeContainerDeprecated) | 5.1rc02 |
| medium | [Consolidated Vector Filter](#itk5-ConsolidatedVectorFilters) | 5.1rc01 |
| medium | [Statistics Filters No Image Output](#itk5-StatisticsFiltersNoImageOutput) | 5.0.0 |
| low | [Fixed Array Reverse Iterators](#itk5-FixedArrayReverseIterators) | 5.0.0 |
| low | [Image Transformer Deprecated](#itk5-ImageTransformerDeprecated) | 5.0.0 |
| low | [Interpolator Get Radius Required](#itk5-InterpolatorGetRadiusRequired) | 5.0.0 |
| low | [Bounding Box Compute Corners](#itk5-BoundingBoxComputeCorners) | 5.0.0 |
| medium | [Barrier And Vector Filters Deprecated](#itk5-BarrierAndVectorFiltersDeprecated) | 5.0rc02 |
| medium | [Spatial Objects Refactoring](#itk5-SpatialObjectRefactoring) | 5.0rc02 |
| medium | [Watershed Results Change](#itk5-WatershedResultsChange) | 5.0rc02 |
| low | [Gaussian Derivative Image Function Renames](#itk5-GaussianDerivativeImageFunctionRenames) | 5.0rc02 |
| low | [Itksys Hash Map Removed](#itk5-ItksysHashMapRemoved) | 5.0rc02 |
| low | [Python Extras Snake Case](#itk5-PythonExtrasSnakeCase) | 5.0rc01 |
| low | [Python Mesh Wrapping Simplified](#itk5-PythonMeshWrappingSimplified) | 5.0rc01 |
| medium | [Otsu Returns Bin Maximum](#itk5-OtsuReturnsBinMaximum) | 5.0b02 |
| medium | [Threading Symbols In Itk Namespace](#itk5-ThreadingSymbolsInItkNamespace) | 5.0b02 |
| medium | [Std Mutex And Atomic Replace Itk Classes](#itk5-StdMutexAndAtomicReplaceItkClasses) | 5.0b02 |
| low | [Minimum System Vxl 202](#itk5-MinimumSystemVxl202) | 5.0b02 |
| low | [Verify Preconditions Const](#itk5-VerifyPreconditionsConst) | 5.0b02 |
| high | [Number Of Threads To Work Units](#itk5-NumberOfThreadsToWorkUnits) | 5.0b01 |
| low | [Progress Reporter To Progress Transformer](#itk5-ProgressReporterToProgressTransformer) | 5.0b01 |
| low | [Hough Circles Radius Template Argument](#itk5-HoughCirclesRadiusTemplateArgument) | 5.0b01 |
| low | [Transform Jacobian Position Type](#itk5-TransformJacobianPositionType) | 5.0b01 |
| high | [Dynamic Threaded Generate Data](#itk5-DynamicThreadedGenerateData) | 5.0a02 |
| medium | [Modern CMake requirement](#itk5-MinimumCMake3102) | 5.0a02 |
| medium | [Multi Threader Class Hierarchy](#itk5-MultiThreaderClassHierarchy) | 5.0a02 |
| low | [Filter Watcher Removed](#itk5-FilterWatcherRemoved) | 5.0a02 |
| high | [Require Cxx 11 Compiler](#itk5-RequireCxx11Compiler) | 5.0a01 |
| high | [Legacy code removed](#itk5-LegacyCodeRemoved) | 5.0a01 |
| medium | [Smart Pointer Null Initialization](#itk5-SmartPointerNullInitialization) | 5.0a01 |
| low | [Updated style](#itk5-CodingStyleCxx11) | 5.0a01 |
| low | [Update scripts](#itk5-ItkV5PreparationScripts) | 5.0a01 |

### ITK 5.4.5

(itk5-AnatomicalOrientationBackport)=
#### ITKv6 will prefer itk::AnatomicalOrientation over itk::SpatialOrientation

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**Backported ITKv6 features**

As of v5.4.5 and later, the `itk::AnatomicalOrientation` API is added and supported to facilitate
conversion to the ITKv6 recommended paradigm.  The `itk::AnatomicalOrientation` is not available
in v5.4.4 and earlier releases.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

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

The Python wrapping of `itk::AnatomicalOrientation` is not supported in ITKv5 due to conflicts with `itk::SpatialOrientation`
wrapping.

[PR #5460](https://github.com/InsightSoftwareConsortium/ITK/pull/5460) · [`294937f135e`](https://github.com/InsightSoftwareConsortium/ITK/commit/294937f135e)

Record: [`20250710_AnatomicalOrientationBackport.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20250710_AnatomicalOrientationBackport.yaml)

:::::

### ITK 5.4rc01

(itk5-NumericTraitsComplexFloatOnly)=
#### Numeric Traits Complex Float Only

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

**Class changes**

`itk::NumericTraits` can no longer be instantiated with `std::complex<T>`, if T is not a floating point type.
The old behavior can be restored by turning off `ITK_LEGACY_REMOVE`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #3768](https://github.com/InsightSoftwareConsortium/ITK/pull/3768) · [`37e4c718057`](https://github.com/InsightSoftwareConsortium/ITK/commit/37e4c718057) · Old form available until `ITK_LEGACY_REMOVE` is set

Record: [`20221129_NumericTraitsComplexFloatOnly.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20221129_NumericTraitsComplexFloatOnly.yaml)

:::::

### ITK 5.3rc04

(itk5-SpatialOrientationEnums)=
#### Spatial Orientation Enums

{bdg-warning}`medium` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Class changes**

With ITK 5.3, SpatialOrientation was updated to a strongly typed enumeration (see below for details).
The namespece itself is only available in legacy mode.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Some classes were renamed:
* `itk::SpatialOrientation::CoordinateTerms` became `itk::SpatialOrientationEnums::CoordinateTerms`.
* `itk::SpatialOrientation::CoordinateMajornessTerms` became `itk::SpatialOrientationEnums::CoordinateMajornessTerms`.
* `itk::SpatialOrientation::ValidCoordinateOrientationFlags` became `itk::SpatialOrientationEnums::ValidCoordinateOrientations`.

Enumeration member names (`ITK_COORDINATE_UNKNOWN`, `ITK_COORDINATE_Right`, `ITK_COORDINATE_PrimaryMinor`, `ITK_COORDINATE_ORIENTATION_RIP` etc) are unchanged.

[PR #3180](https://github.com/InsightSoftwareConsortium/ITK/pull/3180) · [`182c018e7b2`](https://github.com/InsightSoftwareConsortium/ITK/commit/182c018e7b2) · Old form available until `ITK_LEGACY_REMOVE` is set

Record: [`20220214_SpatialOrientationEnums.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220214_SpatialOrientationEnums.yaml)

:::::

(itk5-ExplicitScalarFillConstructors)=
#### Explicit Scalar Fill Constructors

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**Class changes**

Implicit conversion of a single scalar value to a container (which would _fill_ the container by the
scalar value) is discouraged. With ITK 5.3, when having `ITK_LEGACY_REMOVE=ON`, the constructors of
`Point`, `RGBPixel`, `RGBAPixel`, and `Vector` that accept a single scalar value as argument are
declared `explicit`. ITK 5.3 has included a preferable alternative to these constructors:
`itk::MakeFilled<ContainerType>(value)`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/3255>

[PR #3237](https://github.com/InsightSoftwareConsortium/ITK/pull/3237) · [`88258344063`](https://github.com/InsightSoftwareConsortium/ITK/commit/88258344063) · Deprecated in 5.3 · Old form available until `ITK_LEGACY_REMOVE` is set

Record: [`20220309_ExplicitScalarFillConstructors.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220309_ExplicitScalarFillConstructors.yaml)

:::::

(itk5-OrientationAdapterBaseDeprecated)=
#### Orientation Adapter Base Deprecated

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**Class changes**

Since ITK 5.3, `OrientationAdapterBase` is deprecated. In ITKv6 it will be available in `ITKDeprecated` module.
SpatialOrientationAdapter no longer inherits from it.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #3378](https://github.com/InsightSoftwareConsortium/ITK/pull/3378) · [`c294138d23a`](https://github.com/InsightSoftwareConsortium/ITK/commit/c294138d23a) · Deprecated in 5.3

Record: [`20220418_OrientationAdapterBaseDeprecated.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220418_OrientationAdapterBaseDeprecated.yaml)

:::::

### ITK 5.3rc02

(itk5-MplTypeTraitsReplacedByStd)=
#### Mpl Type Traits Replaced By Std

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**C++11**

  * `mpl::EnableIf<X>::Type` with `std::enable_if_t<X>`, and `#include "itkEnableIf.h"` with `#include <type_traits>`.
  * `mpl::IsSame<X, Y>::Value` with `std::is_same<X, Y>::value`, and `#include "itkIsSame.h"` with `#include <type_traits>`.
  * `mpl::IsBaseOf<X, Y>::Value` with `std::is_base_of<X, Y>::value`, and `#include "itkIsBaseOf.h"` with `#include <type_traits>`.
  * `mpl::IsConvertible<X, Y>::Value` with `std::is_convertible<X, Y>::value`, and `#include "itkIsBaseOf.h"` with `#include <type_traits>`.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/commit/342845e644a>, <https://github.com/InsightSoftwareConsortium/ITK/commit/d458298bdd0>, <https://github.com/InsightSoftwareConsortium/ITK/commit/2f29c8d2d09>

[PR #2713](https://github.com/InsightSoftwareConsortium/ITK/pull/2713) · [`9f3a6243bf8`](https://github.com/InsightSoftwareConsortium/ITK/commit/9f3a6243bf8)

Record: [`20210913_MplTypeTraitsReplacedByStd.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20210913_MplTypeTraitsReplacedByStd.yaml)

:::::

### ITK 5.3rc01

(itk5-BSplineNumberOfWeightsConstexpr)=
#### BSpline Number Of Weights Constexpr

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Class changes**

With ITK 5.3, the `GetNumberOfWeights()` member functions of `itk::BSplineBaseTransform` and `itk::BSplineInterpolationWeightFunction`
are replaced by static constexpr data members named `NumberOfWeights`, and the `GetSupportSize()` member function of
`itk::BSplineInterpolationWeightFunction` is replaced by a static constexpr data member named `SupportSize`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #2709](https://github.com/InsightSoftwareConsortium/ITK/pull/2709) · [`34f3db8d8ad`](https://github.com/InsightSoftwareConsortium/ITK/commit/34f3db8d8ad)

Record: [`20210904_BSplineNumberOfWeightsConstexpr.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20210904_BSplineNumberOfWeightsConstexpr.yaml)

:::::

### ITK 5.1rc02

High impact, described under Most impactful changes:

- [Strongly Typed Enumerations](#itk5-StronglyTypedEnumerations)

(itk5-NiftiAnalyzeFlavors)=
#### Nifti Analyze Flavors

{bdg-warning}`medium` {bdg-info-line}`FileFormat` {bdg-info-line}`DefaultChanged` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake` {bdg-primary-line}`data`

**Class changes**

`itk::NiftiImageIO` was changed to support different kinds of Analyze file conventions.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

See [Mayo/SPM "Analyze" Format Spec Compilation](http://www.grahamwideman.com/gw/brain/analyze/formatdoc.htm) for detailed description.
The method `SetLegacyAnalyze75Mode` is now expecting parameter specifying which convention to use:
* `itk::Analyze75Flavor::AnalyzeReject` - refuse to read Analyze files and show an error message
* `itk::Analyze75Flavor::AnalyzeITK4` - behaviour introduced in ITK4 NIFTI reader
* `itk::Analyze75Flavor::AnalyzeITK4Warning` - same as `AnalyzeITK4`, but a warning will be displayed
* `itk::Analyze75Flavor::AnalyzeSPM` -  use SPM convention, same as Analyze reader in ITK3/ITK4
* `itk::Analyze75Flavor::AnalyzeFSL` -  use FSL convention
There is also now CMake configuration option `ITK_NIFTI_IO_ANALYZE_FLAVOR` which specifies default behaviour of the reader, `ITK4Warning` is the default.

**Links:** <http://www.grahamwideman.com/gw/brain/analyze/formatdoc.htm>, <https://github.com/InsightSoftwareConsortium/ITK/pull/1590>

[PR #1557](https://github.com/InsightSoftwareConsortium/ITK/pull/1557) · [`1b21b239872`](https://github.com/InsightSoftwareConsortium/ITK/commit/1b21b239872)

Record: [`20200208_NiftiAnalyzeFlavors.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20200208_NiftiAnalyzeFlavors.yaml)

:::::

(itk5-TreeContainerDeprecated)=
#### Tree Container Deprecated

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

**Class changes**

`itk::TreeNode`, `itk::TreeContainer`, `itk::TreeIteratorBase`, and all related classes have been moved to `ITKDeprecated` module.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #1490](https://github.com/InsightSoftwareConsortium/ITK/pull/1490) · [`48c2ff830e1`](https://github.com/InsightSoftwareConsortium/ITK/commit/48c2ff830e1)

Record: [`20191225_TreeContainerDeprecated.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20191225_TreeContainerDeprecated.yaml)

:::::

### ITK 5.1rc01

(itk5-ConsolidatedVectorFilters)=
#### Consolidated Vector Filter

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

In ITK 5.0 many "vector" specialized filters and functions are
deprecated.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

These filters commonly contain a "Vector" prefix to the
regular filter. "Vector" may refer to an `itk::Image` of
`itk::Vector` pixel types and/or  an `itk::VectorImage`. Support for
"vector" images is being consolidated into the regular filters without
the "Vector" prefix. The following deprecated classes need to be replaced
as follows:

* `VectorCentralDifferenceImageFunction` -> `CentralDifferenceImageFunction`
* `VectorExpandImageFilter` -> `ExpandImageFilter`
* `VectorCastImageFilter` -> `CastImageFilter`
* `VectorResampleImageFilter` -> `ResampleImageFilter`

Additionally, the following change should be considered:

* `WarpImageFilter` -> `ResampleImageFilter`
* `WarpVectorImageFilter` -> `ResampleImageFilter`

[This update to the ITK
Examples](https://github.com/InsightSoftwareConsortium/ITK/commit/b8dbc939ecb086b0e60faf4710657596553f643f)
is illustrative on how to use the `ResampleImageFilter` in place of a
warp filter. Note these warp filters are being considered for
deprecation in the future.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/commit/b8dbc939ecb086b0e60faf4710657596553f643f>

[PR #1311](https://github.com/InsightSoftwareConsortium/ITK/pull/1311) · [`ad8f0bbd6f5`](https://github.com/InsightSoftwareConsortium/ITK/commit/ad8f0bbd6f5)

Record: [`20191017_ConsolidatedVectorFilters.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20191017_ConsolidatedVectorFilters.yaml)

:::::

### ITK 5.0.0

(itk5-StatisticsFiltersNoImageOutput)=
#### Statistics Filters No Image Output

{bdg-warning}`medium` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

**Class changes**

`itk::StatisticsImageFilter`, `itk::LabelStatisticsImageFilter` and
`itk::MinimumMaximumImageFilter` no longer produce an image as their
primary output, as it was a shallow copy of the primary
input. Additionally, minor API changes have occurred related to the
decorated output methods to conform to ITK conventions.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #855](https://github.com/InsightSoftwareConsortium/ITK/pull/855) · [`884fe6e013c`](https://github.com/InsightSoftwareConsortium/ITK/commit/884fe6e013c)

Record: [`20220718_StatisticsFiltersNoImageOutput.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220718_StatisticsFiltersNoImageOutput.yaml)

:::::

(itk5-FixedArrayReverseIterators)=
#### Fixed Array Reverse Iterators

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Class changes**

`FixedArray` member functions `rBegin()` and `rEnd()` are replaced by `rbegin()` and `rend()`,
which return a `reverse_iterator`, compatible with the Standard C++ Library.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #857](https://github.com/InsightSoftwareConsortium/ITK/pull/857) · [`b86fc4e009e`](https://github.com/InsightSoftwareConsortium/ITK/commit/b86fc4e009e)

Record: [`20190511_FixedArrayReverseIterators.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190511_FixedArrayReverseIterators.yaml)

:::::

(itk5-ImageTransformerDeprecated)=
#### Image Transformer Deprecated

{bdg-secondary}`low` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**Class changes**

`itk::ImageTransformer` has been moved to `ITKDeprecated` module. The new `itk::ImageSink` filter can be used in its place.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #855](https://github.com/InsightSoftwareConsortium/ITK/pull/855) · [`08fcda0f882`](https://github.com/InsightSoftwareConsortium/ITK/commit/08fcda0f882)

Record: [`20190515_ImageTransformerDeprecated.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190515_ImageTransformerDeprecated.yaml)

:::::

(itk5-InterpolatorGetRadiusRequired)=
#### Interpolator Get Radius Required

{bdg-secondary}`low` {bdg-primary-line}`cxx`

**Class changes**

All descendents of `itk::InterpolateImageFunction` must implement
`SizeType GetRadius() const` to indicate support radius of the interpolator.
This is used in `itk::ResampleImageFilter` to support streaming.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #668](https://github.com/InsightSoftwareConsortium/ITK/pull/668) · [`e4003fc3dc6`](https://github.com/InsightSoftwareConsortium/ITK/commit/e4003fc3dc6)

Record: [`20190515_InterpolatorGetRadiusRequired.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190515_InterpolatorGetRadiusRequired.yaml)

:::::

(itk5-BoundingBoxComputeCorners)=
#### Bounding Box Compute Corners

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Class changes**

`BoundingBox` member function `GetCorners()` is replaced by `ComputeCorners()`, which return an
`std::array<PointType, NumberOfCorners>`, instead of a pointer to a `PointsContainer`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #892](https://github.com/InsightSoftwareConsortium/ITK/pull/892) · [`814419ae9f9`](https://github.com/InsightSoftwareConsortium/ITK/commit/814419ae9f9)

Record: [`20190517_BoundingBoxComputeCorners.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190517_BoundingBoxComputeCorners.yaml)

:::::

### ITK 5.0rc02

(itk5-BarrierAndVectorFiltersDeprecated)=
#### Barrier And Vector Filters Deprecated

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx` {bdg-primary-line}`cmake`

**Class changes**

`itk::Barrier`, `itk::VectorResampleImageFilter` and  `itk::VectorCastImageFilter` have been moved to `ITKDeprecated` module.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/856>, <https://github.com/InsightSoftwareConsortium/ITK/pull/870>

[PR #718](https://github.com/InsightSoftwareConsortium/ITK/pull/718) · [`51fc9d9c100`](https://github.com/InsightSoftwareConsortium/ITK/commit/51fc9d9c100)

Record: [`20190516_BarrierAndVectorFiltersDeprecated.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190516_BarrierAndVectorFiltersDeprecated.yaml)

:::::

(itk5-SpatialObjectRefactoring)=
#### Spatial Objects Refactoring

{bdg-warning}`medium` {bdg-info-line}`ApiRenamed` {bdg-info-line}`ApiRemoved` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx`

The SpatialObject classes were refactored to address issues and inconsistencies that had arisen over the years.  The severity of these issues and inconsistencies was such that the classes provided incorrect and/or unexpected behaviors that made their proper usage problematic.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The most notable change is that dependencies on VNL Tree data structures and the requirement that every SpatialObject be defined
in an object space were eliminated.   The VNL Tree data structures introduced unnecessary complexity, additional (redundant)
transformations, and dependencies on VNL in the API that ITKv5 seeks to eliminate.   The elimination of a dependency on an IndexSpace
simplified the set of transformations that each SpatialObject needed to maintain and made the API of every SpatialObject more consistent
and intuitive.

SpatialObjects now only have two spaces and two transforms directly associated with them:
* ObjectSpace is the space that is "local" to each object.   It is the physical space in which its data/parameters are defined.  For
example, for an ImageSpatialObject it is the "PhysicalSpace" (see Image Class definition) of the Image.   For an Line/Tube/DTI and other
point-based SpatialObjects, it is the inherent space in which the Point coordinates are specified.   Typically, when a SpatialObject is
extracted from an Image, the parameters/coordinates of the SpatialObject are the space as the physical space of the source Image.   Any
children of a SpatialObject are defined within the ObjectSpace of that parent SpatialObject.

* ObjectToParent transform is the transform applied to move a SpatialObject within its parent object's ObjectSpace.   An ObjectToParent
transform is an invertible affine transform.  It is used to, for example, align a SpatialObject with a parent image (e.g., if an object
is extracted from one ImageSpatialObject but then aligned to and made a child of another ImageSpatialObject as is needed for atlas-based
image segmentation or for image-to-image registration).    If an object does not have a parent, then its ObjectToParent transform
specifies how that object is positioned in "WorldSpace", as defined next.

* WorldSpace is the coordinate system defined by the top-level SpatialObject in a hierarchy (defined by parent-child relationships) of a
set of SpatialObjects.   The top-level SpatialObject does not have a parent SpatialObject, and its ObjectToParentTransform specifies how
its ObjectSpace is transformed to map into the WorldSpace.

* ObjectToWorld transform is a derived transform that is managed by each SpatialObject for convenience (e.g., to save redundant
computations).  It is a composition of all of the ObjectToParent transforms from an object's ObjectSpace to the WorldSpace in which it
exists.   If an application maintains a single WorldSpace, point selections, object intersections, and such can be readily resolved by
using the ObjectToWorld transforms (and their inverses) at each object.

Each member function and variable of a SpatialObject now explicitly declares (via its name) if it is operating in WorldSpace or
ObjectSpace.   For example, the `IsInside( point )` function has been replaced by two functions `IsInsideInObjectSpace( point )` and
`IsInsideInWorldSpace( point )` functions.

As implied above, the changes to SpatialObject are extensive.   They include the following:
* Replace use of ComputeMyBoundingBox() with Update();
* Replace use of ComputeObjectToWorldTransform() with Update();
* Eliminate IndexToX transforms. SpatialObjects exist purely in physical space coordinates
* Eliminate vnl TreeNode usage. SpatialObjects now track their transforms, parents, and children directly.
* Eliminate AffineGeometryFrame
* Eliminate OffsetTable computation
* Dimension renamed to ObjectDimension - for consistency
* `AddSpatialObject()` renamed to `AddChild()` - for consistency
* `RemoveSpatialObject()` renamed to `RemoveChild()` - for consistency
* `Clear()` renamed to `ClearChildren()` - for clarity and consistency
* `AddChildrenToList()` added to speed parsing of children in tree
* `GetSpatialObjectTypeAsString()` is now `GetClassNameAndDimension()`
* PropertyType is not templated
* Converted API from using char * to using std::string
* `SetParent()` and `AddChild()` now create consistent trees and do not cause a spatial object to move in physical space (i.e., transforms are updated appropriately).
* `RemoveChild()` and `RemoveAllChildren()` fixed to remove all pointers to / from those children to / from the tree
* Helper functions simplify the specification of `IsInsideInObjectSpace()`, `ValueAtInObjectSpace()`, and other computations that potentially traverse an SO tree.
* Derived classes typically only need to implement `IsInsideInObjectSpace()` and `ComputeMyBoundingBoxInObjectSpace()` member functions. Logic for `ValueAtInObjectSpace()`, `IsInsideInWorldSpace()` and such is improved.
* PointBasedSpatialObjects had a PointListType type declaration.  This was confusing because it referred to a list of SpatialObjectPoints and not ITK::Points.  So, to avoid such confusion, now TubeSpatialObjects define TubePointListType, BlobSpatialObjects define BlobPointListType, and so forth.
* `ImageMaskSpatialObject::GetAxisAlignedBoundingBoxRegion()` was removed. `ImageMaskSpatialObject::ComputeMyBoundingBoxInIndexSpace()` should be used instead.
* `SpatialObjectReader::GetScene` was renamed to `GetGroup` along with changing the type from `ScenePointer` to `GroupPointer`.
  * Elements of a `Group` are now `Children`, so `scene->GetObjects` now becomes `group->GetChildren`

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/630>

[PR #613](https://github.com/InsightSoftwareConsortium/ITK/pull/613) · [`261cac34718`](https://github.com/InsightSoftwareConsortium/ITK/commit/261cac34718)

Record: [`20250107_SpatialObjectRefactoring.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20250107_SpatialObjectRefactoring.yaml)

:::::

(itk5-WatershedResultsChange)=
#### Watershed Results Change

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

**Class changes**

`WatershedImageFilter` produces slightly different results due to changes of `hash_map`.
Generally, [MorphologicalWatersheds](https://docs.itk.org/projects/doxygen/en/stable/classitk_1_1MorphologicalWatershedImageFilter.html)
produce more stable results and should be preferred to regular watersheds.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/744>

[PR #187](https://github.com/InsightSoftwareConsortium/ITK/pull/187) · [`0e2d4f2dc55`](https://github.com/InsightSoftwareConsortium/ITK/commit/0e2d4f2dc55)

Record: [`20250117_WatershedResultsChange.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20250117_WatershedResultsChange.yaml)

:::::

(itk5-GaussianDerivativeImageFunctionRenames)=
#### Gaussian Derivative Image Function Renames

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Class changes**

`GaussianDerivativeImageFunction::ImageDimension2` is renamed to `GaussianDerivativeImageFunction::ImageDimension`.
The nested `GaussianDerivativeImageFunction` types `GaussianDerivativeFunctionType` and
`GaussianDerivativeFunctionPointer` are renamed to `GaussianDerivativeSpatialFunctionType` and
`GaussianDerivativeSpatialFunctionPointer`, respectively.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/commit/b6c668ea346>

[PR #559](https://github.com/InsightSoftwareConsortium/ITK/pull/559) · [`207131557a8`](https://github.com/InsightSoftwareConsortium/ITK/commit/207131557a8)

Record: [`20190301_GaussianDerivativeImageFunctionRenames.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190301_GaussianDerivativeImageFunctionRenames.yaml)

:::::

(itk5-ItksysHashMapRemoved)=
#### Itksys Hash Map Removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

**Class changes**

`itksys::hash_map` has been removed. It should be replaced by `std::unordered_map`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #187](https://github.com/InsightSoftwareConsortium/ITK/pull/187) · [`0e2d4f2dc55`](https://github.com/InsightSoftwareConsortium/ITK/commit/0e2d4f2dc55)

Record: [`20190411_ItksysHashMapRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190411_ItksysHashMapRemoved.yaml)

:::::

### ITK 5.0rc01

(itk5-PythonExtrasSnakeCase)=
#### Python Extras Snake Case

{bdg-secondary}`low` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`python`

**Python changes**

Arguments to functions in `extras` were cleaned up and now use snake case.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #403](https://github.com/InsightSoftwareConsortium/ITK/pull/403) · [`443955f9644`](https://github.com/InsightSoftwareConsortium/ITK/commit/443955f9644)

Record: [`20210320_PythonExtrasSnakeCase.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20210320_PythonExtrasSnakeCase.yaml)

:::::

(itk5-PythonMeshWrappingSimplified)=
#### Python Mesh Wrapping Simplified

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`python` {bdg-primary-line}`wrapping`

**Python changes**

Mesh-related class wrapping has been simplified, made more consistent, and
expanded, but previous template parameters may not be available.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PR #403](https://github.com/InsightSoftwareConsortium/ITK/pull/403) · [`443955f9644`](https://github.com/InsightSoftwareConsortium/ITK/commit/443955f9644)

Record: [`20210320_PythonMeshWrappingSimplified.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20210320_PythonMeshWrappingSimplified.yaml)

:::::

### ITK 5.0b02

(itk5-OtsuReturnsBinMaximum)=
#### Otsu Returns Bin Maximum

{bdg-warning}`medium` {bdg-info-line}`ResultsChange` {bdg-info-line}`DefaultChanged` {bdg-primary-line}`cxx` {bdg-primary-line}`data`

**Class changes**

Otsu filters now return the correct threshold (bin's maximum value instead of mid-point) with ITKv5.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The old behavior is kept when ITKV4_COMPATIBILITY is enabled by setting `ReturnBinMidpoint` to true by default.
It is recommended when migrating to ITKv5 to explicitly set the `ReturnBinMidpoint` value to false.
This change may effect computations which rely on the results of an Otsu threshold filter.

[`79148b662d4`](https://github.com/InsightSoftwareConsortium/ITK/commit/79148b662d4)

Record: [`20190125_OtsuReturnsBinMaximum.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20190125_OtsuReturnsBinMaximum.yaml)

:::::

(itk5-ThreadingSymbolsInItkNamespace)=
#### Threading Symbols In Itk Namespace

{bdg-warning}`medium` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

:::::{dropdown} Details and examples
:class-container: sd-mb-4

**Multithreading refactored**

-  ITK_THREAD_RETURN_TYPE is now in the itk:: namespace
```cpp
#if ITK_VERSION_MAJOR >= 5
  static itk::ITK_THREAD_RETURN_TYPE NetworkingThreaderCallback( void * );
#else
  static ITK_THREAD_RETURN_TYPE NetworkingThreaderCallback( void * );
#endif
```

- ITK_THREAD_RETURN_VALUE is named itk::ITK_THREAD_RETURN_DEFAULT_VALUE
```cpp
#if ITK_VERSION_MAJOR >= 5
  return itk::ITK_THREAD_RETURN_DEFAULT_VALUE;
#else
  return ITK_THREAD_RETURN_VALUE;
#endif
```

- ThreadInfoStruct is renamed to WorkUnitInfo
```cpp
#if ITK_VERSION_MAJOR >= 5
    (((itk::PlatformMultiThreader::WorkUnitInfo *)(arg))->UserData);
#else
    (((itk::MultiThreader::ThreadInfoStruct *)(arg))->UserData);
#endif
```

The variables `ITK_MAX_THREADS` and `ITK_DEFAULT_THREAD_ID` are now in the `itk::` namespace.
Backwards compatibility is currently supported by exposing these to the global namespace
with
```cpp
  using itk::ITK_MAX_THREADS;
  using itk::ITK_DEFAULT_THREAD_ID;
```

[`df93980bc02`](https://github.com/InsightSoftwareConsortium/ITK/commit/df93980bc02)

Record: [`20220214_ThreadingSymbolsInItkNamespace.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220214_ThreadingSymbolsInItkNamespace.yaml)

:::::

(itk5-StdMutexAndAtomicReplaceItkClasses)=
#### Std Mutex And Atomic Replace Itk Classes

{bdg-warning}`medium` {bdg-info-line}`ApiDeprecated` {bdg-primary-line}`cxx`

**C++11**

Availability of the C++11 standard allows use of many Standard Library
features.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

These were previously implemented as portable ITK classes.
The standard library classes are preferred over ITK's implementations.
The most notable examples of this are:
 * [atomic integers](https://docs.itk.org/projects/doxygen/en/v4.13.0/classitk_1_1AtomicInt.html) should be replaced by `std::atomic`.
 * [mutex locks](https://docs.itk.org/projects/doxygen/en/v4.13.0/classitk_1_1MutexLock.html)
and related classes should be replaced by the similarly named classes from STL's
[`<mutex>`](https://en.cppreference.com/w/cpp/header/mutex) header.
 * `itksys::hash_map` should be replaced by [std::unordered_map](https://en.cppreference.com/w/cpp/container/unordered_map).

To modernize your code base, replace:
  * `SimpleFastMutexLock` with `std::mutex`, and `#include "itkSimpleFastMutexLock.h"`  with `#include <mutex>`.
  * `FastMutexLock` with `std::mutex`, and `#include "itkFastMutexLock.h"`  with `#include <mutex>`.
  * `MutexLock` with `std::mutex`, and `#include "itkMutexLock.h"`  with `#include <mutex>`.

[`b56b8cc34e7`](https://github.com/InsightSoftwareConsortium/ITK/commit/b56b8cc34e7)

Record: [`20250117_StdMutexAndAtomicReplaceItkClasses.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20250117_StdMutexAndAtomicReplaceItkClasses.yaml)

:::::

(itk5-MinimumSystemVxl202)=
#### Minimum System Vxl 202

{bdg-secondary}`low` {bdg-info-line}`Dependency` {bdg-primary-line}`cmake`

**Legacy code removed**

Support for for pre-20160229 `VXL_VERSION_DATE_FULL` system installed
versions of VXL has been removed.  VXL now supports more common Semantic Versioning
conventions with the minimum supported version for ITKv5 being 2.0.2 (as of 2018-11-30).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[`8f4ff0a28b3`](https://github.com/InsightSoftwareConsortium/ITK/commit/8f4ff0a28b3)

Record: [`20181204_MinimumSystemVxl202.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20181204_MinimumSystemVxl202.yaml)

:::::

(itk5-VerifyPreconditionsConst)=
#### Verify Preconditions Const

{bdg-secondary}`low` {bdg-primary-line}`cxx`

**Class changes**

With ITK 5.0, `itk::ProcessObject::VerifyPreconditions()`  and
`itk::ProcessObject::VerifyInputInformation` are now declared `const`,
so if you have overridden these virtual member function, make sure that you
also add `const`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

If your application needs to compile with both ITKv4 and ITKv5,
you should use macro `ITKv5_CONST` instead of `const` keyword.
This macro is present in ITKv4 since commit
[commit b40f74e on 2018-11-14](https://github.com/InsightSoftwareConsortium/ITK/commit/b40f74e07d74614c75be4aceac63b87e80e589d1).

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/commit/b40f74e07d74614c75be4aceac63b87e80e589d1>

[`3e6b6f5bd67`](https://github.com/InsightSoftwareConsortium/ITK/commit/3e6b6f5bd67)

Record: [`20201110_VerifyPreconditionsConst.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20201110_VerifyPreconditionsConst.yaml)

:::::

### ITK 5.0b01

High impact, described under Most impactful changes:

- [Number Of Threads To Work Units](#itk5-NumberOfThreadsToWorkUnits)

(itk5-ProgressReporterToProgressTransformer)=
#### Progress Reporter To Progress Transformer

{bdg-secondary}`low` {bdg-primary-line}`cxx`

**Class changes**

Since `itk::ProgressReporter` does not work well with the new threading model,
it should be replaced by `itk::ProgressTransformer`.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

This only applies to classes which use `GenerateData()` method, and either
have multiple `ParallelizeRegion` calls or a long single-threaded section.
An example of how to add progress reporting can be found in
[this commit](https://github.com/InsightSoftwareConsortium/ITK/commit/dd0b0d128d6c0760cefd8a958107cb0e841b51b4).

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/commit/dd0b0d128d6c0760cefd8a958107cb0e841b51b4>

[`18818da5b23`](https://github.com/InsightSoftwareConsortium/ITK/commit/18818da5b23)

Record: [`20180905_ProgressReporterToProgressTransformer.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20180905_ProgressReporterToProgressTransformer.yaml)

:::::

(itk5-HoughCirclesRadiusTemplateArgument)=
#### Hough Circles Radius Template Argument

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

**Class changes**

`HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>` no longer
has a default argument for its last template parameter. Instead, users of the filter should now
explicitly specify all three template arguments.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Earlier versions of ITK assumed that the radius
pixel type should be the same as `TOutputPixelType`. However, it appears that for the radius pixel
type (`TRadiusPixelType`), a floating point type is often preferred, whereas for the accumulator
output pixel type (`TOutputPixelType`), an unsigned integer type is often more appropriate.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/278>

[`06afe0fbd03`](https://github.com/InsightSoftwareConsortium/ITK/commit/06afe0fbd03)

Record: [`20181207_HoughCirclesRadiusTemplateArgument.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20181207_HoughCirclesRadiusTemplateArgument.yaml)

:::::

(itk5-TransformJacobianPositionType)=
#### Transform Jacobian Position Type

{bdg-secondary}`low` {bdg-primary-line}`cxx`

**Class changes**

`itk::Transform` class had method signature change.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

`void ComputeJacobianWithRespectToPosition(const PointType &, JacobianType &) const`
now has signature
`void ComputeJacobianWithRespectToPosition(const PointType &, JacobianPositionType &) const`.
`JacobianPositionType` is publicly exposed in `itk::Transform`.
See commit [commit 212cae5](https://github.com/InsightSoftwareConsortium/ITK/commit/212cae522d8451ea089c41f8a151279e1dd17042) for details.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/pull/2101>

[`212cae522d8`](https://github.com/InsightSoftwareConsortium/ITK/commit/212cae522d8)

Record: [`20201110_TransformJacobianPositionType.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20201110_TransformJacobianPositionType.yaml)

:::::

### ITK 5.0a02

High impact, described under Most impactful changes:

- [Dynamic Threaded Generate Data](#itk5-DynamicThreadedGenerateData)

(itk5-MinimumCMake3102)=
#### Modern CMake requirement

{bdg-warning}`medium` {bdg-info-line}`Dependency` {bdg-info-line}`CompilerEnvironment` {bdg-primary-line}`cmake`

ITK now requires **CMake 3.10.2** for configuration. While only a few features
enabled by this modern version of CMake are being used for 5.0 release, a
modern version of CMake is a prerequisite for future more aggressive updates
to ITK's build system. This is planned for a later version.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Before making other changes suggested in this document,
your code should work with the latest version of CMake.

[`a4f6697acd6`](https://github.com/InsightSoftwareConsortium/ITK/commit/a4f6697acd6)

Record: [`20180905_MinimumCMake3102.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20180905_MinimumCMake3102.yaml)

:::::

(itk5-MultiThreaderClassHierarchy)=
#### Multi Threader Class Hierarchy

{bdg-warning}`medium` {bdg-info-line}`ApiRenamed` {bdg-primary-line}`cxx`

**Multithreading refactored**

Since ITK 5.0 `itk::MultiThreader` has been split into a class hierarchy.
Instead of a single `itk::MultiThreader` class which could optionally delegate work
to an [itk::ThreadPool](https://docs.itk.org/projects/doxygen/en/stable/classitk_1_1ThreadPool.html),
there are now multiple backends to provide thread-based parallel processing.
Most of the time you will want to replace `itk::MultiThreader` by
[itk::MultiThreaderBase](https://docs.itk.org/projects/doxygen/en/stable/classitk_1_1MultiThreaderBase.html).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[PlatformMultiThreader](https://docs.itk.org/projects/doxygen/en/stable/itkPlatformMultiThreader_8h.html)
is essentially the old `itk::MultiThreader`, renamed. `itk::PoolMultiThreader` behaves like
the old `itk::MultiThreader` with `ITK_USE_THREADPOOL=ON`. There is an addition of
[TBBMultiThreader](https://docs.itk.org/projects/doxygen/en/stable/classitk_1_1TBBMultiThreader.html),
which uses Intel Thread Building Blocks library's thread-pool, with has load balancing features.
The option to build TBB support must be enabled during the CMake configuration step.
The default multi-threader can be set via environment variable
`ITK_GLOBAL_DEFAULT_THREADER` with possible case-insensitive values of
`Platform`, `Pool` and `TBB`, e.g. `ITK_GLOBAL_DEFAULT_THREADER=tbb`.

[`e0899a1ee00`](https://github.com/InsightSoftwareConsortium/ITK/commit/e0899a1ee00)

Record: [`20260511_MultiThreaderClassHierarchy.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20260511_MultiThreaderClassHierarchy.yaml)

:::::

(itk5-FilterWatcherRemoved)=
#### Filter Watcher Removed

{bdg-secondary}`low` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

**Class changes**

`itk::FilterWatcher` was deleted.
It should be replaced by [itk::SimpleFilterWatcher](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Modules/Core/Common/include/itkSimpleFilterWatcher.h).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[`e4355459bab`](https://github.com/InsightSoftwareConsortium/ITK/commit/e4355459bab)

Record: [`20231108_FilterWatcherRemoved.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20231108_FilterWatcherRemoved.yaml)

:::::

### ITK 5.0a01

High impact, described under Most impactful changes:

- [Require Cxx 11 Compiler](#itk5-RequireCxx11Compiler)
- [Legacy code removed](#itk5-LegacyCodeRemoved)

(itk5-SmartPointerNullInitialization)=
#### Smart Pointer Null Initialization

{bdg-warning}`medium` {bdg-info-line}`ApiRemoved` {bdg-primary-line}`cxx`

**C++11**

Errors similar to `error: conversion from 'int' to 'typename InterpolatorType::Pointer'` are a result of further
type safety for dealing with pointers.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

Enhancements in nullptr behavior in ITKv5 provide more clear
type checking and respect the nullptr identifier.  The 'long 0' value
known as NULL causes an ambiguity for overload compilations of the ITKv5 smartpointers. To be backwards compatible
with pre C++11 compilers use the `ITK_NULLPTR` designation, otherwise replace NULL and 0 initialization of
`itk::SmartPointer` with nullptr.

[`ecb6dd083e4`](https://github.com/InsightSoftwareConsortium/ITK/commit/ecb6dd083e4)

Record: [`20220718_SmartPointerNullInitialization.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20220718_SmartPointerNullInitialization.yaml)

:::::

(itk5-CodingStyleCxx11)=
#### Updated style

{bdg-secondary}`low` {bdg-info-line}`Policy` {bdg-primary-line}`cxx`

Important changes in style have also been integrated in ITK to match
C++11 best practices. This includes replacing `typedef` calls with
the keyword `using`, the usage of the keyword `auto` when appropriate,
and moving the macros `ITK_DISALLOW_COPY_AND_ASSIGN` or `ITK_DISALLOW_COPY_AND_MOVE` from the private
class section to the public class section. The ITK Software Guide has
been updated to match these changes.

:::::{dropdown} Details and examples
:class-container: sd-mb-4

[`66e5d6b3bcc`](https://github.com/InsightSoftwareConsortium/ITK/commit/66e5d6b3bcc)

Record: [`20210215_CodingStyleCxx11.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20210215_CodingStyleCxx11.yaml)

:::::

(itk5-ItkV5PreparationScripts)=
#### Update scripts

{bdg-secondary}`low` {bdg-primary-line}`cxx`

A set of bash scripts that automate common ITKv4 → ITKv5 source-tree
rewrites used to live at [`Utilities/ITKv5Preparation/`](https://github.com/InsightSoftwareConsortium/ITK/tree/21851448adfe9b81790f0f370314b8c1a21fbdba/Utilities/ITKv5Preparation).

:::::{dropdown} Details and examples
:class-container: sd-mb-4

The directory was removed from the active ITK tree after the migration
window closed; the permalink above points to the last commit where the
scripts were present, so they remain accessible to anyone bringing a
very old ITKv4 codebase forward.

**Links:** <https://github.com/InsightSoftwareConsortium/ITK/tree/21851448adfe9b81790f0f370314b8c1a21fbdba/Utilities/ITKv5Preparation>

[`0365d6934d0`](https://github.com/InsightSoftwareConsortium/ITK/commit/0365d6934d0)

Record: [`20260511_ItkV5PreparationScripts.yaml`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/docs/migration_guides/records/itk5/20260511_ItkV5PreparationScripts.yaml)

:::::
