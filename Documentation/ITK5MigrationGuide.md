ITK v5 Migration Guide
======================

This guide documents the changes required to migrate a code base
which uses ITK v4 to use ITK v5. The migration guide for transition
from v3 to v4 can be found [here](https://itk.org/migrationv4/).

Legacy code removed
-------------------

All code which was marked as legacy in ITK 4.13 has been removed.
External code which previously required
`ITK_LEGACY_REMOVE` CMake option to be `OFF` in order to build will now fail
to compile. Before starting migration to v5 (as explained in this guide),
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

Support for for pre-20160229 `VXL_VERSION_DATE_FULL` system installed
versions of VXL has been removed.  VXL now supports more common Semantic Versioning
conventions with the minimum supported version for ITKv5 being 2.0.2 (as of 2018-11-30).

C++11
-----

A major improvement in ITK 5 is to fully adopt C++11.
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

Errors similar to `error: conversion from 'int' to 'typename InterpolatorType::Pointer'` are a result of further
type safty for dealing with pointers. Enhancements in nullptr behavior in ITKv5 provide more clear
type checking and respect the nullptr identifier.  The 'long 0' value
known as NULL causes an abiguity for overload compilations of the ITKv5 smartpointers. To be backwards compatible
with pre C++11 compilers use the `ITK_NULLPTR` designation, otherwise replace NULL and 0 initialization of
`itk::SmartPointer` with nullptr.



Availability of the C++11 standard allows use of many Standard Library
features. These were previously implemented as portable ITK classes.
The standard library classes are preferred over ITK's implementations.
The most notable examples of this are:
 * [atomic integers](https://itk.org/Doxygen/html/classitk_1_1AtomicInt.html) should be replaced by `std::atomic`.
 * [mutex locks](https://itk.org/Doxygen/html/classitk_1_1MutexLock.html)
and related classes should be replaced by the similarly named classes from
[<mutex>](https://en.cppreference.com/w/cpp/header/mutex) header.
 * itksys::hash_map should be replaced by [std::unordered_map](https://en.cppreference.com/w/cpp/container/unordered_map).

To modernize your code base, replace:
  * `SimpleFastMutexLock` with `std::mutex`, and `#include "itkSimpleFastMutexLock.h"`  with `#include <mutex>`.
  * `FastMutexLock` with `std::mutex`, and `#include "itkFastMutexLock.h"`  with `#include <mutex>`.
  * `MutexLock` with `std::mutex`, and `#include "itkMutexLock.h"`  with `#include <mutex>`.


Modern CMake requirement
------------------------

ITK now requires **CMake 3.10.2** for configuration. While only a few features
enabled by this modern version of CMake are being used for 5.0 release, a
modern version of CMake is a prerequisite for future more aggressive updates
to ITK's build system. This is planned for a later version.

Before making other changes suggested in this document,
your code should work with the latest version of CMake.

Updated style
-------------

Important changes in style have also been integrated in ITK to match
C++11 best practices. This includes replacing `typedef` calls with
the keyword `using`, the usage of the keyword `auto` when appropriate,
and moving the macro `ITK_DISALLOW_COPY_AND_ASSIGN` from the private
class section to the public class section. The ITK Software Guide has
been updated to match these changes.

Multithreading refactored
-------------------------

Since ITK 5.0 `itk::MultiThreader` has been split into a class hierarchy.
Instead of a single `itk::MultiThreader` class which could optionally delegate work
to an [itk::ThreadPool](https://itk.org/Insight/Doxygen/html/classitk_1_1ThreadPool.html),
there are now multiple backends to provide thread-based parallel processing.
Most of the time you will want to replace `itk::MultiThreader` by
[itk::MultiThreaderBase](https://itk.org/Insight/Doxygen/html/classitk_1_1MultiThreaderBase.html).

[PlatformMultiThreader](https://itk.org/Insight/Doxygen/html/itkPlatformMultiThreader_8h.html)
is essentially the old `itk::MultiThreader`, renamed. `itk::PoolMultiThreader` behaves like
the old `itk::MultiThreader` with `ITK_USE_THREADPOOL=ON`. There is an addition of
[TBBMultiThreader](https://itk.org/Insight/Doxygen/html/classitk_1_1TBBMultiThreader.html),
which uses Intel Thread Building Blocks library's thread-pool, with has load balancing features.
The option to build TBB support must be enabled during the CMake configuration step.
The default multi-threader can be set via environment variable
`ITK_GLOBAL_DEFAULT_THREADER` with possible case-insensitive values of
`Platform`, `Pool` and `TBB`, e.g. `ITK_GLOBAL_DEFAULT_THREADER=tbb`.

For filter multi-threading, a new signature has been introduced:
`void DynamicThreadedGenerateData( const OutputRegionType& threadRegion )`.
By default, this new signature is invoked instead of the classic
`void ThreadedGenerateData( const OutputRegionType& threadRegion, ThreadIdType threadId )`.
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
  //code1 (parallel)
  myBarrier->Wait();
  if (threadId==0)
    {
    //code2 single-threaded
    }
  //code3 (parallel)
}
```

after refactoring to not use barrier:
```cpp
GenerateData() //Not Threaded
{
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();
  ParallelizeImageRegion(code1 as lambda)
  //code2 single-threaded
  ParallelizeImageRegion(code3 as lambda)
  this->AfterThreadedGenerateData();
}
```

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

- Pattern for Parallel Counting:
Previously parallel counting was often storing per-thread counts in an `itk::Array`
and aggregating the result in a filter's `AfterThreadedGenerateData()`.
With C++11, you might want to instead use `std::atomic`.
An external module example that demonstrates this can be found in
[this commit](https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry/pull/32/commits/a8014c186ac53837362a0cb9db46ae224b8e9584).

Before, using `itk::Array`:
```C++
// Members:
Array<SizeValueType> m_NumVoxelsInsideMask;
BeforeThreadedGenerateData()
{
  // Resize the thread temporaries
  m_NumVoxelsInsideMask.SetSize(this->GetNumberOfThreads());
  m_NumVoxelsInsideMask.Fill(0);
}

ThreadedGenerateData(const RegionType & outputRegionForThread,
                     ThreadIdType threadId)
{
  // Do algorithm per threadId
  // Store the results per thread at the end
  m_NumVoxelsInsideMask[threadId] = numVoxelsForThisRegion;
}

AfterThreadedGenerateData()
{
  // Retrieve and sum all the results per thread.
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();
  SizeValueType numVoxelsInsideMask = 0;
  for (unsigned int i = 0; i < numberOfThreads; ++i )
    {
    numVoxelsInsideMask += m_NumVoxelsInsideMask[i];
    }
}

```
After, using `std::atomic`:
```C++
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

`Get/SetGlobalMaximumNumberOfThreads()`, and `GlobalDefaultNumberOfThreads()`
now reside in `itk::MultiThreaderBase`.  With a warning, they are still
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

The variables `ITK_MAX_THREADS` and `ITK_DEFAULT_THREAD_ID` are now in the `itk::` namespace.
Backwards compatibility is currently supported by exposing these to the global namespace
with
```C++
  using itk::ITK_MAX_THREADS;
  using itk::ITK_DEFAULT_THREAD_ID;
```

Spatial Objects Refactoring
---------------------------
The SpatialObject classes were refactored to address issues and inconsistencies that had arisen over the years.  The severity of these issues and inconsistencies was such that the classes provided incorrect and/or unexpected behaviors that made their proper usage problematic.

The most noteable change is that dependencies on VNL Tree data structures and the requirement that every SpatialObject be defined
in an object space were eliminated.   The VNL Tree data structures introduced unnecessary complexity, additional (redundant)
transformations, and dependencies on VNL in the API that ITKv5 seeks to eliminate.   The elimination of a dependency on an IndexSpace
simplified the set of transformations that each SpatialObject needed to maintain and made the API of every SpatialObject more consistent
and intuitive.

SpatialObjects now only have two spaces and two transforms directly associated with them:
* ObjectSpace is the space that is "local" to each object.   It is the physical space in which its data/parameters are defined.  For
example, for an ImageSpatialObject it is the "PhysicalSpace" (see Image Class definition) of the Image.   For an Line/Tube/DTI and other
point-based SpatialObjects, it is the inherent space in which the Point coordinates are specififed.   Typically, when a SpatialObject is
extracted from an Image, the parameters/coordinates of the SpatialObject are the space as the physical space of the source Image.   Any
children of a SpatialObject are defined within the ObjectSpace of that parent SpatialObject.

* ObjectToParent transform is the transform applied to move a SpatialObject within it's parent object's ObjectSpace.   An ObjectToParent
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
* `ImageMaskSpatialObject::GetAxisAlignedBoundingBoxRegion()` was removed.   `GetMyBoundingBoxInObjectSpace()` or `GetMyBoundingBoxInWorldSpace()` should be used instead.  If a region in IndexSpace is needed, then consider ITK's LabelMap classes and the [LabelImageToShapelLabelMapFilter](https://itk.org/Doxygen/html/classitk_1_1LabelImageToShapeLabelMapFilter.html).
```
  //REPLACE typename FixedImageType::RegionType roiRegion = roiMask->GetAxisAlignedBoundingBoxRegion();
  using LabelType = unsigned char;
  using ShapeLabelObjectType = itk::ShapeLabelObject< LabelType, Dimension >;
  using LabelMapType = itk::LabelMap< ShapeLabelObjectType >;

  using LI2SLMFType = itk::LabelImageToShapeLabelMapFilter<FixedImageType,LabelMapType >;
  typename LI2SLMFType::Pointer i2l = LI2SLMFType::New();
  i2l->SetInput(roiImage);
  i2l->Update();
  LabelMapType *labelMap = i2l->GetOutput();
  const typename LabelMapType::SizeValueType numLabelObjects = labelMap->GetNumberOfLabelObjects();
  ShapeLabelObjectType *labelObject = labelMap->GetNthLabelObject(1);
  typename FixedImageType::RegionType roiRegion = labelObject->GetBoundingBox();
```

Class changes
-------------

[itk::FilterWatcher](../Modules/Core/TestKernel/include/itkFilterWatcher.h) was deleted.
It should be replaced by [itk::SimpleFilterWatcher](../Modules/Core/Common/include/itkSimpleFilterWatcher.h).

Since `itk::ProgressReporter` does not work well with the new threading model,
it should be replaced by `itk::ProgressTransformer`.
This only applies to classes which use `GenerateData()` method, and either
have multiple `ParallelizeRegion` calls or a long single-threaded section.
An example of how to add progress reporting can be found in
[this commit](https://github.com/InsightSoftwareConsortium/ITK/commit/dd0b0d128d6c0760cefd8a958107cb0e841b51b4).

Otsu filters now return the correct threshold (bin's maximum value instead of mid-point) with ITKv5.
The old behavior is kept when ITKV4_COMPATIBILITY is enabled by setting `ReturnBinMidpoint` to true by default.
It is recommended when migrating to ITKv5 to explicitly set the `ReturnBinMidpoint` value to false.
This change may effect computations which rely on the results of an Otsu threshold filter.

`HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>` no longer
has a default argument for its last template parameter. Instead, users of the filter should now
explicitly specify all three template arguments. Earlier versions of ITK assumed that the radius
pixel type should be the same as `TOutputPixelType`. However, it appears that for the radius pixel
type (`TRadiusPixelType`), a floating point type is often preferred, whereas for the accumulator
output pixel type (`TOutputPixelType`), an unsigned integer type is often more appropriate.

`GaussianDerivativeImageFunction::ImageDimension2` is renamed to `GaussianDerivativeImageFunction::ImageDimension`.
The nested `GaussianDerivativeImageFunction` types `GaussianDerivativeFunctionType` and
`GaussianDerivativeFunctionPointer` are renamed to `GaussianDerivativeSpatialFunctionType` and
`GaussianDerivativeSpatialFunctionPointer`, respectively.

With ITK 5.0, `itk::ProcessObject::VerifyPreconditions()`  and
`itk::ProcessObject::VerifyInputInformation` are now declared `const`,
so if you have overridden these virtual member function, make sure that you
also add `const`. If your application needs to compile with both ITKv4 and ITKv5,
you should use macro `ITKv5_CONST` instead of `const` keyword.
This macro is present in ITKv4 since commit
b40f74e07d74614c75be4aceac63b87e80e589d1 on 2018-11-14.

Python changes
--------------

Mesh-related class wrapping has been simplified, made more consistent, and
expanded, but previous template parameters may not be available.

Arguments to functions in `itkExtras` were cleaned up and now use snake case.

Update scripts
--------------

[Utilities/ITKv5Preparation](../Utilities/ITKv5Preparation/) directory contains
bash scripts which have been used to update ITK to version 5. These scripts
can assist with updating external code bases to ITK 5 content and style.
