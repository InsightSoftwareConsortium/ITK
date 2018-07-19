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
migration to v4 should be finished so the external code builds
with `ITK_LEGACY_REMOVE` turned `ON` when compiling against
[ITK v4.13](https://github.com/InsightSoftwareConsortium/ITK/releases/tag/v4.13.0).

Code which was marked as `ITK_FUTURE_LEGACY_REMOVE` has been now
re-flagged as `ITK_LEGACY_REMOVE`. There have been some other
deprecations and API changes. The new behavior is activated by setting
`ITK_LEGACY_REMOVE` to `ON`. By default, compatibility with v4 is retained
(`ITK_LEGACY_REMOVE=OFF`).

Once the external code builds successfully with default configuration,
`ITK_LEGACY_REMOVE` should be set to `ON`.
Possible further build errors and/or warnings should be addressed.
Once it builds without legacy options, the migration to v5 is complete.

C++11
-----

The main improvement in this release is that ITK has been updated to fully use C++11.
Prior to that, a limited subset of C++11 functionalities were already available in ITK
through back-ports and macros that would take advantage of this standard if possible.
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
are also now used in ITK. However, due to limitations in C++11 support
Visual Studio 2013 (MSVC 12.0) and earlier cannot be used to build ITK since version 5.0.


Modern CMake requirement
------------------------

ITK now requires **CMake 3.10.2** to be configured. While only a few features
enabled by this modern version of CMake are being used for 5.0 release, a
modern version of CMake is a prerequisite for a bigger update of the ITK's
build system. This is planned for a later version.

Before making other changes in suggested in this document,
your code should work with the latest version of CMake.

Updated style
-------------

Important changes in style have also been integrated in ITK to match
the C++11 best practices. This includes replacing `typedef` calls with
the keyword `using`, the usage of the keyword `auto` when appropriate,
and moving the macro `ITK_DISALLOW_COPY_AND_ASSIGN` from the private
class section to the public class section. The ITK Software Guide has
been updated to match these changes.

Multithreading refactored
-------------------------

Since ITK 5.0 `MultiThreader` has been split into a class hierarchy.
Instead of a single `MultiThreader` class which could optionally delegate work
to an [itk:ThreadPool](https://itk.org/Insight/Doxygen/html/classitk_1_1ThreadPool.html),
there is now a class hierarchy. Most of the time you will want to replace `MultiThreader` by
[itk::MultiThreaderBase](https://itk.org/Insight/Doxygen/html/classitk_1_1MultiThreaderBase.html).

[PlatformMultiThreader](https://itk.org/Insight/Doxygen/html/itkPlatformMultiThreader_8h.html)
is essentially the old `MultiThreader`, renamed. `PoolMultiThreader` behaves like
the old `MultiThreader` with `ITK_USE_THREADPOOL=ON`. There is an addition of
[TBBMultiThreader](https://itk.org/Insight/Doxygen/html/classitk_1_1TBBMultiThreader.html),
which uses Intel Thread Building Blocks library's thread-pool with load balancing.
The option to build TBB needs to be enabled during CMake configure step.
The default multi-threader can be set via environment variable
`ITK_GLOBAL_DEFAULT_THREADER` with allowed case-insensitive values of
`Platform`, `Pool` and `TBB`, e.g. `ITK_GLOBAL_DEFAULT_THREADER=tbb`.

For filter multi-threading, a new signature has been introduced:
`void DynamicThreadedGenerateData( const OutputRegionType& threadRegion )`.
By default, this new signature is invoked instead of the classic
`void ThreadedGenerateData( const OutputRegionType&, ThreadIdType )`.
To temporarily get the old behavior (classic signature invoked by default),
set `ITKV4_COMPATIBILITY` to `ON` in CMake configuration of ITK.
To permanently have your filter use the classic threading model,
invoke `this->DynamicMultiThreadingOff();` in the filter constructor.
That is required if any of the following is true:
 * Your filter needs a constant number of threads (known in advance)
 * Your filter uses `threadId` parameter in `ThreadedGenerateData()`
 * Your filter uses a custom region splitting method

Additionally, replace `MultiThreader` by PlatformMultiThreader
if any of the following is true:
 * Your filter uses cross-thread synchronization e.g. itkBarrier
 * Your filter uses MultipleMethodExecute()
 * Your filter uses SpawnThread/TerminateThread

It is stronly advised not to explicitly use PlatformMultiThreader.
SpawnThread/TerminateThread and MultipleMethodExecute can be
replaced by C++11 `std::thread`. And below code example shows
how to remove dependence on barrier by using ParallelizeImageRegion.

- Pattern with Barrier:
```C++
ThreadedGenerateData()
{
//code1 (parallel)
myBarrier->Wait();
if (threadId==0)
  //code2 single-threaded
//code3 (parallel)
}
```
after refactoring to not use barrier:
```C++
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

- Pattern with Arrays:
If you are storing the results of threading computations in an `Array`,
you might use instead `std::atomic`.
You can see an example of this for an external module in
[this commit](https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry/pull/32/commits/a8014c186ac53837362a0cb9db46ae224b8e9584).
Before, using `Array`:
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

Get/Set GlobalMaximumNumberOfThreads and GlobalDefaultNumberOfThreads
now reside in `MultiThreaderBase`. With a warning, they are still
available in `PlatformMultiThreader`. The common case of
`innerFilter->SetNumberOfThreads(1);` should be replaced by
`innerFilter->SetNumberOfWorkUnits(1);`.

To transition to the new threading model, it is usually enough to rename
`ThreadedGenerateData` into `DynamicThreadedGenerateData`, remove the
`threadId` parameter, and remove progress reporting which uses `threadId`.
Progress is being reported by Multithreaders on behalf of filters which
use `DynamicThreadedGenerateData` signature.

If your class needs to also work with legacy code where
`ITKV4_COMPATIBILITY` is enabled, invoke
`this->DynamicMultiThreadingOn();` in the filter constructor. An example of
external module transitioning to the new threading model can be found in
[this commit](https://github.com/InsightSoftwareConsortium/ITKTextureFeatures/commit/f794baa7546f9bb8b7d89ae3a083c9a432d55df0).

Class changes
-------------

[FilterWatcher](../Modules/Core/TestKernel/include/itkFilterWatcher.h) was deleted.
It should be replaced by [itk::SimpleFilterWatcher](../Modules/Core/Common/include/itkSimpleFilterWatcher.h).

Update scripts
--------------

[Utilities/ITKv5Preparation](../Utilities/ITKv5Preparation/) directory contains
bash scripts which have been used to update ITK to version 5. These scripts
can assist with updating external code bases to ITK v5 content and style.
