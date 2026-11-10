/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkTBBMultiThreader.h"
#include "itkNumericTraits.h"
#include "itkProcessObject.h"
#include "itkTotalProgressReporter.h"
#include <iostream>
#include <atomic>
#include <limits>
#include <thread>
#include "tbb/parallel_for.h"

#include "tbb/global_control.h"

namespace
{
// From tbb/examples/common/utility/get_default_num_threads.h
namespace tbb_utility
{
int
get_default_num_threads()
{
#if __TBB_MIC_OFFLOAD
#  pragma offload target(mic) out(default_num_threads)
#endif // __TBB_MIC_OFFLOAD
  const static size_t default_num_threads =
    tbb::global_control::active_value(tbb::global_control::max_allowed_parallelism);
  return static_cast<int>(default_num_threads);
}
} // namespace tbb_utility

// Target chunk size: about one chunk per work unit.
itk::SizeValueType
ChunkGrain(itk::SizeValueType count, itk::ThreadIdType numberOfWorkUnits)
{
  return std::max<itk::SizeValueType>(1, count / std::max(1u, numberOfWorkUnits));
}
} // namespace

namespace itk
{

TBBMultiThreader::TBBMultiThreader()
{
  ThreadIdType defaultThreads = std::max(1u, GetGlobalDefaultNumberOfThreads());
  if (defaultThreads > 1) // one work unit for only one thread
  {
    const auto workUnitPerThreadRatio = GetGlobalDefaultWorkUnitPerThreadRatio();
    // Prevent overflow when multiplying defaultThreads by workUnitPerThreadRatio
    m_NumberOfWorkUnits = defaultThreads > std::numeric_limits<ThreadIdType>::max() / workUnitPerThreadRatio
                            ? std::numeric_limits<ThreadIdType>::max()
                            : defaultThreads * workUnitPerThreadRatio;
  }
}

TBBMultiThreader::~TBBMultiThreader() = default;

void
TBBMultiThreader::SetSingleMethod(ThreadFunctionType f, void * data)
{
  m_SingleMethod = std::move(f);
  m_SingleData = data;
}

void
TBBMultiThreader::SingleMethodExecute()
{
  if (!m_SingleMethod)
  {
    itkExceptionStringMacro("No single method set!");
  }

  // Construct TBB static context with only m_MaximumNumberOfThreads threads
  // https://software.intel.com/en-us/node/589744
  // Total parallelism that TBB can utilize
  // is limited by the current active global_control object
  // for the dynamic extension of the given scope.
  // ( instantiation of "global_control" object pushes the
  //   value onto active head of the FIFO stack for 'max_allowed_parallelism'
  //   type, and destruction of the "global_control" object pops
  //   the 'max_allowed_parallelism' type and returns the active value
  //   to its original state.
  tbb::global_control l_SingleMethodExecute_tbb_global_context(
    tbb::global_control::max_allowed_parallelism,
    std::min<int>(tbb_utility::get_default_num_threads(), m_MaximumNumberOfThreads));

  // we request grain size of 1 and simple_partitioner to ensure there is no chunking
  tbb::parallel_for(
    tbb::blocked_range<int>(0, m_NumberOfWorkUnits, 1),
    [&](tbb::blocked_range<int> r) {
      // Make sure that TBB did not call us with a block of work units
      // but rather with only one work unit to handle
      itkAssertInDebugAndIgnoreInReleaseMacro(r.begin() + 1 == r.end());

      WorkUnitInfo ti;
      ti.WorkUnitID = r.begin();
      ti.UserData = m_SingleData;
      ti.NumberOfWorkUnits = m_NumberOfWorkUnits;
      m_SingleMethod(&ti); // TBB takes care of properly propagating exceptions
    },
    tbb::simple_partitioner());
}

void
TBBMultiThreader::SetNumberOfWorkUnits(ThreadIdType numberOfWorkUnits)
{
  m_NumberOfWorkUnits = std::max(1u, numberOfWorkUnits);
}

void
TBBMultiThreader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

void
TBBMultiThreader::ParallelizeArray(SizeValueType             firstIndex,
                                   SizeValueType             lastIndexPlus1,
                                   ArrayThreadingFunctorType aFunc,
                                   ProcessObject *           filter)
{
  if (!this->GetUpdateProgress())
  {
    filter = nullptr;
  }
  ProgressReporter progressStartEnd(filter, 0, 1);

  if (firstIndex + 1 < lastIndexPlus1)
  {
    const SizeValueType count = lastIndexPlus1 - firstIndex;
    tbb::global_control l_ParallelizeArray_tbb_global_context(
      tbb::global_control::max_allowed_parallelism,
      std::min<int>(tbb_utility::get_default_num_threads(), m_MaximumNumberOfThreads));

    tbb::parallel_for(
      tbb::blocked_range<SizeValueType>(firstIndex, lastIndexPlus1, ChunkGrain(count, m_NumberOfWorkUnits)),
      [&](const tbb::blocked_range<SizeValueType> & r) {
        TotalProgressReporter progress(filter, count, 100);
        progress.CheckAbortGenerateData();
        for (SizeValueType i = r.begin(); i < r.end(); ++i)
        {
          aFunc(i);
          progress.CompletedPixel();
        }
      },
      tbb::simple_partitioner());
  }
  else if (firstIndex + 1 == lastIndexPlus1)
  {
    aFunc(firstIndex);
  }
}

} // namespace itk

namespace
{
struct TBBImageRegionSplitter : public itk::ImageIORegion
{
  // Optional. If true, the proportional splitting constructor
  // is defined for the range and may be used by parallel algorithms.
  // https://software.intel.com/en-us/node/506143
  // This variable must exist for tbb to properly identify and use this
  // specialization.
  static constexpr bool is_splittable_in_proportion = true;
  TBBImageRegionSplitter(const TBBImageRegionSplitter &) = default;
  TBBImageRegionSplitter &
  operator=(const TBBImageRegionSplitter &) = default;
  TBBImageRegionSplitter(const itk::ImageIORegion & region, itk::SizeValueType grain)
    : itk::ImageIORegion(region) // use itk::ImageIORegion's copy constructor
    , m_Grain(grain)
  {
    for (unsigned int d = 1; d < this->GetImageDimension(); ++d)
    {
      m_KeepRowsWhole = m_KeepRowsWhole || this->GetSize(d) > 1;
    }
  }
  TBBImageRegionSplitter(TBBImageRegionSplitter & region, tbb::split)
    : TBBImageRegionSplitter(region, tbb::proportional_split(1, 1)) // delegate to proportional split
  {}

  TBBImageRegionSplitter(TBBImageRegionSplitter & region, tbb::proportional_split p)
  {
    // The following if statement is primarily used to ensure use of
    // is_splittable_in_proportion to avoid unused variable warning.
    if (TBBImageRegionSplitter::is_splittable_in_proportion)
    {
      *this = region; // most things will be the same
      for (int d = static_cast<int>(this->GetImageDimension()) - 1; d >= 0;
           d--) // prefer to split along highest dimension
      {
        if (this->GetSize(d) > 1) // split along this dimension
        {
          size_t myP = (this->GetSize(d) * p.right()) / (p.left() + p.right());
          if (myP == 0)
          {
            ++myP;
          }
          else if (myP == this->GetSize(d))
          {
            --myP;
          }
          this->SetSize(d, myP);
          region.SetSize(d, region.GetSize(d) - myP);
          region.SetIndex(d, myP + region.GetIndex(d));
          return;
        }
      }
    }
    itkGenericExceptionMacro("An ImageIORegion could not be split. Region: " << region);
  }

  [[nodiscard]] bool
  empty() const
  {
    for (unsigned int d = 0; d < this->GetImageDimension(); ++d)
    {
      if (this->GetSize(d) == 0)
      {
        return true;
      }
    }
    return false;
  }

  [[nodiscard]] bool
  is_divisible() const
  {
    if (this->GetNumberOfPixels() < 2 * m_Grain)
    {
      return false;
    }
    for (unsigned int d = m_KeepRowsWhole ? 1 : 0; d < this->GetImageDimension(); ++d)
    {
      if (this->GetSize(d) > 1)
      {
        return true;
      }
    }
    return false;
  }

  itk::SizeValueType m_Grain{ 1 };
  bool               m_KeepRowsWhole{ false };
}; // TBBImageRegionSplitter struct definition
} // anonymous namespace

namespace itk
{
void
TBBMultiThreader::ParallelizeImageRegion(unsigned int         dimension,
                                         const IndexValueType index[],
                                         const SizeValueType  size[],
                                         ThreadingFunctorType funcP,
                                         ProcessObject *      filter)
{
  if (!this->GetUpdateProgress())
  {
    filter = nullptr;
  }
  ProgressReporter progressStartEnd(filter, 0, 1);

  if (m_NumberOfWorkUnits == 1)
  {
    funcP(index, size);
  }
  else
  {
    ImageIORegion region(dimension);
    for (unsigned int d = 0; d < dimension; ++d)
    {
      region.SetIndex(d, index[d]);
      region.SetSize(d, size[d]);
    }
    const SizeValueType    totalCount = region.GetNumberOfPixels();
    TBBImageRegionSplitter regionSplitter(region, ChunkGrain(totalCount, m_NumberOfWorkUnits));

    tbb::global_control l_ParallelizeImageRegion_tbb_global_context(
      tbb::global_control::max_allowed_parallelism,
      std::min<int>(tbb_utility::get_default_num_threads(), m_MaximumNumberOfThreads));

    tbb::parallel_for(regionSplitter, [&](TBBImageRegionSplitter regionToProcess) {
      TotalProgressReporter progress(filter, totalCount, 100);
      progress.CheckAbortGenerateData();

      funcP(&regionToProcess.GetIndex()[0], &regionToProcess.GetSize()[0]);

      progress.Completed(regionToProcess.GetNumberOfPixels());
    }); // we implicitly use auto_partitioner for load balancing
  }
}
} // namespace itk
