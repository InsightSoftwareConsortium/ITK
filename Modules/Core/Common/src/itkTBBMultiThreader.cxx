/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include <iostream>
#include <atomic>
#include <thread>
#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"

namespace itk
{

TBBMultiThreader::TBBMultiThreader()
{
  ThreadIdType defaultThreads = std::max(1u, GetGlobalDefaultNumberOfThreads());
#if defined(ITKV4_COMPATIBILITY)
  m_NumberOfWorkUnits = defaultThreads;
#else
  if (defaultThreads > 1) // one work unit for only one thread
  {
    m_NumberOfWorkUnits = 16 * defaultThreads;
  }
#endif
}

TBBMultiThreader::~TBBMultiThreader() {}

void
TBBMultiThreader::SetSingleMethod(ThreadFunctionType f, void * data)
{
  m_SingleMethod = f;
  m_SingleData = data;
}

void
TBBMultiThreader::SingleMethodExecute()
{
  if (!m_SingleMethod)
  {
    itkExceptionMacro(<< "No single method set!");
  }

  tbb::task_scheduler_init tbb_init(m_MaximumNumberOfThreads);
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
TBBMultiThreader ::ParallelizeArray(SizeValueType             firstIndex,
                                    SizeValueType             lastIndexPlus1,
                                    ArrayThreadingFunctorType aFunc,
                                    ProcessObject *           filter)
{
  MultiThreaderBase::HandleFilterProgress(filter, 0.0f);

  if (firstIndex + 1 < lastIndexPlus1)
  {
    unsigned                   count = lastIndexPlus1 - firstIndex;
    std::atomic<SizeValueType> progress(0);
    std::thread::id            callingThread = std::this_thread::get_id();
    tbb::task_scheduler_init   tbb_init(m_MaximumNumberOfThreads);
    // we request grain size of 1 and simple_partitioner to ensure there is no chunking
    tbb::parallel_for(
      tbb::blocked_range<SizeValueType>(firstIndex, lastIndexPlus1, 1),
      [&](tbb::blocked_range<SizeValueType> r) {
        // Make sure that TBB did not call us with a block of "threads"
        // but rather with only one "thread" to handle
        itkAssertInDebugAndIgnoreInReleaseMacro(r.begin() + 1 == r.end());
        MultiThreaderBase::HandleFilterProgress(filter);

        aFunc(r.begin()); // invoke the function

        if (filter)
        {
          ++progress;
          // make sure we are updating progress only from the thead which invoked us
          if (callingThread == std::this_thread::get_id())
          {
            filter->UpdateProgress(float(progress) / count);
          }
        }
      },
      tbb::simple_partitioner());
  }
  else if (firstIndex + 1 == lastIndexPlus1)
  {
    aFunc(firstIndex);
  }
  // else nothing needs to be executed

  MultiThreaderBase::HandleFilterProgress(filter, 1.0f);
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
  TBBImageRegionSplitter(const itk::ImageIORegion & region)
    : itk::ImageIORegion(region) // use itk::ImageIORegion's copy constructor
  {}
  TBBImageRegionSplitter(TBBImageRegionSplitter & region, tbb::split)
    : TBBImageRegionSplitter(region, tbb::proportional_split(1, 1)) // delegate to proportional split
  {}

  TBBImageRegionSplitter(TBBImageRegionSplitter & region, tbb::proportional_split p)
  {
    // The following if statement is primarily used to ensure use of
    // is_splittable_in_proportion to avoid unused variable warning.
    if (TBBImageRegionSplitter::is_splittable_in_proportion == true)
    {
      *this = region;                                               // most things will be the same
      for (int d = int(this->GetImageDimension()) - 1; d >= 0; d--) // prefer to split along highest dimension
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

  bool
  empty() const
  {
    for (unsigned d = 0; d < this->GetImageDimension(); d++)
    {
      if (this->GetSize(d) == 0)
      {
        return true;
      }
    }
    return false;
  }

  bool
  is_divisible() const
  {
    for (unsigned d = 0; d < this->GetImageDimension(); d++)
    {
      if (this->GetSize(d) > 1)
      {
        return true;
      }
    }
    return false;
  }

}; // TBBImageRegionSplitter struct definition
} // anonymous namespace

namespace itk
{
void
TBBMultiThreader ::ParallelizeImageRegion(unsigned int         dimension,
                                          const IndexValueType index[],
                                          const SizeValueType  size[],
                                          ThreadingFunctorType funcP,
                                          ProcessObject *      filter)
{
  MultiThreaderBase::HandleFilterProgress(filter, 0.0f);

  if (m_NumberOfWorkUnits == 1) // no multi-threading wanted
  {
    funcP(index, size);
  }
  else // normal multi-threading
  {
    ImageIORegion region(dimension);
    for (unsigned d = 0; d < dimension; d++)
    {
      region.SetIndex(d, index[d]);
      region.SetSize(d, size[d]);
    }
    TBBImageRegionSplitter regionSplitter = region; // use copy constructor

    std::atomic<SizeValueType> pixelProgress = { 0 };
    SizeValueType              totalCount = region.GetNumberOfPixels();
    std::thread::id            callingThread = std::this_thread::get_id();
    tbb::task_scheduler_init   tbb_init(m_MaximumNumberOfThreads);
    tbb::parallel_for(regionSplitter, [&](TBBImageRegionSplitter regionToProcess) {
      MultiThreaderBase::HandleFilterProgress(filter);
      funcP(&regionToProcess.GetIndex()[0], &regionToProcess.GetSize()[0]);
      if (filter) // filter is provided, update progress
      {
        SizeValueType pixelCount = regionToProcess.GetNumberOfPixels();
        pixelProgress += pixelCount;
        // make sure we are updating progress only from the thead which invoked filter->Update();
        if (callingThread == std::this_thread::get_id())
        {
          filter->UpdateProgress(float(pixelProgress) / totalCount);
        }
      }
    }); // we implicitly use auto_partitioner for load balancing
  }

  MultiThreaderBase::HandleFilterProgress(filter, 1.0f);
}
} // namespace itk
