/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteDifferenceSparseImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

     =========================================================================*/
#ifndef __itkFiniteDifferenceSparseImageFilter_txx_
#define __itkFiniteDifferenceSparseImageFilter_txx_ 

#include "itkFiniteDifferenceSparseImageFilter.h"

namespace itk {

template <class TInputImageType, class TSparseOutputImageType>
FiniteDifferenceSparseImageFilter <TInputImageType, TSparseOutputImageType>
::FiniteDifferenceSparseImageFilter()
{
  m_SparseFunction = 0;
  m_PrecomputeFlag = false;
}

template <class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter <TInputImageType, TSparseOutputImageType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PrecomputeFlag: " << m_PrecomputeFlag << std::endl;
}

template <class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter <TInputImageType, TSparseOutputImageType>
::SetSparseFunction( SparseFunctionType *sf )
{
  m_SparseFunction = sf;
  Superclass::SetDifferenceFunction (sf);
}

template <class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter <TInputImageType, TSparseOutputImageType>
::Initialize()
{
  m_RegionList=(this->GetOutput()->GetNodeList())
    ->SplitRegions(this->GetNumberOfThreads());
  // The active set of pixels in the sparse image is split into multi-threading
  // regions once here for computationally efficiency.
  // Later GetSplitRegions is used to access these partitions. 
  // This assumes that the active will not be changed until another
  // call to Initialize(). Any reinitialization function also must call the
  // SplitRegions function.
}

template <class TInputImageType, class TSparseOutputImageType>
int 
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::GetSplitRegion( int i, int num, ThreadRegionType &splitRegion ) 
{
  splitRegion.first = m_RegionList[i].first;
  splitRegion.last = m_RegionList[i].last;
  return num;
  // check this last line with the ITKdevelopers. not sure what this is doing
  // copied it from FiniteDifferenceImageFilter class
}

template<class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::ApplyUpdate( TimeStepType dt )
{
  // Set up for multithreaded processing.
  FDThreadStruct str;
  str.Filter = this;
  str.TimeStep = dt;
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->ApplyUpdateThreaderCallback,
                                            &str);
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();
}
 
template<class TInputImageType, class TSparseOutputImageType>
ITK_THREAD_RETURN_TYPE
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::ApplyUpdateThreaderCallback( void * arg )
{
  FDThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;
  str = (FDThreadStruct *)
    (((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Use GetSplitRegion to access partition previously computed by
  // the SplitRegions function in the SparseFieldLayer class.
  ThreadRegionType splitRegion;
  total = str->Filter->GetSplitRegion(threadId, threadCount, splitRegion);
  
  if (threadId < total)
    {
      str->Filter->ThreadedApplyUpdate(str->TimeStep, splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::ThreadedApplyUpdate( TimeStepType dt, const ThreadRegionType &regionToProcess,
                       int)
{
  typename NodeListType::Iterator it;
  
  for (it=regionToProcess.first; it != regionToProcess.last; ++it)
    {
    // all sparse image node types must have Data and Update members to be used
    // with this filter 
    it->m_Data = this->DataConstraint (it->m_Data +
                                       it->m_Update * dt);
    }
}

template <class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::PrecalculateChange()
{
  // Set up for multithreaded processing.
  FDThreadStruct str;
  str.Filter = this;
  
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod
    (this->PrecalculateChangeThreaderCallback,&str);
  
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();
}

template <class TInputImageType, class TSparseOutputImageType>
typename FiniteDifferenceSparseImageFilter <TInputImageType,
                                            TSparseOutputImageType>::TimeStepType
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::CalculateChange()
{
  if (m_PrecomputeFlag == true)
    {
    this->PrecalculateChange();
    }
  int threadCount;
  TimeStepType dt;

  // Set up for multithreaded processing.
  FDThreadStruct str;
  str.Filter = this;
  str.TimeStep = NumericTraits<TimeStepType>::Zero;
  // Not used during the calculate change step for normals. 

  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod
    (this->CalculateChangeThreaderCallback,&str);

  // Initialize the list of time step values that will be generated by the
  // various threads.  There is one distinct slot for each possible thread,
  // so this data structure is thread-safe.  All of the time steps calculated
  // in each thread will be combined in the ResolveTimeStepMethod.
  threadCount = this->GetMultiThreader()->GetNumberOfThreads();  
  str.TimeStepList = new TimeStepType[threadCount];                 
  str.ValidTimeStepList = new bool[threadCount];
  for (int i =0; i < threadCount; ++i)
    {
    str.ValidTimeStepList[i] = false;
    } 

  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Resolve the single value time step to return.  The default implementation
  // of ResolveTimeStep is to return the lowest value in the list that it is
  // given.  
  dt = this->ResolveTimeStep(str.TimeStepList,
                             str.ValidTimeStepList, threadCount);
  
  delete [] str.TimeStepList;
  delete [] str.ValidTimeStepList;

  return  dt;
}

template <class TInputImageType, class TSparseOutputImageType>
ITK_THREAD_RETURN_TYPE
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::CalculateChangeThreaderCallback( void * arg )
{
  FDThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;
  
  str = (FDThreadStruct *)
    (((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Use GetSplitRegion to access partition previously computed by
  // the Splitegions function in the SparseFieldLayer class.
  ThreadRegionType splitRegion;
  total = str->Filter->GetSplitRegion(threadId, threadCount, splitRegion);
  
  if (threadId < total)
    { 
      str->TimeStepList[threadId]
        = str->Filter->ThreadedCalculateChange(splitRegion, threadId);
      str->ValidTimeStepList[threadId] = true;
    }

  return ITK_THREAD_RETURN_VALUE;  
}

template <class TInputImageType, class TSparseOutputImageType>
ITK_THREAD_RETURN_TYPE
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::PrecalculateChangeThreaderCallback( void * arg )
{
  FDThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;
  
  str = (FDThreadStruct *)
    (((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Use GetSplitRegion to access partition previously computed by
  // the Splitegions function in the SparseFieldLayer class.
  ThreadRegionType splitRegion;
  total = str->Filter->GetSplitRegion(threadId, threadCount, splitRegion);
  
  if (threadId < total)
    { 
    str->Filter->ThreadedPrecalculateChange(splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;  
}

template <class TInputImageType, class TSparseOutputImageType>
typename FiniteDifferenceSparseImageFilter<TInputImageType,
                                           TSparseOutputImageType>::TimeStepType
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::ThreadedCalculateChange( const ThreadRegionType &regionToProcess, int )
{
  typedef typename SparseOutputImageType::SizeType   SizeType;
  typedef typename SparseOutputImageType::IndexType  IndexType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodType
    NeighborhoodIteratorType;
  
  typename SparseOutputImageType::Pointer output = this->GetOutput();

  TimeStepType timeStep;
  void *globalData;

  const SizeType  radius = m_SparseFunction->GetRadius();
  
  // Ask the function object for a pointer to a data structure it will use to
  // manage any global values it needs.  We'll pass this back to the function
  // object at each calculation so that the function object can use it to
  // determine a time step for this iteration.
  globalData = m_SparseFunction->GetGlobalDataPointer();
  
  typename NodeListType::Iterator bandIt;
  NeighborhoodIteratorType outputIt(radius, output,
                                    output->GetRequestedRegion());

  // compute the update variables
  for (bandIt = regionToProcess.first; bandIt != regionToProcess.last; ++bandIt)
    {
    outputIt.SetLocation (bandIt->m_Index);
    outputIt.GetCenterPixel()->m_Update =
      m_SparseFunction->ComputeSparseUpdate(outputIt, globalData);
    }
  
  // Ask the finite difference function to compute the time step for
  // this iteration.  We give it the global data pointer to use, then
  // ask it to free the global data memory. 
  timeStep = m_SparseFunction->ComputeGlobalTimeStep(globalData);
  m_SparseFunction->ReleaseGlobalDataPointer(globalData);

  return timeStep;
}

template <class TInputImageType, class TSparseOutputImageType>
void
FiniteDifferenceSparseImageFilter<TInputImageType, TSparseOutputImageType>
::ThreadedPrecalculateChange( const ThreadRegionType &regionToProcess, int )
{
  typedef typename SparseOutputImageType::SizeType   SizeType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodType
    NeighborhoodIteratorType;

  typename SparseOutputImageType::Pointer output = this->GetOutput();

  const SizeType  radius = m_SparseFunction->GetRadius();
  
  typename NodeListType::Iterator bandIt;
  NeighborhoodIteratorType outputIt(radius, output,
                                    output->GetRequestedRegion());

  // the step for computing the flux variables
  // these are used for computing the update in diffusion processes
  // can disable these lines for non-diffusion processes
  for (bandIt = regionToProcess.first; bandIt != regionToProcess.last; ++bandIt)
    {
    outputIt.SetLocation(bandIt->m_Index);
    m_SparseFunction->PrecomputeSparseUpdate(outputIt);
    }

}

} // end namespace itk

#endif
