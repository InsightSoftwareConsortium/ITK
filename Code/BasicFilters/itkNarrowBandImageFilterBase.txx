/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBandImageFilterBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNarrowBandImageFilterBase_txx_
#define __itkNarrowBandImageFilterBase_txx_
#include "itkNarrowBandImageFilterBase.h"
#include "itkShiftScaleImageFilter.h"

#define INNER_MASK 2

namespace itk {


template <class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::ClearNarrowBand () 
{
  while (!m_NarrowBand->Empty()) 
    {
      m_NarrowBand->Clear();
    }
}

template <class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::CopyInputToOutput()
{

  //   First need to subtract the iso-surface value from the input image.
  typedef ShiftScaleImageFilter<InputImageType, OutputImageType> ShiftScaleFilterType;
  typename ShiftScaleFilterType::Pointer shiftScaleFilter = ShiftScaleFilterType::New();
  shiftScaleFilter->SetInput( this->GetInput()  );
  shiftScaleFilter->SetShift( - m_IsoSurfaceValue );
  shiftScaleFilter->Update();  
  this->GraftOutput(shiftScaleFilter->GetOutput());

}


template <class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::Initialize () 
{

  m_Step = 0;

  ClearNarrowBand(); 
  CreateNarrowBand();
    
  // SetNarrowBand is expected to be defined in a subclass.
  // It should use the InsertNarrowBandNode function, which takes care of 
  // memory management issues, to create the desired narrow band. 
  
  m_RegionList=m_NarrowBand->SplitBand(this->GetNumberOfThreads());
  
  // The narrow band is split into multi-threading regions once here for
  // computationally efficiency. Later GetSplitRegions is used to access these
  // partitions. This assumes that the band will not be changed until another
  // call to Initialize(). Any reinitialization function also must call the
  // SplitRegions function.

}

template <class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::InitializeIteration()
{
  
//Check if we have to reinitialize the narrowband
  if (m_Touched || ((this->GetElapsedIterations() >0) &&(this->m_Step == m_ReinitializationFrequency )))
    {
    //Reinitialize the narrowband properly
    CreateNarrowBand();
    m_Step=0;
    m_Touched = false;
    }
}


template<class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::ApplyUpdate(TimeStepType dt)
{
  // Set up for multithreaded processing.
  NarrowBandFDThreadStruct str;
  str.Filter = this;
  str.TimeStep = dt;
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->ApplyUpdateThreaderCallback,
                                            &str);
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();
  m_Step++;
}
 
template<class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::ApplyUpdateThreaderCallback( void * arg )
{
  NarrowBandFDThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;

  str = (NarrowBandFDThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

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

template <class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::ThreadedApplyUpdate(TimeStepType dt, const ThreadRegionType &regionToProcess,
                           int threadId)
{
  typename NarrowBandType::ConstIterator it;
  typename OutputImageType::Pointer image=this->GetOutput();
  typename OutputImageType::PixelType oldvalue;
  typename OutputImageType::PixelType newvalue;
  for (it=regionToProcess.first; it != regionToProcess.last; ++it)
    {
      oldvalue = image->GetPixel(it->m_Index);
      newvalue = oldvalue + dt * it->m_Data;
      //Check whether solution is out the inner band or not
      m_Touched = ((m_Touched) || ( !(it->m_NodeState & INNER_MASK)
                                    && ( (oldvalue>0)!=(newvalue>0))));
      image->SetPixel(it->m_Index, image->GetPixel(it->m_Index)+dt * it->m_Data);
         
    }
}

template <class TInputImage, class TOutputImage>
int 
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::GetSplitRegion (int i, int num, ThreadRegionType &splitRegion)
{
  splitRegion.first = m_RegionList[i].Begin;
  splitRegion.last = m_RegionList[i].End;
  return num; // check this with the ITKdevelopers. not sure if it is correct! 
}


template <class TInputImage, class TOutputImage>
typename
NarrowBandImageFilterBase<TInputImage, TOutputImage>::TimeStepType
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::CalculateChange()
{
  int threadCount;
  TimeStepType dt;

  // Set up for multithreaded processing.
  NarrowBandFDThreadStruct str;
  str.Filter = this;
  str.TimeStep = NumericTraits<TimeStepType>::Zero;  // Not used during the
                                                     // calculate change step.
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->CalculateChangeThreaderCallback,
                                            &str);

  // Initialize the list of time step values that will be generated by the
  // various threads.  There is one distinct slot for each possible thread,
  // so this data structure is thread-safe.  All of the time steps calculated
  // in each thread will be combined in the ResolveTimeStepMethod.
  threadCount = this->GetMultiThreader()->GetNumberOfThreads();  
  str.TimeStepList = new TimeStepType[threadCount];                 
  str.ValidTimeStepList = new bool[threadCount];
  for (int i =0; i < threadCount; ++i)
    {      str.ValidTimeStepList[i] = false;    } 

  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Resolve the single value time step to return.  The default implementation
  // of ResolveTimeStep is to return the lowest value in the list that it is
  // given.  This method can be overridden to create another implementation.
  dt = this->ResolveTimeStep(str.TimeStepList, str.ValidTimeStepList, threadCount);
  delete [] str.TimeStepList;
  delete [] str.ValidTimeStepList;

  return  dt;
}

template <class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::CalculateChangeThreaderCallback( void * arg )
{
  NarrowBandFDThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;

  str = (NarrowBandFDThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

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

template <class TInputImage, class TOutputImage>
typename
NarrowBandImageFilterBase<TInputImage, TOutputImage>::TimeStepType
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::ThreadedCalculateChange(const ThreadRegionType &regionToProcess, int threadId)
{
  typedef typename OutputImageType::RegionType RegionType;
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::SizeValueType   SizeValueType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::IndexValueType  IndexValueType;

  typedef typename FiniteDifferenceFunctionType::NeighborhoodType
    NeighborhoodIteratorType;
  
  typename OutputImageType::Pointer output = this->GetOutput();
  TimeStepType timeStep;
  void *globalData;

  // Get the FiniteDifferenceFunction to use in calculations.
  const typename FiniteDifferenceFunctionType::Pointer df
    = this->GetDifferenceFunction();
  const SizeType  radius = df->GetRadius();
  
  // Ask the function object for a pointer to a data structure it will use to
  // manage any global values it needs.  We'll pass this back to the function
  // object at each calculation so that the function object can use it to
  // determine a time step for this iteration.
  globalData = df->GetGlobalDataPointer();

  typename NarrowBandType::Iterator bandIt;
  NeighborhoodIteratorType outputIt(radius, output, output->GetRequestedRegion());

  for (bandIt = regionToProcess.first; bandIt != regionToProcess.last; ++bandIt)
    {
      outputIt.SetLocation(bandIt->m_Index);
      bandIt->m_Data = df->ComputeUpdate(outputIt, globalData);
    }
  
  // Ask the finite difference function to compute the time step for
  // this iteration.  We give it the global data pointer to use, then
  // ask it to free the global data memory.
  timeStep = df->ComputeGlobalTimeStep(globalData);
  df->ReleaseGlobalDataPointer(globalData);

  return timeStep;
}

template <class TInputImage, class TOutputImage>
void
NarrowBandImageFilterBase<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}// end namespace itk

#endif
