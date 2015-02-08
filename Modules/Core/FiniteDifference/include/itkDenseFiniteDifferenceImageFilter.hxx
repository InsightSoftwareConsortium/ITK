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
#ifndef itkDenseFiniteDifferenceImageFilter_hxx
#define itkDenseFiniteDifferenceImageFilter_hxx
#include "itkDenseFiniteDifferenceImageFilter.h"

#include <list>
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::CopyInputToOutput()
{
  typename TInputImage::ConstPointer input  = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();

  if ( !input || !output )
    {
    itkExceptionMacro(<< "Either input and/or output is ITK_NULLPTR.");
    }

  // Check if we are doing in-place filtering
  if ( this->GetInPlace() && this->CanRunInPlace() )
    {
    typename TInputImage::Pointer tempPtr =
      dynamic_cast< TInputImage * >( output.GetPointer() );
    if ( tempPtr && tempPtr->GetPixelContainer() == input->GetPixelContainer() )
      {
      // the input and output container are the same - no need to copy
      return;
      }
    }

  ImageRegionConstIterator< TInputImage > in( input, output->GetRequestedRegion() );
  ImageRegionIterator< TOutputImage >     out( output, output->GetRequestedRegion() );

  while ( !out.IsAtEnd() )
    {
    out.Value() =  static_cast< PixelType >( in.Get() );  // Supports input
                                                          // image adaptors only
    ++in;
    ++out;
    }
}

template< typename TInputImage, typename TOutputImage >
void
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::AllocateUpdateBuffer()
{
  // The update buffer looks just like the output.
  typename TOutputImage::Pointer output = this->GetOutput();

  m_UpdateBuffer->SetOrigin( output->GetOrigin() );
  m_UpdateBuffer->SetSpacing( output->GetSpacing() );
  m_UpdateBuffer->SetDirection( output->GetDirection() );
  m_UpdateBuffer->SetLargestPossibleRegion( output->GetLargestPossibleRegion() );
  m_UpdateBuffer->SetRequestedRegion( output->GetRequestedRegion() );
  m_UpdateBuffer->SetBufferedRegion( output->GetBufferedRegion() );
  m_UpdateBuffer->Allocate();
}

template< typename TInputImage, typename TOutputImage >
void
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::ApplyUpdate(const TimeStepType& dt)
{
  // Set up for multithreaded processing.
  DenseFDThreadStruct str;

  str.Filter = this;
  str.TimeStep = dt;
  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ApplyUpdateThreaderCallback,
                                            &str);
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Explicitely call Modified on GetOutput here
  // since ThreadedApplyUpdate changes this buffer
  // through iterators which do not increment the
  // output timestamp
  this->GetOutput()->Modified();
}

template< typename TInputImage, typename TOutputImage >
ITK_THREAD_RETURN_TYPE
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::ApplyUpdateThreaderCallback(void *arg)
{
  ThreadIdType threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  ThreadIdType threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  DenseFDThreadStruct* str = (DenseFDThreadStruct *)
      ( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  ThreadRegionType splitRegion;
  ThreadIdType total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);

  if ( threadId < total )
    {
    str->Filter->ThreadedApplyUpdate(str->TimeStep, splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TInputImage, typename TOutputImage >
typename
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >::TimeStepType
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::CalculateChange()
{
  // Set up for multithreaded processing.
  DenseFDThreadStruct str;

  str.Filter = this;
  str.TimeStep = NumericTraits< TimeStepType >::ZeroValue();  // Not used during the
  // calculate change step.
  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->CalculateChangeThreaderCallback,
                                            &str);

  // Initialize the list of time step values that will be generated by the
  // various threads.  There is one distinct slot for each possible thread,
  // so this data structure is thread-safe.
  ThreadIdType threadCount = this->GetMultiThreader()->GetNumberOfThreads();

  str.TimeStepList.clear();
  str.TimeStepList.resize( threadCount, NumericTraits< TimeStepType >::ZeroValue() );

  str.ValidTimeStepList.clear();
  str.ValidTimeStepList.resize( threadCount, false );

  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Resolve the single value time step to return
  TimeStepType dt = this->ResolveTimeStep( str.TimeStepList,
                                           str.ValidTimeStepList );

  // Explicitely call Modified on m_UpdateBuffer here
  // since ThreadedCalculateChange changes this buffer
  // through iterators which do not increment the
  // update buffer timestamp
  this->m_UpdateBuffer->Modified();

  return dt;
}

template< typename TInputImage, typename TOutputImage >
ITK_THREAD_RETURN_TYPE
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::CalculateChangeThreaderCallback(void *arg)
{
  ThreadIdType threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  ThreadIdType threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  DenseFDThreadStruct * str = (DenseFDThreadStruct *)
      ( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  ThreadRegionType splitRegion;

  ThreadIdType total = str->Filter->SplitRequestedRegion( threadId,
                                                 threadCount,
                                                 splitRegion );

  if ( threadId < total )
    {
    str->TimeStepList[threadId] =
      str->Filter->ThreadedCalculateChange(splitRegion, threadId);
    str->ValidTimeStepList[threadId] = true;
    }

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TInputImage, typename TOutputImage >
void
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::ThreadedApplyUpdate(const TimeStepType& dt,
                      const ThreadRegionType & regionToProcess,
                      ThreadIdType)
{
  ImageRegionIterator< UpdateBufferType > u(m_UpdateBuffer,    regionToProcess);
  ImageRegionIterator< OutputImageType >  o(this->GetOutput(), regionToProcess);

  u.GoToBegin();
  o.GoToBegin();

  while ( !u.IsAtEnd() )
    {
    o.Value() += static_cast< PixelType >( u.Value() * dt );  // no adaptor
                                                              // support here
    ++o;
    ++u;
    }
}

template< typename TInputImage, typename TOutputImage >
typename
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >::TimeStepType
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::ThreadedCalculateChange(const ThreadRegionType & regionToProcess, ThreadIdType)
{
  typedef typename OutputImageType::SizeType                      SizeType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodType NeighborhoodIteratorType;

  typedef ImageRegionIterator< UpdateBufferType > UpdateIteratorType;

  typename OutputImageType::Pointer output = this->GetOutput();

  // Get the FiniteDifferenceFunction to use in calculations.
  const typename FiniteDifferenceFunctionType::Pointer
      df = this->GetDifferenceFunction();

  const SizeType radius = df->GetRadius();

  // Ask the function object for a pointer to a data structure it
  // will use to manage any global values it needs.  We'll pass this
  // back to the function object at each calculation and then
  // again so that the function object can use it to determine a
  // time step for this iteration.
  void * globalData = df->GetGlobalDataPointer();

  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions.  We operate
  // on the output region because input has been copied to output.
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< OutputImageType >
  FaceCalculatorType;

  typedef typename FaceCalculatorType::FaceListType FaceListType;

  FaceCalculatorType faceCalculator;

  FaceListType faceList = faceCalculator(output, regionToProcess, radius);
  typename FaceListType::iterator fIt = faceList.begin();
  typename FaceListType::iterator fEnd = faceList.end();

  // Process the non-boundary region.
  NeighborhoodIteratorType nD(radius, output, *fIt);
  UpdateIteratorType       nU(m_UpdateBuffer,  *fIt);
  nD.GoToBegin();
  while ( !nD.IsAtEnd() )
    {
    nU.Value() = df->ComputeUpdate(nD, globalData);
    ++nD;
    ++nU;
    }

  // Process each of the boundary faces.
  for ( ++fIt; fIt != fEnd; ++fIt )
    {
    NeighborhoodIteratorType bD(radius, output, *fIt);
    UpdateIteratorType bU(m_UpdateBuffer, *fIt);

    bD.GoToBegin();
    bU.GoToBegin();
    while ( !bD.IsAtEnd() )
      {
      bU.Value() = df->ComputeUpdate(bD, globalData);
      ++bD;
      ++bU;
      }
    }

  // Ask the finite difference function to compute the time step for
  // this iteration.  We give it the global data pointer to use, then
  // ask it to free the global data memory.
  TimeStepType timeStep = df->ComputeGlobalTimeStep(globalData);
  df->ReleaseGlobalDataPointer(globalData);

  return timeStep;
}

template< typename TInputImage, typename TOutputImage >
void
DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
