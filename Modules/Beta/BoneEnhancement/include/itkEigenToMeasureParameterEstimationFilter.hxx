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

#ifndef itkEigenToMeasureParameterEstimationFilter_hxx
#define itkEigenToMeasureParameterEstimationFilter_hxx

#include "itkEigenToMeasureParameterEstimationFilter.h"
#include "itkCommand.h"
#include "itkImageAlgorithm.h"
#include "itkImageRegionSplitterSlowDimension.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionSplitterBase.h"

namespace itk
{
template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::EigenToMeasureParameterEstimationFilter() :
  m_CurrentSplit(0)
{
  /* We require an input image */
  this->SetNumberOfRequiredInputs( 1 );

  /* We require an output image and parameters */
  this->SetNumberOfRequiredOutputs( 2 );

  /* Allocate parameterset decorator */
  typename ParameterDecoratedType::Pointer output = ParameterDecoratedType::New();
  ParameterType initialParameters;
  for (unsigned int i = 0; i < initialParameters.Length; ++i){
    initialParameters[i] = 0;
  }
  output->Set(initialParameters);
  this->ProcessObject::SetNthOutput( 1,  output.GetPointer() );
}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::~EigenToMeasureParameterEstimationFilter()
{}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
void
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::ThreadedGenerateData(const InputImageRegionType& region, unsigned int streamNumber)
{
  /* If size is zero, return */
  if (region.GetSize(0) == 0)
  {
    return;
  }

  /* Get input pointers */
  InputImageConstPointer inputPointer = this->GetInput();
  SpatialObjectConstPointer maskPointer = this->GetMaskingSpatialObject();
  typename InputImageType::PointType point;

  /* Setup iterator */
  ImageRegionConstIteratorWithIndex< TInputImage > inputIt(inputPointer, region);

  /* Iterate and count */
  inputIt.GoToBegin();
  while ( !inputIt.IsAtEnd() )
  {
    inputPointer->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
    if ( (!maskPointer) ||  (maskPointer->IsInside(point)) )
    {
      m_Functor.ProcessPixel(inputIt.Get(), streamNumber);
    }
    ++inputIt;
  }
}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
void
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::UpdateOutputData(DataObject *itkNotUsed(output))
{
  /** Prevent chasing our tail */
  if ( this->m_Updating )
  {
    return;
  }

  /** Prepare all the outputs. This may deallocate previous bulk data. */
  this->PrepareOutputs();

  /** Make sure we have the necessary inputs */
  const itk::ProcessObject::DataObjectPointerArraySizeType &ninputs = this->GetNumberOfValidRequiredInputs();
  if ( ninputs < this->GetNumberOfRequiredInputs() )
  {
    itkExceptionMacro(
      << "At least " << static_cast< unsigned int >( this->GetNumberOfRequiredInputs() )
      << " inputs are required but only " << ninputs << " are specified.");
    return;
  }

  /**
   * Tell all Observers that the filter is starting,
   * before emiting the 0.0 Progress event
   */
  this->InvokeEvent( StartEvent() );

  this->SetAbortGenerateData(0);
  this->UpdateProgress(0.0);
  this->m_Updating = true;

  /** Allocate the output buffer. */
  OutputImageType      *outputPtr = this->GetOutput(0);
  const OutputImageRegionType outputRegion = outputPtr->GetRequestedRegion();
  outputPtr->SetBufferedRegion(outputRegion);
  outputPtr->Allocate();

  /** Grab the input */
  InputImageType * inputPtr = const_cast < InputImageType * >(this->GetInput(0));

  /**
   * Determine of number of pieces to divide the input.  This will be the
   * minimum of what the user specified via SetNumberOfStreamDivisions()
   * and what the Splitter thinks is a reasonable value.
   */
  unsigned int numDivisions, numDivisionsFromSplitter;

  numDivisions = this->GetNumberOfStreamDivisions();
  numDivisionsFromSplitter =
    this->GetRegionSplitter()
    ->GetNumberOfSplits(outputRegion, this->GetNumberOfStreamDivisions());
  if ( numDivisionsFromSplitter < numDivisions )
  {
    numDivisions = numDivisionsFromSplitter;
  }

  /* Tell the functor the number of times we will call it (potentially) */
  m_Functor.Initialize(this->GetNumberOfStreamDivisions() * this->GetNumberOfThreads());

  /**
   * Loop over the number of pieces, execute the upstream pipeline on each
   * piece, and copy the results into the output image.
   */
  for (unsigned int piece=0;
       piece < numDivisions && !this->GetAbortGenerateData();
       piece++ )
  {
    /* Set the current peice */
    m_CurrentSplit = piece;

    /* Determine the split region and calculate the input */
    InputImageRegionType streamRegion = outputRegion;
    this->GetRegionSplitter()->GetSplit(piece, numDivisions, streamRegion);
    inputPtr->SetRequestedRegion(streamRegion);
    inputPtr->PropagateRequestedRegion();
    inputPtr->UpdateOutputData();

    /* Setup a multithreadign process */
    ThreadStruct str;
    str.Filter = this;

    // Get the output pointer
    const ImageRegionSplitterBase * splitter = this->GetImageRegionSplitter();
    const unsigned int validThreads = splitter->GetNumberOfSplits( streamRegion, this->GetNumberOfThreads() );

    this->GetMultiThreader()->SetNumberOfThreads( validThreads );
    this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str);

    // multithread the execution
    this->GetMultiThreader()->SingleMethodExecute();

    // copy the result to the proper place in the output. the input
    // requested region determined by the RegionSplitter (as opposed
    // to what the pipeline might have enlarged it to) is used to
    // copy the regions from the input to output
    if (inputPtr != outputPtr)
    {
      ImageAlgorithm::Copy( inputPtr, outputPtr, streamRegion, streamRegion );
    }

    /* Update progress and stream another chunk */
    this->UpdateProgress( static_cast<float>(piece) / static_cast<float>(numDivisions) );
  }

  /* Compute and set the parameters */
  this->GetParametersOutput()->Set(m_Functor.GetComputedParameters());

  /**
   * If we ended due to aborting, push the progress up to 1.0
   * (since it probably didn't end there)
   */
  if ( !this->GetAbortGenerateData() )
  {
    this->UpdateProgress(1.0);
  }

  /** Notify end event observers */
  this->InvokeEvent( EndEvent() );

  /** Now we have to mark the data as up to data. */
  for (unsigned int idx = 0; idx < this->GetNumberOfOutputs(); ++idx )
  {
    if ( this->GetOutput(idx) )
    {
      this->GetOutput(idx)->DataHasBeenGenerated();
    }
  }

  /** Release any inputs if marked for release */
  this->ReleaseInputs();

  /** Mark that we are no longer updating the data in this filter */
  this->m_Updating = false;
}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
ITK_THREAD_RETURN_TYPE
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::ThreaderCallback(void *arg)
{
  ThreadStruct *str;
  ThreadIdType  total, threadId, threadCount;

  threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  str = (ThreadStruct *)( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  typename OutputImageType::RegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);
  unsigned int streamNumber = str->Filter->m_CurrentSplit;

  if ( threadId < total )
  {
    str->Filter->ThreadedGenerateData(splitRegion, str->Filter->GetNumberOfStreamDivisions()*threadId + streamNumber);
  }
  // else
  //   {
  //   otherwise don't use this thread. Sometimes the threads dont
  //   break up very well and it is just as efficient to leave a
  //   few threads idle.
  //   }

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
typename EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >::ParameterDecoratedType *
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::GetParametersOutput() {
  return static_cast< ParameterDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
const typename EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >::ParameterDecoratedType *
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::GetParametersOutput() const {
  return static_cast< const ParameterDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
void
EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject, TFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif // itkEigenToMeasureParameterEstimationFilter_hxx
