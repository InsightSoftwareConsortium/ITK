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
#include "itkImageRegionSplitterSlowDimension.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >
::EigenToMeasureParameterEstimationFilter()
{
  /* Set stream parameters */
  this->SetNumberOfStreamDivisions(10);
  this->SetRegionSplitter(ImageRegionSplitterSlowDimension::New());

  /* Allocate parameterset decorator */
  typename ParameterDecoratedType::Pointer output = ParameterDecoratedType::New().GetPointer();
  this->ProcessObject::SetNthOutput( 1,  output.GetPointer() );
  this->GetParametersOutput()->Set( ParameterArrayType() );
}

template< typename TInputImage, typename TOutputImage >
void
EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >
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

  // Call a method that can be overridden by a subclass to perform
  // some calculations prior to splitting the main computations into
  // separate threads
  this->BeforeThreadedGenerateData();

  /**
   * Loop over the number of pieces, execute the upstream pipeline on each
   * piece, and copy the results into the output image.
   */
  for (unsigned int piece=0;
       piece < numDivisions && !this->GetAbortGenerateData();
       piece++ )
  {
    /* Determine the split region and calculate the input */
    InputImageRegionType streamRegion;
    this->CallCopyOutputRegionToInputRegion(streamRegion, outputRegion);

    this->GetRegionSplitter()->GetSplit(piece, numDivisions, streamRegion);
    inputPtr->SetRequestedRegion(streamRegion);
    inputPtr->PropagateRequestedRegion();
    inputPtr->UpdateOutputData();

    /* Process this chunk */
    this->ThreadedGenerateData(streamRegion, piece);
    
    /* Update progress and stream another chunk */
    this->UpdateProgress( static_cast<float>(piece) / static_cast<float>(numDivisions) );
  }

  // Call a method that can be overridden by a subclass to perform
  // some calculations after all the threads have completed
  this->AfterThreadedGenerateData();

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
    if ( this->ProcessObject::GetOutput(idx) )
    {
      this->ProcessObject::GetOutput(idx)->DataHasBeenGenerated();
    }
  }

  /** Release any inputs if marked for release */
  this->ReleaseInputs();

  /** Mark that we are no longer updating the data in this filter */
  this->m_Updating = false;
}

template< typename TInputImage, typename TOutputImage >
typename EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >::ParameterDecoratedType *
EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >
::GetParametersOutput() {
  return static_cast< ParameterDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TOutputImage >
const typename EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >::ParameterDecoratedType *
EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >
::GetParametersOutput() const {
  return static_cast< const ParameterDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TOutputImage >
void
EigenToMeasureParameterEstimationFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif // itkEigenToMeasureParameterEstimationFilter_hxx
