/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStreamingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkStreamingImageFilter_txx
#define _itkStreamingImageFilter_txx
#include "itkStreamingImageFilter.h"
#include "itkCommand.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
StreamingImageFilter<TInputImage,TOutputImage>
::StreamingImageFilter()
{
  // default to 10 divisions
  m_NumberOfStreamDivisions = 10;

  // create default region splitter
  m_RegionSplitter = ImageRegionSplitter<InputImageDimension>::New();
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
StreamingImageFilter<TInputImage,TOutputImage>
::~StreamingImageFilter()
{
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
StreamingImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Number of stream divisions: " << m_NumberOfStreamDivisions
     << std::endl;
  if (m_RegionSplitter)
    {
    os << indent << "Region splitter:" << m_RegionSplitter << std::endl;
    }
  else
    {
    os << indent << "Region splitter: (none)" << std::endl;
    }
}

/**
 *
 */
template<class TInputImage, class TOutputImage>
void 
StreamingImageFilter<TInputImage,TOutputImage>
::UpdateOutputData(DataObject *itkNotUsed(output))
{
  std::vector<DataObjectPointer>::size_type idx;

  /**
   * prevent chasing our tail
   */
  if (m_Updating)
    {
    return;
    }

  /**
   * Make sure we have an input
   */
  if (this->GetNumberOfInputs() < this->GetNumberOfRequiredInputs())
    {
    itkErrorMacro(<< "At least " << this->GetNumberOfRequiredInputs() << " inputs are required but only " << this->GetNumberOfInputs() << " are specified");
    return;
    }
  this->SetAbortGenerateData(0);
  this->SetProgress(0.0);
  m_Updating = true;
    

  /**
   * Initialize all the outputs
   */
  for (idx = 0; idx < this->GetNumberOfOutputs(); idx++)
    {
    if (this->GetOutput(idx))
      {
      this->GetOutput(idx)->PrepareForNewData(); 
      }
    }
 
  this->InvokeEvent(Command::StartEvent);

  /**
   * Allocate the output buffer. 
   */
  OutputImagePointer outputPtr = this->GetOutput(0);
  OutputImageRegionType outputRegion = outputPtr->GetRequestedRegion();
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->Allocate();

  /**
   * Grab the input
   */
  InputImagePointer inputPtr = this->GetInput(0);

  /**
   * Determine of number of pieces to divide the input.  This will be the
   * minimum of what the user specified via SetNumberOfStreamDivisions()
   * and what the Splitter thinks is a reasonable value.
   */
  unsigned int numDivisions, numDivisionsFromSplitter;

  numDivisions = m_NumberOfStreamDivisions;
  numDivisionsFromSplitter
    = m_RegionSplitter
        ->GetNumberOfSplits(outputRegion, m_NumberOfStreamDivisions);
  if (numDivisionsFromSplitter < numDivisions)
    {
    numDivisions = numDivisionsFromSplitter;
    }
  
  /**
   * Loop over the number of pieces, execute the upstream pipeline on each
   * piece, and copy the results into the output image.
   */
  unsigned int piece;
  InputImageRegionType streamRegion;
  for (piece = 0;
       piece < numDivisions && !this->GetAbortGenerateData();
       piece++)
    {
    streamRegion = m_RegionSplitter->GetSplit(piece, numDivisions,
                                                   outputRegion);
      
    inputPtr->SetRequestedRegion(streamRegion);
    inputPtr->PropagateRequestedRegion();
    inputPtr->UpdateOutputData();

    // copy the result to the proper place in the output. the input
    // requested region determined by the RegionSplitter (as opposed
    // to what the pipeline might have enlarged it to) is used to
    // construct the iterators for both the input and output
    ImageRegionIterator<InputImageType> inIt(inputPtr, streamRegion);
    ImageRegionIterator<OutputImageType> outIt(outputPtr, streamRegion);

    for (inIt.GoToBegin(), outIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt, ++outIt)
      {
      outIt.Set( inIt.Get() );
      }

    this->UpdateProgress((float) piece / numDivisions );
    }

  m_Updating = false;
  
  /**
   * If we ended due to aborting, push the progress up to 1.0 (since
   * it probably didn't end there)
   */
  if ( !this->GetAbortGenerateData() )
    {
    this->UpdateProgress(1.0);
    }

  // Notify end event observers
  this->InvokeEvent(Command::EndEvent);

  /**
   * Now we have to mark the data as up to data.
   */
  for (idx = 0; idx < this->GetNumberOfOutputs(); ++idx)
    {
    if (this->GetOutput(idx))
      {
      this->GetOutput(idx)->DataHasBeenGenerated();
      }
    }
  
  /**
   * Release any inputs if marked for release
   */
  for (idx = 0; idx < this->GetNumberOfInputs(); ++idx)
    {
    if (this->GetInput(idx))
      {
      if ( this->GetInput(idx)->ShouldIReleaseData() )
        {
        this->GetInput(idx)->ReleaseData();
        }
      }  
    }
  
  /**
   * Information gets invalidated as soon as Update is called,
   * so validate it again here.
   */
  m_InformationTime.Modified();
}


} // end namespace itk

#endif
