/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStreamingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkStreamingImageFilter_txx
#define _itkStreamingImageFilter_txx
#include "itkStreamingImageFilter.h"
#include "itkCommand.h"
#include "itkImageRegionIterator.h"

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
  unsigned int idx;

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
    itkExceptionMacro(<< "At least " << static_cast<unsigned int>( this->GetNumberOfRequiredInputs() ) << " inputs are required but only " << static_cast<unsigned int>( this->GetNumberOfInputs()) << " are specified");
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
 
  this->InvokeEvent( StartEvent() );

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
  InputImagePointer inputPtr = 
    const_cast< InputImageType * >( this->GetInput(0) );

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
  this->InvokeEvent( EndEvent() );

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
        InputImagePointer input =
          const_cast< InputImageType * >( this->GetInput(idx) );
        input->ReleaseData();
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
