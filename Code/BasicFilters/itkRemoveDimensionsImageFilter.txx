/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRemoveDimensionsImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkRemoveDimensionsImageFilter_txx
#define _itkRemoveDimensionsImageFilter_txx

#include "itkRemoveDimensionsImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, unsigned int outputImageDimension>
RemoveDimensionsImageFilter<TInputImage,outputImageDimension>
::RemoveDimensionsImageFilter()
{
}


/**
 *
 */
template <class TInputImage, unsigned int outputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,outputImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent <<m_RemoveDimensionsIndex<<"\n"; 
}


/** 
 * RemoveDimensionsImageFilter produces an image which is a different 
 * resolution than its input image.  As such, 
 * RemoveDimensionsImageFilter needs to provide an implementation for 
 * GenerateOutputInformation() in order to inform the pipeline 
 * execution model.  The original documentation of this method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton() 
 */


template <class TInputImage, unsigned int outputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,outputImageDimension>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method

  Superclass::GenerateOutputInformation();
 
  // get pointers to the input and output
  OutputImagePointer      outputPtr = this->GetOutput();
  InputImageConstPointer  inputPtr  = this->GetInput();

  if ( !outputPtr || !inputPtr)
    {
    return;
    }

  // Set the output image size to the same value as the extraction region.
//  outputPtr->SetLargestPossibleRegion( m_ExtractionRegion );
}




//-----------------------------------------------------------------------
//
template <class TInputImage, unsigned int outputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,outputImageDimension>
::GenerateInputRequestedRegion()
{
  InputImagePointer inputPtr =  const_cast< InputImageType * >( this->GetInput().GetPointer() );
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
}



//-----------------------------------------------------------------------
//
template <class TInputImage, unsigned int outputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,outputImageDimension>
::GenerateData()
{
  int i;

  itkDebugMacro(<<"Actually executing");
  
  // Get the input and output pointers
  InputImageConstPointer  inputPtr = this->GetInput();
  InputImageRegionType inputRegion = inputPtr->GetLargestPossibleRegion();
  InputImageSizeType inputSize = inputRegion.GetSize();
  InputImageIndexType inputIndex;
  OutputImagePointer outputPtr = this->GetOutput();
  OutputImageRegionType outputRegion = outputPtr->GetLargestPossibleRegion();
  outputPtr->SetBufferedRegion(outputPtr->GetLargestPossibleRegion());

  //check if m_RemoveDimensionsIndex is consistent with input and output
  //image dimensions, and with the m_RemoveDimensionsIndex.  If they arent 
  //consistent, throw an exception.  At the same time setup the region size for
  //input image region iterator
  int zeroCount = 0;
  for (i = 0; i < m_RemoveDimensionsIndex.GetIndexDimension(); ++i)
    {
    if (!m_RemoveDimensionsIndex[i])
      {
      zeroCount++;
      }
    else
      {
      inputSize[i] = 1;
      }
    }

  /*
  if (zeroCount == m_RemoveDimensionsIndex.GetIndexDimension())
    {
    //This exceptionMacro call is crashing for some reason.
    itkExceptionMacro(<<"RemoveDimensionIndex needs atleast 1 non-zero value");
    }
  */

  //now setup the start index for the input image region iterator
  inputIndex = m_RemoveDimensionsIndex;
  inputRegion.SetIndex(inputIndex);
  inputRegion.SetSize(inputSize);

  // Define  and setup the iterators.
  typedef ImageRegionIterator<TOutputImage> OutputIterator;
  typedef ImageRegionConstIterator<TInputImage> InputIterator;
  OutputIterator outIt(outputPtr, outputRegion);
  InputIterator inIt(inputPtr, inputRegion);
  
  //walk the input and output and set the output.
  for (outIt.GoToBegin(), inIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt, ++inIt, i++ )
    {
    //copy the input pixel to the output
    outIt.Set( inIt.Get());    //The code is crashing here for some odd reason.
    }

}

} // end namespace itk

#endif
