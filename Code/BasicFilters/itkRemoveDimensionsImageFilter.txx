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
template <class TInputImage, unsigned int OutputImageDimension>
RemoveDimensionsImageFilter<TInputImage,OutputImageDimension>
::RemoveDimensionsImageFilter()
{
}


/**
 *
 */
template <class TInputImage, unsigned int OutputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,OutputImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent <<m_RemoveDimensionsIndex<<"\n"; 
}


/** RemoveDimensionsImageFilter produces an image with different number of 
 * dimension than the input image.  As such, RemoveDimensionsImageFilter 
 * needs to provide an implementation for GenerateOutputInformation() in 
 * order to inform the pipeline execution model.  The original documentation 
 * of this method is below.
 * \sa ProcessObject::GenerateOutputInformaton() */

template <class TInputImage, unsigned int OutputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,OutputImageDimension>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
 
  // get pointers to the input and output
  OutputImagePointer outputPtr = this->GetOutput();
  InputImageConstPointer inputPtr =  this->GetInput();

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  //Get spacing and origin of the input image;
  const double *inputSpacing = inputPtr->GetSpacing();
  const double *inputOrigin = inputPtr->GetOrigin();

  //Set the spacing and origin of the output image to correspond 
  //to the input image
  double outputSpacing[OutputImageDimension]; // output image spacing
  double outputOrigin[OutputImageDimension];  // output image origin
  int i;
  int count = 0;
  for (i=0; i < InputImageDimension; i++)
    {
    if (!m_RemoveDimensionsIndex[i])
      {
      outputSpacing[count] = inputSpacing[i];
      outputOrigin[count] = inputOrigin[i];
      count++;
      }
    }
  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);
}




/** RemoveDimensionsImageFilter needs only a small portion of an input
 * image.  For every dimension that is to be removed, we only need to
 * look at one particular value in that dimension in order to get the 
 * correct output.
 */
template <class TInputImage, unsigned int OutputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,OutputImageDimension>
::GenerateInputRequestedRegion()
{
  int i;
  InputImagePointer inputPtr =  const_cast< InputImageType * >( this->GetInput().GetPointer() );
  InputImageRegionType inputRegion = inputPtr->GetLargestPossibleRegion();
  InputImageSizeType inputSize = inputRegion.GetSize();
  InputImageIndexType inputIndex;

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

  inputIndex = m_RemoveDimensionsIndex;
  inputRegion.SetIndex(inputIndex);
  inputRegion.SetSize(inputSize);
  inputPtr->SetRequestedRegion( inputRegion );

}



//-----------------------------------------------------------------------
//
template <class TInputImage, unsigned int OutputImageDimension>
void 
RemoveDimensionsImageFilter<TInputImage,OutputImageDimension>
::GenerateData()
{

  itkDebugMacro(<<"Actually executing");
  
  // Get the input and output pointers
  InputImageConstPointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();
  OutputImageRegionType outputRegion = outputPtr->GetLargestPossibleRegion();
  outputPtr->SetBufferedRegion(outputPtr->GetLargestPossibleRegion());
  outputPtr->Allocate();

  // Define and setup the iterators.
  typedef ImageRegionIterator<TOutputImage> OutputIterator;
  typedef ImageRegionConstIterator<TInputImage> InputIterator;
  OutputIterator outIt(outputPtr, outputRegion);
  InputIterator inIt(inputPtr, inputPtr->GetRequestedRegion());
  
  //walk the input and output and set the output.
  for (outIt.GoToBegin(), inIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt, ++inIt)
    {
    //copy the input pixel to the output
    outIt.Set( inIt.Get());
    }

}

} // end namespace itk

#endif
