/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceOfGaussiansGradientImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDifferenceOfGaussiansGradientImageFilter_txx
#define __itkDifferenceOfGaussiansGradientImageFilter_txx

#include <math.h>
#include "itkProgressReporter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template<typename TInputImage, typename TDataType>
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::DifferenceOfGaussiansGradientImageFilter()
{
  itkDebugMacro(<< "DifferenceOfGaussiansGradientImageFilter::DifferenceOfGaussiansGradientImageFilter() called");

  m_Width = 2;
}

template<typename TInputImage, typename TDataType>
void
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::GenerateData()
{
  itkDebugMacro(<< "DifferenceOfGaussiansGradientImageFilter::GenerateData() called");

  // Get the input and output pointers
  typename Superclass::InputImagePointer  inputPtr = 
      const_cast< TInputImage * >( this->GetInput(0));
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);

  // Make sure we're getting everything
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

  // How big is the input image?
  typename TInputImage::SizeType size = inputPtr->GetLargestPossibleRegion().GetSize();

  // Create a region object native to the output image type
  OutputImageRegionType outputRegion;

  // Resize the output region
  outputRegion.SetSize( size );

  // Set the largest legal region size (i.e. the size of the whole image)
  // to what we just defined
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->Allocate();

  // Create a progress reporter
  ProgressReporter progress(this, 0, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType outputIndex;
  typename TOutputImage::IndexType upperIndex;
  typename TOutputImage::IndexType lowerIndex;

  // walk the output image, and sample the input image
  for ( ; !outIt.IsAtEnd(); ++outIt)
    {
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // is the current index an acceptable distance from the edges
    // of the image?
    bool isValidGrad = true;

    for (unsigned int i = 0; i < NDimensions; ++i)
      {
        const int width = static_cast<int>(m_Width);
        const int sizeDifference = 
            static_cast<int>(size.m_Size[i]) - width;
          
        if( !( (outputIndex[i] < sizeDifference ) &&
               (outputIndex[i] >= width         )    ) )
        {
          isValidGrad = false;
        }
      }

    if (isValidGrad)
      {
      // We're in a safe position, so calculate the gradient for
      // each dimension
      for (unsigned int i = 0; i < NDimensions; i++)
        {
        // Build the indices for each pixel
        for (unsigned int j = 0; j < NDimensions; j++)
          {
          if(j == i)
            {
            upperIndex[j] = outputIndex[j] + static_cast<typename TOutputImage::IndexValueType>(m_Width);
            lowerIndex[j] = outputIndex[j] - static_cast<typename TOutputImage::IndexValueType>(m_Width);
            }
          else
            {
            upperIndex[j] = outputIndex[j];
            lowerIndex[j] = outputIndex[j];
            }
          }
        // Remember, output type is a covariant vector of TDataType
        outputPtr->GetPixel(outputIndex)[i] =
        inputPtr->GetPixel(upperIndex) - inputPtr->GetPixel(lowerIndex);
        }
      }
    else // We're not in a safe position, gradient is zero
      {
      for (unsigned int i = 0; i < NDimensions; ++i)
        outputPtr->GetPixel(outputIndex)[i] = 0.0;
      }
    progress.CompletedPixel();
    }

  itkDebugMacro(<< "DifferenceOfGaussiansGradientImageFilter::GenerateData() finished");
}

template<typename TInputImage, typename TDataType>
void
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Width is " << m_Width << std::endl;
}

} // end namespace

#endif
