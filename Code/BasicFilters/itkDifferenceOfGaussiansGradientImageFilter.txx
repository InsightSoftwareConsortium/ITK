#ifndef __itkDifferenceOfGaussiansGradientImageFilter_txx
#define __itkDifferenceOfGaussiansGradientImageFilter_txx

#include <iostream>
#include <math.h>
#include "itkDifferenceOfGaussiansGradientImageFilter.h"

namespace itk
{
template<typename TInputImage, typename TDataType>
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::DifferenceOfGaussiansGradientImageFilter()
{
  std::cout << "DifferenceOfGaussiansGradientImageFilter::DifferenceOfGaussiansGradientImageFilter() called\n";

  m_Width = 2;
}

template<typename TInputImage, typename TDataType>
void
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::GenerateData()
{
  std::cout << "DifferenceOfGaussiansGradientImageFilter::GenerateData() called\n";

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

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
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->Allocate();

  // Pass through the origin and spacing of the source image
  const double* origin;
  const double* spacing;

  // Get the origin and spacing from the input image
  origin = inputPtr->GetOrigin();
  spacing = inputPtr->GetSpacing();

  // Set the origin and spacing of the output image
  outputPtr->SetOrigin(origin);
  outputPtr->SetSpacing(spacing);

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

    for (int i = 0; i < NDimensions; ++i)
      {
      if( !( (outputIndex[i] < (size.m_Size[i] - static_cast<int>(m_Width)) ) &&
        (outputIndex[i] >= m_Width) ) )
        isValidGrad = false;
      }

    if (isValidGrad)
      {
      // We're in a safe position, so calculate the gradient for
      // each dimension
      for (int i = 0; i < NDimensions; i++)
        {
        // Build the indices for each pixel
        for (int j = 0; j < NDimensions; j++)
          {
          if(j == i)
            {
            upperIndex[j] = outputIndex[j] + m_Width;
            lowerIndex[j] = outputIndex[j] - m_Width;
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
      for (int i = 0; i < NDimensions; ++i)
        outputPtr->GetPixel(outputIndex)[i] = 0.0;
      }
    }

  std::cout << "DifferenceOfGaussiansGradientImageFilter::GenerateData() finished\n";
}

} // end namespace

#endif
