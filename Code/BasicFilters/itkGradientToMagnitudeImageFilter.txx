#ifndef __itkGradientToMagnitudeImageFilter_txx
#define __itkGradientToMagnitudeImageFilter_txx

#include <iostream>
#include <math.h>
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkGradientToMagnitudeImageFilter.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::GradientToMagnitudeImageFilter()
{
  std::cout << "GradientToMagnitudeImageFilter::GradientToMagnitudeImageFilter() called\n";

}


template< class TInputImage, class TOutputImage >
void
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  std::cout << "GradientToMagnitudeImageFilter::GenerateData() called\n";

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Commented out per Lydia's instructions
  /*
  // Make sure we're getting everything
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

  // How big is the input image?
  typename TInputImage::SizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  // Create a region object native to the output image type
  OutputImageRegionType outputRegion;

  // Resize the output region
  outputRegion.SetSize( inputSize );

  // Set the largest legal region size (i.e. the size of the whole image)
  // to what we just defined
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->Allocate();
  */

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType index;

  // walk the output image, and sample the input image
  for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
    {
    double acc = 0;

    // determine the index of the output pixel
    index = outIt.GetIndex();

    for(int i = 0; i < NDimensions; i++)
      {
      acc += inputPtr->GetPixel(index)[i]
        * inputPtr->GetPixel(index)[i];
      }

    acc = sqrt(acc);

    // std::cout << "Magnitude is " << acc << "\n";

    outputPtr->GetPixel(index) = acc;

    }

  std::cout << "GradientToMagnitudeImageFilter::GenerateData() finished\n";
}

} // end namespace

#endif
