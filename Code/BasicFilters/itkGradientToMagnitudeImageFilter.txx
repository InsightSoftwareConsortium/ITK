#ifndef __itkGradientToMagnitudeImageFilter_txx
#define __itkGradientToMagnitudeImageFilter_txx

#include <iostream>
#include <math.h>
#include "itkSize.h"
#include "itkPixelTraits.h"
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

  // Make sure we're getting everything
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

  // How big is the input image?
  TInputImage::SizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

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

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType index;

  const double* origin;
  const double* spacing;

  // Get the origin and spacing from the input image
  origin = inputPtr->GetOrigin();
  spacing = inputPtr->GetSpacing();

  // Set the origin and spacing of the output image
  outputPtr->SetOrigin(origin);
  outputPtr->SetSpacing(spacing);

  double acc = 0;

  // walk the output image, and sample the input image
  for ( ; !outIt.IsAtEnd(); ++outIt)
    {
    // determine the index of the output pixel
    index = outIt.GetIndex();

    for(int i = 0; i < NDimensions; i++)
      {
      acc += inputPtr->GetPixel(index).GetVector()[i]
        * inputPtr->GetPixel(index).GetVector()[i];
      }

    acc = sqrt(acc);

    // std::cout << "Magnitude is " << acc << "\n";

    outputPtr->GetPixel(index).SetScalar(acc);

    }

  std::cout << "GradientToMagnitudeImageFilter::GenerateData() finished\n";
}

} // end namespace

#endif
