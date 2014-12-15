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
#ifndef itkHistogramToImageFilter_hxx
#define itkHistogramToImageFilter_hxx

#include "itkHistogramToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{
/** Constructor */
template< typename THistogram, typename TImage, typename TFunction >
HistogramToImageFilter< THistogram, TImage, TFunction >
::HistogramToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
}

/** Destructor */
template< typename THistogram, typename TImage, typename TFunction >
HistogramToImageFilter< THistogram, TImage, TFunction >
::~HistogramToImageFilter()
{}

/** Set the Input Histogram */
template< typename THistogram, typename TImage, typename TFunction >
void
HistogramToImageFilter< THistogram, TImage, TFunction >
::SetInput(const HistogramType *input)
{
  // Process object is not const-correct so the const_cast is required here
  HistogramType * histogram = const_cast< HistogramType * >( input );
  this->ProcessObject::SetNthInput(0,  histogram);
}

template< typename THistogram, typename TImage, typename TFunction >
const typename HistogramToImageFilter< THistogram, TImage, TFunction >::HistogramType *
HistogramToImageFilter< THistogram, TImage, TFunction >
::GetInput(void)
{
  return itkDynamicCastInDebugMode< const HistogramType * >( this->GetPrimaryInput() );
}

template< typename THistogram, typename TImage, typename TFunction >
void
HistogramToImageFilter< THistogram, TImage, TFunction >
::SetTotalFrequency(SizeValueType n)
{
  if ( n < 1 )
    {
    itkExceptionMacro("Total frequency in the histogram must be at least 1.");
    }

  if ( n == this->GetFunctor().GetTotalFrequency() )
    {
    return;
    }
  else
    {
    this->GetFunctor().SetTotalFrequency(n);
    this->Modified();
    }
}

template< typename THistogram, typename TImage, typename TFunction >
void
HistogramToImageFilter< THistogram, TImage, TFunction >
::GenerateOutputInformation()
{
  // we need the input histogram to be up to date, so we can look at its values
  // to compute the size, spacing and origin of the output image.
  // the GetInput() from ProcessObject is used to get a non const histogram.
  this->ProcessObject::GetInput(0)->Update();

  // Get the input and output pointers
  // Get from decorator
  const HistogramType *inputHistogram = this->GetInput();
  OutputImageType *    outputImage    = this->GetOutput();

  SizeType size;
  PointType origin;
  SpacingType spacing;
  // Set the image size to the number of bins along each dimension.
  // TODO: is it possible to have a size 0 on one of the dimension? if yes, the size must be checked
  unsigned int minDim = std::min((unsigned int)ImageDimension, inputHistogram->GetMeasurementVectorSize());
  for ( unsigned int i = 0; i < minDim; i++ )
    {
    size[i]    = inputHistogram->GetSize(i);
    origin[i]  = inputHistogram->GetMeasurement(0, i);
    spacing[i] = inputHistogram->GetBinMax(i, 0) - inputHistogram->GetBinMin(i, 0);
    }

  // if the image is of greater dimension than the histogram, use some default values
  for ( unsigned int i = inputHistogram->GetMeasurementVectorSize(); i<ImageDimension; i++ )
    {
    size[i]    = 1;
    origin[i]  = 0.0;
    spacing[i] = 1.0;
    }

  // Set output image params and Allocate image
  typename OutputImageType::RegionType region;
  region.SetSize(size);

  outputImage->SetRegions(region);
  outputImage->SetSpacing(spacing);     // set spacing
  outputImage->SetOrigin(origin);       // and origin
}

//----------------------------------------------------------------------------

/** Update */
template< typename THistogram, typename TImage, typename TFunction >
void
HistogramToImageFilter< THistogram, TImage, TFunction >
::GenerateData(void)
{
  itkDebugMacro(<< "HistogramToImageFilter::Update() called");

  this->AllocateOutputs();

  // Get the input and output pointers
  // Get from decorator
  const HistogramType *inputHistogram = this->GetInput();
  OutputImageType *    outputImage    = this->GetOutput();

  // Set the TotalFrequency in the functor
  this->SetTotalFrequency( static_cast< SizeValueType >(
                             inputHistogram->GetTotalFrequency() ) );

  ProgressReporter progress( this, 0,
                             outputImage->GetRequestedRegion().GetNumberOfPixels() );

  typedef typename HistogramType::ConstIterator         HistogramIterator;
  typedef typename HistogramType::AbsoluteFrequencyType AbsoluteFrequencyType;

  HistogramIterator hitr = inputHistogram->Begin();

  // Fill image with frequencies from Histogram
  ImageIteratorType iter( outputImage, outputImage->GetRequestedRegion() );

  while ( !iter.IsAtEnd() )
    {
    const AbsoluteFrequencyType & value = hitr.GetFrequency();

    iter.Set( m_Functor( static_cast< SizeValueType >( value ) ) );

    ++iter;
    ++hitr;

    progress.CompletedPixel();
    }
} // end update function

template< typename THistogram, typename TImage, typename TFunction >
void
HistogramToImageFilter< THistogram, TImage, TFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Sum of frequencies of measurement vectors of the histogram: "
     << m_Functor.GetTotalFrequency() << std::endl;
}
} // end namespace itk

#endif
