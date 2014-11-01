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
#ifndef __itkExpandImageFilter_hxx
#define __itkExpandImageFilter_hxx

#include "itkExpandImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TInputImage, typename TOutputImage >
ExpandImageFilter< TInputImage, TOutputImage >
::ExpandImageFilter()
{
  // Set default factors to 1
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ExpandFactors[j] = 1;
    }

  // Setup the default interpolator
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_Interpolator = static_cast< InterpolatorType * >(
    interp.GetPointer() );

//TEST_RMV20100728   // Set default padding value to zero
//TEST_RMV20100728   m_EdgePaddingValue = NumericTraits<OutputPixelType>::ZeroValue();
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutputImage >
void
ExpandImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int j;
  os << indent << "ExpandFactors: [";
  for ( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_ExpandFactors[j] << ", ";
    }
  os << m_ExpandFactors[j] << "]" << std::endl;

  os << indent << "Interpolator: ";
  os << m_Interpolator.GetPointer() << std::endl;

//TEST_RMV20100728   os << indent << "EdgePaddingValue: "
//TEST_RMV20100728      << static_cast<typename
// NumericTraits<OutputPixelType>::PrintType>(m_EdgePaddingValue)
//TEST_RMV20100728      << std::endl;
//TEST_RMV20100728   os << indent << "EdgePaddingValue: ";
//TEST_RMV20100728   os << m_EdgePaddingValue << std::endl;
}

/**
 * Set expand factors from a single unsigned int
 */
template< typename TInputImage, typename TOutputImage >
void
ExpandImageFilter< TInputImage, TOutputImage >
::SetExpandFactors(
  const unsigned int factor)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( factor != m_ExpandFactors[j] ) { break; }
    }
  if ( j < ImageDimension )
    {
    this->Modified();
    for ( j = 0; j < ImageDimension; j++ )
      {
      m_ExpandFactors[j] = factor;
      if ( m_ExpandFactors[j] < 1 ) { m_ExpandFactors[j] = 1; }
      }
    }
}

/**
 * BeforeThreadedGenerateData
 */
template< typename TInputImage, typename TOutputImage >
void
ExpandImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  if ( !m_Interpolator || !this->GetInput() )
    {
    itkExceptionMacro(<< "Interpolator and/or Input not set");
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );
}

/**
 * ThreadedGenerateData
 */
template< typename TInputImage, typename TOutputImage >
void
ExpandImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  int i;

  // Get the input and output pointers
  OutputImagePointer outputPtr = this->GetOutput();

  // Iterator for walking the output
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input
  // pixel to and output pixel
  typename TOutputImage::IndexType outputIndex;
  typename InterpolatorType::ContinuousIndexType inputIndex;

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Walk the output region, and interpolate the input image
  for ( i = 0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    // Determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // Determine the input pixel location associated with this output pixel.
    // Don't need to check for division by zero because the factors are
    // clamped to be minimum for 1.
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      inputIndex[j] = ( (double)outputIndex[j] + 0.5 ) / (double)m_ExpandFactors[j] - 0.5;
      }

    // interpolate value and write to output
    if ( m_Interpolator->IsInsideBuffer(inputIndex) )
      {
      outIt.Set( static_cast< OutputPixelType >(
                   m_Interpolator->EvaluateAtContinuousIndex(inputIndex) ) );
      }
    else
      {
      itkExceptionMacro(<< "Interpolator outside buffer should never occur ");
//TEST_RMV20100728 * \warning: The following is valid only when the flag
//TEST_RMV20100728 * ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY is ON
//TEST_RMV20100728 * The output image will not contain any padding, and
// therefore the
//TEST_RMV20100728 * EdgePaddingValue will not be used.
//TEST_RMV20100728       outIt.Set( m_EdgePaddingValue );
      }
    progress.CompletedPixel();
    }
}

/**
 * GenerateInputRequesteRegion
 */
template< typename TInputImage, typename TOutputImage >
void
ExpandImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // We need to compute the input requested region (size and start index)
  unsigned int i;
  const typename TOutputImage::SizeType & outputRequestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex =
    outputPtr->GetRequestedRegion().GetIndex();

  typename TInputImage::SizeType inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;

  /**
   * inputRequestedSize = (outputRequestedSize / ExpandFactor) + 1)
   * The extra 1 above is to take care of edge effects when streaming.
   */
  for ( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    inputRequestedRegionSize[i] =
      (SizeValueType)std::ceil( (double)outputRequestedRegionSize[i]
                      / (double)m_ExpandFactors[i] ) + 1;

    inputRequestedRegionStartIndex[i] =
      (SizeValueType)std::floor( (double)outputRequestedRegionStartIndex[i]
                       / (double)m_ExpandFactors[i] );
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.SetIndex(inputRequestedRegionStartIndex);

  // Make sure the requested region is within largest possible.
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  // Set the input requested region.
  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

/**
 * GenerateOutputInformation
 */
template< typename TInputImage, typename TOutputImage >
void
ExpandImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // We need to compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType &
  inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();
  const typename TInputImage::PointType &
  inputOrigin = inputPtr->GetOrigin();

  typename TOutputImage::SpacingType outputSpacing;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputStartIndex;
  typename TOutputImage::PointType outputOrigin;

  typename TInputImage::SpacingType inputOriginShift;

  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSpacing[i] = inputSpacing[i] / (float)m_ExpandFactors[i];
    outputSize[i] = inputSize[i] * (SizeValueType)m_ExpandFactors[i];
    outputStartIndex[i] = inputStartIndex[i] * (IndexValueType)m_ExpandFactors[i];
    const double fraction = (double)( m_ExpandFactors[i] - 1 ) / (double)m_ExpandFactors[i];
    inputOriginShift[i] = -( inputSpacing[i] / 2.0 ) * fraction;
    }

  const typename TInputImage::DirectionType inputDirection = inputPtr->GetDirection();
  const typename TOutputImage::SpacingType outputOriginShift = inputDirection * inputOriginShift;

  outputOrigin = inputOrigin + outputOriginShift;

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
