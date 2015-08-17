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
#ifndef itkVectorExpandImageFilter_hxx
#define itkVectorExpandImageFilter_hxx

#include "itkVectorExpandImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TInputImage, typename TOutputImage >
VectorExpandImageFilter< TInputImage, TOutputImage >
::VectorExpandImageFilter()
{
  // Set default factors to 1
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ExpandFactors[j] = 1;
    }

  // Setup the default interpolator
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_Interpolator =
    static_cast< InterpolatorType * >( interp.GetPointer() );

//TEST_RMV20100728   // Set default padding value to zero
//TEST_RMV20100728   for( unsigned int k = 0; k < VectorDimension; k++ )
//TEST_RMV20100728     {
//TEST_RMV20100728     m_EdgePaddingValue[k] =
// NumericTraits<OutputValueType>::ZeroValue();
//TEST_RMV20100728     }
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutputImage >
void
VectorExpandImageFilter< TInputImage, TOutputImage >
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

//TEST_RMV20100728  os << indent << "EdgePaddingValue: "
//TEST_RMV20100728     << static_cast<typename
// NumericTraits<OutputPixelType>::PrintType>(m_EdgePaddingValue)
//TEST_RMV20100728     << std::endl;
}

/**
 * Set expand factors from a single unsigned int
 */
template< typename TInputImage, typename TOutputImage >
void
VectorExpandImageFilter< TInputImage, TOutputImage >
::SetExpandFactors(const float factor)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( Math::NotExactlyEquals(factor, m_ExpandFactors[j]) ) { break; }
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

//TEST_RMV20100728/**
//TEST_RMV20100728 * Set the edge padding value
//TEST_RMV20100728 */
//TEST_RMV20100728template <typename TInputImage, typename TOutputImage>
//TEST_RMV20100728void
//TEST_RMV20100728VectorExpandImageFilter<TInputImage,TOutputImage>
//TEST_RMV20100728::SetEdgePaddingValue( const OutputPixelType& value )
//TEST_RMV20100728{
//TEST_RMV20100728  unsigned int i;
//TEST_RMV20100728  for( i = 0; i < OutputPixelType::Dimension; i++ )
//TEST_RMV20100728    {
//TEST_RMV20100728    if( value[i] != m_EdgePaddingValue[i] )
//TEST_RMV20100728      {
//TEST_RMV20100728      break;
//TEST_RMV20100728      }
//TEST_RMV20100728    }
//TEST_RMV20100728
//TEST_RMV20100728  if( i < OutputPixelType::Dimension )
//TEST_RMV20100728    {
//TEST_RMV20100728    this->Modified();
//TEST_RMV20100728    m_EdgePaddingValue = value;
//TEST_RMV20100728    }
//TEST_RMV20100728
//TEST_RMV20100728}

/**
 * BeforeThreadedGenerateData
 */
template< typename TInputImage, typename TOutputImage >
void
VectorExpandImageFilter< TInputImage, TOutputImage >
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
VectorExpandImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Get the input and output pointers
  OutputImagePointer outputPtr = this->GetOutput();

  // Iterator for walking the output
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input
  // pixel to and output pixel
  typename TOutputImage::IndexType outputIndex;
  typename InterpolatorType::ContinuousIndexType inputIndex;

  typedef typename InterpolatorType::OutputType InterpolatedType;

  OutputPixelType  outputValue;
  InterpolatedType interpolatedValue;

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Walk the output region, and interpolate the input image
  while ( !outIt.IsAtEnd() )
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
      interpolatedValue =
        m_Interpolator->EvaluateAtContinuousIndex(inputIndex);

      for ( unsigned int k = 0; k < VectorDimension; k++ )
        {
        outputValue[k] = static_cast< OutputValueType >(
          interpolatedValue[k] );
        }

      outIt.Set(outputValue);
      }
    else
      {
      itkExceptionMacro(<< "Interpolator outside buffer should never occur ");
//TEST_RMV20100728 * \warning: The following is valid only when the flag
//TEST_RMV20100728 * ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY is ON
//TEST_RMV20100728 * The output image will not contain any padding, and
// therefore the
//TEST_RMV20100728 * EdgePaddingValue will not be used.
//TEST_RMV20100728      outIt.Set( m_EdgePaddingValue );
      }
    ++outIt;
    progress.CompletedPixel();
    }
}

/**
 * GenerateInputRequesteRegion
 */
template< typename TInputImage, typename TOutputImage >
void
VectorExpandImageFilter< TInputImage, TOutputImage >
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
      (IndexValueType)std::floor( (double)outputRequestedRegionStartIndex[i]
                       / (double)m_ExpandFactors[i] );
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.SetIndex(inputRequestedRegionStartIndex);

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);

    throw e;
    }
}

/**
 * GenerateOutputInformation
 */
template< typename TInputImage, typename TOutputImage >
void
VectorExpandImageFilter< TInputImage, TOutputImage >
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
  const typename InputImageType::SpacingType &
  inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();
  const typename TInputImage::PointType &
  inputOrigin = inputPtr->GetOrigin();

  typename OutputImageType::SpacingType outputSpacing;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputStartIndex;
  typename TOutputImage::PointType outputOrigin;

  typename TInputImage::SpacingType inputOriginShift;

  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSpacing[i] = inputSpacing[i] / (float)m_ExpandFactors[i];
    outputSize[i] = (SizeValueType)
                    ( (float)inputSize[i] * m_ExpandFactors[i]
                      + 0.5f );
    outputStartIndex[i] = (IndexValueType)
                          ( (float)inputStartIndex[i] * m_ExpandFactors[i]
                            + 0.5f );
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
