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
#ifndef itkExpandImageFilter_hxx
#define itkExpandImageFilter_hxx

#include "itkExpandImageFilter.h"
#include "itkImageScanlineIterator.h"
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
  // Get the input and output pointers
  OutputImagePointer outputPtr = this->GetOutput();

  // Iterator for walking the output
  typedef ImageScanlineIterator< TOutputImage > OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Report progress on a per scanline basis
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if( size0 == 0)
    {
    return;
    }
  const size_t numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / size0;
  ProgressReporter progress( this, threadId, static_cast<SizeValueType>( numberOfLinesToProcess ) );

  const size_t ln =  outputRegionForThread.GetSize(0);

  // Walk the output region, and interpolate the input image
  while ( !outIt.IsAtEnd() )
    {
    const typename OutputImageType::IndexType outputIndex = outIt.GetIndex();


    // Determine the input pixel location associated with this output
    // pixel at the start of the scanline.
    //
    // Don't need to check for division by zero because the factors are
    // clamped to be minimum for 1.
    typename InterpolatorType::ContinuousIndexType inputIndex;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      inputIndex[j] = ( (double)outputIndex[j] + 0.5 ) / (double)m_ExpandFactors[j] - 0.5;
      }

    const double lineDelta = (double)1.0 / (double)m_ExpandFactors[0];

    for( size_t i = 0; i < ln; ++i )
      {


      itkAssertInDebugAndIgnoreInReleaseMacro(m_Interpolator->IsInsideBuffer(inputIndex));

      outIt.Set( static_cast< OutputPixelType >( m_Interpolator->EvaluateAtContinuousIndex(inputIndex) ) );
      ++outIt;

      // Only increment the x-index as the rest is constant per
      // scanline.
      inputIndex[0] += lineDelta;
      }

    outIt.NextLine();
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
  InputImageType * inputPtr =
    const_cast< InputImageType * >( this->GetInput() );
  const OutputImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro( inputPtr != ITK_NULLPTR );
  itkAssertInDebugAndIgnoreInReleaseMacro( outputPtr );

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
  const InputImageType * inputPtr = this->GetInput();
  OutputImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro( inputPtr );
  itkAssertInDebugAndIgnoreInReleaseMacro( outputPtr != ITK_NULLPTR );

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
