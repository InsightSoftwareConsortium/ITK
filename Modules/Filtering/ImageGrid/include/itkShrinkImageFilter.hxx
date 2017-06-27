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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkShrinkImageFilter_hxx
#define itkShrinkImageFilter_hxx

#include "itkShrinkImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkContinuousIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
ShrinkImageFilter< TInputImage, TOutputImage >
::ShrinkImageFilter()
{
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ShrinkFactors[j] = 1;
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ShrinkImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shrink Factor: ";
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    os << m_ShrinkFactors[j] << " ";
    }
  os << std::endl;
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ShrinkImageFilter< TInputImage, TOutputImage >
::SetShrinkFactors(unsigned int factor)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( factor != m_ShrinkFactors[j] ) { break; }
    }
  if ( j < ImageDimension )
    {
    this->Modified();
    for ( j = 0; j < ImageDimension; j++ )
      {
      m_ShrinkFactors[j] = factor;
      if ( m_ShrinkFactors[j] < 1 )
        {
        m_ShrinkFactors[j] = 1;
        }
      }
    }
}


template< typename TInputImage, typename TOutputImage >
void
ShrinkImageFilter< TInputImage, TOutputImage >
::SetShrinkFactor(unsigned int i, unsigned int factor)
{
  if ( m_ShrinkFactors[i] == factor )
    {
    return;
    }

  this->Modified();
  m_ShrinkFactors[i] = factor;
}


/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ShrinkImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Get the input and output pointers
  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  // Convert the factor for convenient multiplication
  unsigned int i;

  typename TOutputImage::SizeType factorSize;
  for ( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    factorSize[i] = m_ShrinkFactors[i];
    }

  // Define a few indices that will be used to transform from an input pixel
  // to an output pixel
  OutputIndexType  outputIndex;
  InputIndexType   inputIndex;
  OutputOffsetType offsetIndex;

  typename TOutputImage::PointType tempPoint;

  // Use this index to compute the offset everywhere in this class
  outputIndex = outputPtr->GetLargestPossibleRegion().GetIndex();

  // We wish to perform the following mapping of outputIndex to
  // inputIndex on all points in our region
  outputPtr->TransformIndexToPhysicalPoint(outputIndex, tempPoint);
  inputPtr->TransformPhysicalPointToIndex(tempPoint, inputIndex);

  // Given that the size is scaled by a constant factor eq:
  // inputIndex = outputIndex * factorSize
  // is equivalent up to a fixed offset which we now compute
  OffsetValueType zeroOffset = 0;
  for ( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    offsetIndex[i] = inputIndex[i] - outputIndex[i] * m_ShrinkFactors[i];
    // It is plausible that due to small amounts of loss of numerical
    // precision that the offset it negaive, this would cause sampling
    // out of out region, this is insurance against that possibility
    offsetIndex[i] = std::max(zeroOffset, offsetIndex[i]);
    }

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputIterator;
  OutputIterator outIt(outputPtr, outputRegionForThread);

  while ( !outIt.IsAtEnd() )
    {
    // Determine the index and physical location of the output pixel
    outputIndex = outIt.GetIndex();

    // An optimized version of
    // outputPtr->TransformIndexToPhysicalPoint(outputIndex, tempPoint);
    // inputPtr->TransformPhysicalPointToIndex(tempPoint, inputIndex);
    // but without the rounding and precision issues
    inputIndex = outputIndex * factorSize + offsetIndex;

    // Copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(inputIndex) );
    ++outIt;

    progress.CompletedPixel();
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ShrinkImageFilter< TInputImage, TOutputImage >
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

  // Compute the input requested region (size and start index)
  // Use the image transformations to insure an input requested region
  // that will provide the proper range
  unsigned int i;
  const typename TOutputImage::SizeType & outputRequestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex =
    outputPtr->GetRequestedRegion().GetIndex();

  // Convert the factor for convenient multiplication
  typename TOutputImage::SizeType factorSize;
  for ( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    factorSize[i] = m_ShrinkFactors[i];
    }

  OutputIndexType  outputIndex;
  InputIndexType   inputIndex, inputRequestedRegionIndex;
  OutputOffsetType offsetIndex;

  typename TInputImage::SizeType inputRequestedRegionSize;
  typename TOutputImage::PointType tempPoint;

  // Use this index to compute the offset everywhere in this class
  outputIndex = outputPtr->GetLargestPossibleRegion().GetIndex();

  // We wish to perform the following mapping of outputIndex to
  // inputIndex on all points in our region
  outputPtr->TransformIndexToPhysicalPoint(outputIndex, tempPoint);
  inputPtr->TransformPhysicalPointToIndex(tempPoint, inputIndex);

  // Given that the size is scaled by a constant factor eq:
  // inputIndex = outputIndex * factorSize
  // is equivalent up to a fixed offset which we now compute
  OffsetValueType zeroOffset = 0;
  for ( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    offsetIndex[i] = inputIndex[i] - outputIndex[i] * m_ShrinkFactors[i];
    // It is plausible that due to small amounts of loss of numerical
    // precision that the offset it negaive, this would cause sampling
    // out of out region, this is insurance against that possibility
    offsetIndex[i] = std::max(zeroOffset, offsetIndex[i]);
    }

  inputRequestedRegionIndex = outputRequestedRegionStartIndex * factorSize + offsetIndex;

  // originally this was
  // inputRequestedRegionSize = outputRequestedRegionSize * factorSize;
  // but since we don't sample edge to edge, we can reduce the size
  for ( i=0; i < TInputImage::ImageDimension; ++i )
    {
    inputRequestedRegionSize[i] = (outputRequestedRegionSize[i] - 1 ) * factorSize[i] + 1;
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetIndex(inputRequestedRegionIndex);
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ShrinkImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const InputImageType * inputPtr = this->GetInput();
  OutputImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro( inputPtr );
  itkAssertInDebugAndIgnoreInReleaseMacro( outputPtr != ITK_NULLPTR );

  // Compute the output spacing, the output image size, and the
  // output image start index
  unsigned int i;
  const typename TInputImage::SpacingType &
  inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TOutputImage::SpacingType outputSpacing;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputStartIndex;

  for ( i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSpacing[i] = inputSpacing[i] * (double)m_ShrinkFactors[i];

    // Round down so that all output pixels fit input input region
    outputSize[i] = static_cast<SizeValueType>(
      std::floor( (double)inputSize[i] / (double)m_ShrinkFactors[i] ) );

    if ( outputSize[i] < 1 )
      {
      outputSize[i] = 1;
      }

    // Because of the later origin shift this starting index is not
    // critical
    outputStartIndex[i] = static_cast<IndexValueType>(
      std::ceil( (double)inputStartIndex[i] / (double)m_ShrinkFactors[i] ) );
    }

  outputPtr->SetSpacing(outputSpacing);

  // Compute origin offset
  // The physical center's of the input and output should be the same
  ContinuousIndex< SpacePrecisionType, TOutputImage::ImageDimension > inputCenterIndex;
  ContinuousIndex< SpacePrecisionType, TOutputImage::ImageDimension > outputCenterIndex;
  for ( i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    inputCenterIndex[i] = inputStartIndex[i] + ( inputSize[i] - 1 ) / 2.0;
    outputCenterIndex[i] = outputStartIndex[i] + ( outputSize[i] - 1 ) / 2.0;
    }

  typename TOutputImage::PointType inputCenterPoint;
  typename TOutputImage::PointType outputCenterPoint;
  inputPtr->TransformContinuousIndexToPhysicalPoint(inputCenterIndex, inputCenterPoint);
  outputPtr->TransformContinuousIndexToPhysicalPoint(outputCenterIndex, outputCenterPoint);

  const typename TOutputImage::PointType & inputOrigin = inputPtr->GetOrigin();
  typename TOutputImage::PointType outputOrigin;
  outputOrigin = inputOrigin + (inputCenterPoint - outputCenterPoint);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
