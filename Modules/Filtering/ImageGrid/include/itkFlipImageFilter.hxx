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
#ifndef itkFlipImageFilter_hxx
#define itkFlipImageFilter_hxx

#include "itkFlipImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TImage >
FlipImageFilter< TImage >
::FlipImageFilter() :
  m_FlipAboutOrigin( true )
{
  m_FlipAxes.Fill(false);
}

template< typename TImage >
void
FlipImageFilter< TImage >
::GenerateOutputInformation()
{
  // Call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< TImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename TImage::DirectionType & inputDirection = inputPtr->GetDirection();
  const typename TImage::SizeType & inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType & inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TImage::PointType outputOrigin;
  typename TImage::IndexType newIndex = inputStartIndex;

  typename TImage::DirectionType flipMatrix;
  flipMatrix.SetIdentity();

  // Need the coordinate of the pixel that will become the first pixel
  // and need a matrix to model the flip
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
      // If flipping the axis, then we need to know the last pixel in
      // that dimension
      newIndex[j] += ( inputSize[j] - 1 );

      // What we really want is the index padded out past this point
      // by the amount the start index is from [0,0,0] (because the
      // output regions have the same index layout as the input
      // regions)
      newIndex[j] += inputStartIndex[j];

      // Only flip the directions if we are NOT flipping about the
      // origin (when flipping about the origin, the pixels are
      // ordered in the same direction as the input directions. when
      // NOT flipping about the origin, the pixels traverse space in
      // the opposite direction. when flipping about the origin,
      // increasing indices traverse space in the same direction as
      // the original data.).
      if ( !m_FlipAboutOrigin )
        {
        flipMatrix[j][j] = -1.0;
        }
      }
    }

  inputPtr->TransformIndexToPhysicalPoint(newIndex, outputOrigin);

  // Finally, flip about the origin if needed
  if ( m_FlipAboutOrigin )
    {
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      if ( m_FlipAxes[j] )
        {
        outputOrigin[j] *= -1;
        }
      }
    }

  outputPtr->SetDirection(inputDirection * flipMatrix);
  outputPtr->SetOrigin(outputOrigin);
}

template< typename TImage >
void
FlipImageFilter< TImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass's implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< TImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename TImage::SizeType & outputRequestedSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TImage::IndexType & outputRequestedIndex =
    outputPtr->GetRequestedRegion().GetIndex();

  const typename TImage::SizeType & outputLargestPossibleSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType & outputLargestPossibleIndex =
    outputPtr->GetLargestPossibleRegion().GetIndex();

  IndexType inputRequestedIndex(outputRequestedIndex);

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
      inputRequestedIndex[j] =
        2 * outputLargestPossibleIndex[j]
        + static_cast< IndexValueType >( outputLargestPossibleSize[j] )
        - static_cast< IndexValueType >( outputRequestedSize[j] )
        - outputRequestedIndex[j];
      }
    }

  typename TImage::RegionType inputRequestedRegion( inputRequestedIndex, outputRequestedSize);

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

template< typename TImage >
void
FlipImageFilter< TImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Get the input and output pointers
  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  // Support progress methods/callbacks
  const typename OutputImageRegionType::SizeType &regionSize = outputRegionForThread.GetSize();
  const size_t numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / regionSize[0];
  ProgressReporter progress( this, threadId, static_cast<SizeValueType>( numberOfLinesToProcess ) );

  const typename TImage::SizeType & outputLargestPossibleSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType & outputLargestPossibleIndex =
    outputPtr->GetLargestPossibleRegion().GetIndex();

  // Compute the input region the output region maps to
  typename TImage::RegionType inputReginForThread( outputRegionForThread );
   for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
      const IndexValueType idx =
        2 * outputLargestPossibleIndex[j]
        + static_cast< IndexValueType >( outputLargestPossibleSize[j] )
        - static_cast< IndexValueType >( outputRegionForThread.GetSize(j) )
        - outputRegionForThread.GetIndex(j);
      inputReginForThread.SetIndex(j, idx);
      }
    }

  // Setup output region iterator
  ImageScanlineIterator< TImage > outputIt(outputPtr, outputRegionForThread);
  ImageScanlineConstIterator< TImage > inputIter(inputPtr, inputReginForThread);

  IndexValueType offset[ImageDimension];
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
      offset[j] = 2 * outputLargestPossibleIndex[j]
                  + static_cast< IndexValueType >( outputLargestPossibleSize[j] ) - 1;
      }
    else
      {
      offset[j] = 0;
      }
    }

  outputIt.GoToBegin();
  while ( !outputIt.IsAtEnd() )
    {

    // Determine the index of the output line
    const typename TImage::IndexType outputIndex = outputIt.GetIndex();

    // Determine the input pixel location associated with the start of
    // the line
    typename TImage::IndexType inputIndex(outputIndex);
    for ( unsigned int j = 0; j < ImageDimension; ++j )
      {
      if ( m_FlipAxes[j] )
        {
        inputIndex[j] = -1 * outputIndex[j] + offset[j];
        }
      }
    inputIter.SetIndex(inputIndex);

    if ( m_FlipAxes[0] )
      {
      // Move the across the output scanline
      while ( !outputIt.IsAtEndOfLine() )
        {
        // Copy the input pixel to the output
        outputIt.Set( inputIter.Get() );

        ++outputIt;
        // Read the input scaneline in reverse
        --inputIter;
        }
      }
    else
      {
      // Move the across the output scanline
      while ( !outputIt.IsAtEndOfLine() )
        {
        // Copy the input pixel to the output
        outputIt.Set( inputIter.Get() );

        ++outputIt;
        ++inputIter;
        }
      }

      outputIt.NextLine();
      progress.CompletedPixel();

    }
}

template< typename TImage >
void
FlipImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FlipAxes: " << m_FlipAxes << std::endl;
  os << indent << "FlipAboutOrigin: " << m_FlipAboutOrigin << std::endl;
}
} // namespace itk

#endif
