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
#ifndef itkHalfToFullHermitianImageFilter_hxx
#define itkHalfToFullHermitianImageFilter_hxx

#include "itkHalfToFullHermitianImageFilter.h"

#include "itkImageAlgorithm.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TInputImage >
HalfToFullHermitianImageFilter< TInputImage >
::HalfToFullHermitianImageFilter()
{
  this->ActualXDimensionIsOddOff();
}

template< typename TInputImage >
void
HalfToFullHermitianImageFilter< TInputImage >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  typename InputImageType::ConstPointer inputPtr = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename InputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename OutputImageType::SizeType outputSize;
  typename OutputImageType::IndexType outputStartIndex;

  for ( unsigned int i = 0; i < OutputImageType::ImageDimension; i++ )
    {
    outputSize[i] = inputSize[i];
    outputStartIndex[i] = inputStartIndex[i];
    }
  outputSize[0] = ( inputSize[0] - 1 ) * 2;
  if ( this->GetActualXDimensionIsOdd() )
    {
    outputSize[0]++;
    }

  typename OutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

template< typename TInputImage >
void
HalfToFullHermitianImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  typename InputImageType::Pointer inputPtr  =
    const_cast< InputImageType * >( this->GetInput() );
  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage >
void
HalfToFullHermitianImageFilter< TInputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Get pointers to the input and output.
  typename InputImageType::ConstPointer inputPtr = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  InputImageRegionType inputRegion = inputPtr->GetLargestPossibleRegion();
  const InputImageIndexType & inputRegionIndex = inputRegion.GetIndex();
  const InputImageSizeType &  inputRegionSize = inputRegion.GetSize();
  InputImageIndexType  inputRegionMaximumIndex = inputRegionIndex + inputRegionSize;

  // Copy the non-reflected region.
  OutputImageRegionType copyRegion( outputRegionForThread );
  bool copy = copyRegion.Crop( inputRegion );
  float initialProgress = 0.0f;
  if ( copy )
    {
    initialProgress = static_cast< float >( copyRegion.GetNumberOfPixels() ) /
      static_cast< float >( outputRegionForThread.GetNumberOfPixels() );
    }

  // Set up the ProgressReporter.
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels(),
                             100, initialProgress );

  if ( copy )
    {
    ImageAlgorithm::Copy( inputPtr.GetPointer(), outputPtr.GetPointer(),
                          copyRegion, copyRegion );
    }

  // Now copy the redundant complex conjugate region, if there is one
  // in this thread's output region.
  OutputImageIndexType outputRegionIndex = outputRegionForThread.GetIndex();
  OutputImageSizeType  outputRegionSize = outputRegionForThread.GetSize();
  OutputImageIndexType outputRegionMaximumIndex = outputRegionIndex + outputRegionSize;

  if ( outputRegionMaximumIndex[0] > inputRegionMaximumIndex[0] )
    {
    OutputImageIndexType conjugateRegionIndex( outputRegionIndex );
    conjugateRegionIndex[0] = std::max(outputRegionIndex[0], inputRegionMaximumIndex[0] );
    OutputImageSizeType conjugateRegionSize( outputRegionSize );
    conjugateRegionSize[0] = outputRegionMaximumIndex[0] - conjugateRegionIndex[0];
    OutputImageRegionType conjugateRegion( conjugateRegionIndex, conjugateRegionSize );

    ImageRegionIteratorWithIndex< OutputImageType > oIt( outputPtr, conjugateRegion );
    for (oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt)
      {
      OutputImageIndexType conjugateIndex = oIt.GetIndex();

      // Flip the indices in each dimension.
      OutputImageIndexType index( conjugateIndex );
      for (unsigned int i = 0; i < ImageDimension; ++i)
        {
        OutputImageRegionType outputLargestPossibleRegion =
          outputPtr->GetLargestPossibleRegion();
        OutputImageIndexType outputLargestPossibleRegionIndex =
          outputLargestPossibleRegion.GetIndex();
        OutputImageSizeType outputLargestPossibleRegionSize =
          outputLargestPossibleRegion.GetSize();
        if ( conjugateIndex[i] != outputLargestPossibleRegionIndex[i] )
          {
          index[i] = outputLargestPossibleRegionSize[i] - conjugateIndex[i] +
            2 * outputLargestPossibleRegionIndex[i];
          }
        }

      oIt.Set( std::conj( inputPtr->GetPixel( index ) ) );
      progress.CompletedPixel();
      }
    }
}

} // end namespace itk

#endif // itkHalfToFullHermitianImageFilter_hxx
