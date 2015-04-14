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
#ifndef itkFullToHalfHermitianImageFilter_hxx
#define itkFullToHalfHermitianImageFilter_hxx

#include "itkFullToHalfHermitianImageFilter.h"

#include "itkImageAlgorithm.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TInputImage >
FullToHalfHermitianImageFilter< TInputImage >
::FullToHalfHermitianImageFilter()
{
  this->SetActualXDimensionIsOdd(false);
}

template< typename TInputImage >
void
FullToHalfHermitianImageFilter< TInputImage >
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
  outputSize[0] = ( inputSize[0] / 2 ) + 1;

  typename OutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
  this->SetActualXDimensionIsOdd( inputSize[0] % 2 != 0 );
}

template< typename TInputImage >
void
FullToHalfHermitianImageFilter< TInputImage >
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
FullToHalfHermitianImageFilter< TInputImage >
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

  // We don't have a nice progress to report, but at least this simple line
  // reports the beginning and the end of the process.
  ProgressReporter progress(this, threadId, 1);

  // Copy the non-reflected region.
  ImageAlgorithm::Copy( inputPtr.GetPointer(), outputPtr.GetPointer(),
                        outputRegionForThread, outputRegionForThread );
}

} // end namespace itk

#endif // itkFullToHalfHermitianImageFilter_hxx
