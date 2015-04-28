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
#ifndef itkVnlForwardFFTImageFilter_hxx
#define itkVnlForwardFFTImageFilter_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkForwardFFTImageFilter.hxx"
#include "itkProgressReporter.h"
#include "itkVnlFFTCommon.h"
#include "itkVnlForwardFFTImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
void
VnlForwardFFTImageFilter< TInputImage, TOutputImage >
::GenerateData()
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
  ProgressReporter progress( this, 0, 1 );

  const InputSizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  unsigned int vectorSize = 1;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( !VnlFFTCommon::IsDimensionSizeLegal( inputSize[i] ) )
      {
      itkExceptionMacro(<< "Cannot compute FFT of image with size "
                        << inputSize << ". VnlForwardFFTImageFilter operates "
                        << "only on images whose size in each dimension is a multiple of "
                        << "2, 3, or 5." );
      }
    vectorSize *= inputSize[i];
    }

  const InputPixelType *in = inputPtr->GetBufferPointer();
  SignalVectorType signal( vectorSize );
  for ( unsigned int i = 0; i < vectorSize; i++ )
    {
    signal[i] = in[i];
    }

  // call the proper transform, based on compile type template parameter
  VnlFFTCommon::VnlFFTTransform< InputImageType > vnlfft( inputSize );
  vnlfft.transform( signal.data_block(), -1 );

  // Copy the VNL output back to the ITK image.
  ImageRegionIteratorWithIndex< TOutputImage > oIt( outputPtr,
                                                    outputPtr->GetLargestPossibleRegion() );
  for (oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt)
    {
    typename OutputImageType::IndexType index = oIt.GetIndex();
    typename OutputImageType::OffsetValueType offset = inputPtr->ComputeOffset( index );
    oIt.Set( signal[offset] );
    }
}

template< typename TInputImage, typename TOutputImage >
SizeValueType
VnlForwardFFTImageFilter< TInputImage, TOutputImage >
::GetSizeGreatestPrimeFactor() const
{
  return VnlFFTCommon::GREATEST_PRIME_FACTOR;
}

}

#endif
