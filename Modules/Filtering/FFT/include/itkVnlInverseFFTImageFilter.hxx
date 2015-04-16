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
#ifndef itkVnlInverseFFTImageFilter_hxx
#define itkVnlInverseFFTImageFilter_hxx

#include "itkInverseFFTImageFilter.hxx"
#include "itkProgressReporter.h"
#include "itkVnlFFTCommon.h"
#include "itkVnlInverseFFTImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
void
VnlInverseFFTImageFilter< TInputImage, TOutputImage >
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

  const OutputSizeType outputSize = outputPtr->GetLargestPossibleRegion().GetSize();

  // Allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const InputPixelType *in = inputPtr->GetBufferPointer();

  unsigned int vectorSize = 1;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( !VnlFFTCommon::IsDimensionSizeLegal( outputSize[i] ) )
      {
      itkExceptionMacro(<< "Cannot compute FFT of image with size "
                        << outputSize << ". VnlInverseFFTImageFilter operates "
                        << "only on images whose size in each dimension is a multiple of "
                        << "2, 3, or 5." );
      }
    vectorSize *= outputSize[i];
    }

  SignalVectorType signal( vectorSize );
  for (unsigned int i = 0; i < vectorSize; i++ )
    {
    signal[i] = in[i];
    }

  OutputPixelType *out = outputPtr->GetBufferPointer();

  // call the proper transform, based on compile type template parameter
  VnlFFTCommon::VnlFFTTransform< OutputImageType > vnlfft( outputSize );
  vnlfft.transform( signal.data_block(), 1 );

  // Copy the VNL output back to the ITK image.
  // Extract the real part of the signal.
  // Ideally, the normalization by the number of elements
  // should have been accounted for by the VNL inverse Fourier transform,
  // but it is not.  So, we take care of it by dividing the signal by
  // the vectorSize.
  for ( unsigned int i = 0; i < vectorSize; i++ )
    {
    out[i] = signal[i].real() / vectorSize;
    }
}

template< typename TInputImage, typename TOutputImage >
SizeValueType
VnlInverseFFTImageFilter< TInputImage, TOutputImage >
::GetSizeGreatestPrimeFactor() const
{
  return VnlFFTCommon::GREATEST_PRIME_FACTOR;
}

}
#endif
