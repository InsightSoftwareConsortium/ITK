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
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_hxx
#define __itkVnlFFTRealToComplexConjugateImageFilter_hxx

#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkFFTRealToComplexConjugateImageFilter.hxx"
#include "itkProgressReporter.h"

#include "vnl/algo/vnl_fft_base.h"
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/algo/vnl_fft_2d.h"
#include "vnl_fft_3d.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
bool VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >
::IsDimensionSizeLegal(InputSizeValueType n)
{
  int ifac = 2;

  for ( int l = 1; l <= 3; l++ )
    {
    for (; n % ifac == 0; )
      {
      n /= ifac;
      }
    ifac += l;
    }
  return ( n == 1 ); // return false if decomposition failed
}

/** run vnl fft transform
 * In the following, we use the VNL "bwd_transform" even though this
 * filter is actually taking the forward transform.  This is done
 * because the VNL definitions are switched from the standard
 * definition.  The standard definition uses a negative exponent for
 * the forward transform and positive for the reverse transform.
 * VNL does the opposite.
 */
template< class TInputImage, class TOutputImage >
void
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >
::FFTND_transform(SignalVectorType &signal, const InputSizeType &inputSize, DimDiscriminator<1> *)
{
  vnl_fft_1d< InputPixelType > v1d( inputSize[0] );
  v1d.vnl_fft_1d< InputPixelType >::base::transform( signal.data_block(), -1 );
}

template< class TInputImage, class TOutputImage >
void
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >
::FFTND_transform(SignalVectorType &signal, const InputSizeType &inputSize, DimDiscriminator<2> *)
{
  vnl_fft_2d< InputPixelType > v2d( inputSize[1], inputSize[0] );
  v2d.vnl_fft_2d< InputPixelType >::base::transform( signal.data_block(), -1 );
}

template< class TInputImage, class TOutputImage >
void
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >
::FFTND_transform(SignalVectorType &signal, const InputSizeType &inputSize, DimDiscriminator<3> *)
{
  vnl_fft_3d< InputPixelType > v3d( inputSize[2], inputSize[1], inputSize[0] );
  v3d.vnl_fft_3d< InputPixelType >::base::transform( signal.data_block(), -1 );
}

template< class TInputImage, class TOutputImage >
void
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >
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
  // reports the begining and the end of the process.
  ProgressReporter progress( this, 0, 1 );

  const InputSizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();
  OutputPixelType *out = outputPtr->GetBufferPointer();

  unsigned int vectorSize = 1;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( !this->IsDimensionSizeLegal( inputSize[i] ) )
      {
      itkExceptionMacro(<< "Cannot compute FFT of image with size "
                        << inputSize << ". VnlFFTRealToComplexConjugateImageFilter operates "
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
  this->FFTND_transform(signal, inputSize, static_cast<DimDiscriminator<ImageDimension> *>(0));

  // Copy the VNL output back to the ITK image.
  for ( unsigned int i = 0; i < vectorSize; i++ )
    {
    out[i] = signal[i];
    }
}

template< class TInputImage, class TOutputImage >
bool
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >
::FullMatrix()
{
  return true;
}
}

#endif
