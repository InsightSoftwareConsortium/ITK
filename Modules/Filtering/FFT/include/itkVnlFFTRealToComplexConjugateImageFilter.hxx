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
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "vnl/algo/vnl_fft_base.h"
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/algo/vnl_fft_2d.h"
#include "vnl_fft_3d.h"
#include "itkProgressReporter.h"

namespace itk
{
#define DEBUG_PRINT(x) x

template< class TInputImage, class TOutputImage >
bool VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >::Legaldim(int n)
{
  int ifac = 2;

  for ( int l = 1; l <= 3; l++ )
    {
    // Original code
//       k = 0;
//       L10:
//       if (n % ifac != 0) goto L20;
//       ++k;
//       N /= ifac;
//       goto L10;
//       L20:
//       pqr[l-1] = k;
//       ifac += l;
    for (; n % ifac == 0; )
      {
      n /= ifac;
      }
    ifac += l;
    }
  return ( n == 1 ); // return false if decomposition failed
}

template< class TInputImage, class TOutputImage >
void
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >::GenerateData()
{
  unsigned int i;

  // get pointers to the input and output
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we don't have a nice progress to report, but at least this simple line
  // reports the begining and the end of the process
  ProgressReporter progress(this, 0, 1);

  const typename InputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  unsigned int num_dims = inputPtr->GetImageDimension();

  if ( num_dims != outputPtr->GetImageDimension() )
    {
    return;
    }
  InputPixelType *in = const_cast< InputPixelType * >( inputPtr->GetBufferPointer() );
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();
  OutputPixelType *out = outputPtr->GetBufferPointer();

  unsigned int vec_size = 1;
  for ( i = 0; i < num_dims; i++ )
    {
    //#if 0
    if ( !this->Legaldim(inputSize[i]) )
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Illegal Array DIM for FFT");
      exception.SetLocation(ITK_LOCATION);
      throw exception;
      }
    //#endif
    vec_size *= inputSize[i];
    }
  vnl_vector< vcl_complex< InputPixelType > > signal(vec_size);
  for ( i = 0; i < vec_size; i++ )
    {
    signal[i] = in[i];
    }

  switch ( num_dims )
    {
    case 1:
      {
      vnl_fft_1d< InputPixelType > v1d(vec_size);
      v1d.bwd_transform(signal);
      }
      break;
    case 2:
      {
      vnl_fft_2d< InputPixelType > v2d(inputSize[1], inputSize[0]);
      v2d.vnl_fft_2d< InputPixelType >::base::transform(signal.data_block(), -1);
      }
      break;
    case 3:
      {
      vnl_fft_3d< InputPixelType > v3d(inputSize[2], inputSize[1], inputSize[0]);
      v3d.vnl_fft_3d< InputPixelType >::base::transform(signal.data_block(), -1);
      }
      break;
    default:
      break;
    }
  for ( i = 0; i < vec_size; i++ )
    {
    out[i] = signal[i];
    }
}

template< class TInputImage, class TOutputImage >
bool
VnlFFTRealToComplexConjugateImageFilter< TInputImage, TOutputImage >::FullMatrix()
{
  return true;
}
}

#endif
