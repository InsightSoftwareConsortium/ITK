/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkVnlFFTRealToComplexConjugateImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_txx
#define __itkVnlFFTRealToComplexConjugateImageFilter_txx
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkFFTRealToComplexConjugateImageFilter.txx"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkExceptionObject.h"
#include "vnl/algo/vnl_fft_base.txx"
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/algo/vnl_fft_2d.h"
#include "vnl_fft_3d.h"

VNL_FFT_BASE_INSTANTIATE(3,double);
VNL_FFT_BASE_INSTANTIATE(3,float);
//#define DEBUG_PRINT(x) /* */
//#define DEBUG_PRINT(x) x

namespace itk
{
//#define DEBUG_PRINT(x) /* */
#define DEBUG_PRINT(x) x

  template <class TPixel, unsigned int Dimension>
  bool
  VnlFFTRealToComplexConjugateImageFilter<TPixel,Dimension>::
  Legaldim(int n)
  {
    int ifac = 2;
    for (int l = 1; l <= 3; l++) 
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
      for(; n % ifac == 0;)
        {
        n /= ifac;
        }
      ifac += l;
      }
    return (n == 1); // return false if decomposition failed
  }

  template <class TPixel, unsigned int Dimension>
  void
  VnlFFTRealToComplexConjugateImageFilter<TPixel,Dimension>::
  GenerateData()
  {
    unsigned int i;
    // get pointers to the input and output
    typename Superclass::TInputImageType::ConstPointer  inputPtr  = this->GetInput();
    typename Superclass::TOutputImageType::Pointer      outputPtr = this->GetOutput();

    if ( !inputPtr || !outputPtr )
      {
      return;
      }
  
    const typename Superclass::TInputImageType::SizeType&   inputSize
      = inputPtr->GetLargestPossibleRegion().GetSize();
    unsigned int num_dims = inputPtr->GetImageDimension();

    if(num_dims != outputPtr->GetImageDimension())
      return;

    TPixel *in = const_cast<TPixel *>(inputPtr->GetBufferPointer());
    outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
    outputPtr->Allocate();
    std::complex<TPixel> *out = outputPtr->GetBufferPointer();

    unsigned int vec_size = 1;
    for(i = 0; i < num_dims; i++)
      {
      //#if 0
      if( !this->Legaldim(inputSize[i]) ) {
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Illegal Array DIM for FFT");
        exception.SetLocation(ITK_LOCATION);
        throw exception;
      }
      //#endif
      vec_size *= inputSize[i];
      }
    vnl_vector< vcl_complex<TPixel> > signal(vec_size);
    for(i = 0; i < vec_size; i++)
      {
      signal[i] = in[i];
      }

    switch(num_dims)
      {
      case 1:
        {
        vnl_fft_1d<TPixel> v1d(vec_size);
        v1d.fwd_transform(signal);
        }
        break;
      case 2:
        {
        vnl_fft_2d<TPixel> v2d(inputSize[0],inputSize[1]);
        v2d.vnl_fft_2d<TPixel>::base::transform(signal.data_block(),+1);
        }
        break;
      case 3:
        {
        vnl_fft_3d<TPixel> v3d(inputSize[0],inputSize[1],inputSize[2]);
        v3d.vnl_fft_3d<TPixel>::base::transform(signal.data_block(),+1);
        }
        break;
      default:
        break;
      }
    for(i = 0; i < vec_size; i++)
      {
      out[i] = signal[i];
      }
  }

  template <class TPixel, unsigned int Dimension>
  bool
  VnlFFTRealToComplexConjugateImageFilter<TPixel,Dimension>::
  FullMatrix()
  {
    return true;
  }

}


#endif
