/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkVnlFFTComplexConjugateToRealImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVnlFFTComplexConjugateToRealImageFilter_txx
#define __itkVnlFFTComplexConjugateToRealImageFilter_txx

#include "itkVnlFFTComplexConjugateToRealImageFilter.h"
#include "itkFFTComplexConjugateToRealImageFilter.txx"
#include <complex>
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/algo/vnl_fft_2d.h"
#include "vnl_fft_3d.h"

namespace itk
{

  template <class TPixel, unsigned int Dimension>
  void
  VnlFFTComplexConjugateToRealImageFilter<TPixel,Dimension>::
  GenerateData()
  {
    unsigned int i;
    // get pointers to the input and output
    typename TInputImageType::ConstPointer  inputPtr  = this->GetInput();
    typename TOutputImageType::Pointer      outputPtr = this->GetOutput();

    if ( !inputPtr || !outputPtr )
      {
      return;
      }
  
    const typename TInputImageType::SizeType&   outputSize
      = outputPtr->GetLargestPossibleRegion().GetSize();
    unsigned int num_dims = outputPtr->GetImageDimension();

    if(num_dims != outputPtr->GetImageDimension())
      return;

    // allocate output buffer memory
    outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
    outputPtr->Allocate();

    std::complex<TPixel> *in = const_cast<std::complex<TPixel> *>
      (inputPtr->GetBufferPointer());
  
    unsigned int vec_size = 1;
    for(i = 0; i < num_dims; i++)
      {
      vec_size *= outputSize[i];
      }

    vnl_vector< vcl_complex<TPixel> > signal(vec_size);
    for(i = 0; i < vec_size; i++)
      signal[i] = in[i];

    TPixel *out = outputPtr->GetBufferPointer();
  
    switch(num_dims)
      {
      case 1:
        {
        vnl_fft_1d<TPixel> v1d(vec_size);
        v1d.bwd_transform(signal);
        }
        break;
      case 2:
        {
        vnl_fft_2d<TPixel> v2d(outputSize[0],outputSize[1]);
        v2d.vnl_fft_2d<TPixel>::base::transform(signal.data_block(),-1);
        }
        break;
      case 3:
        {
        vnl_fft_3d<TPixel> v3d(outputSize[0],outputSize[1],outputSize[2]);
        v3d.vnl_fft_3d<TPixel>::base::transform(signal.data_block(),-1);
        }
        break;
      default:
        break;
      }
    for(i = 0; i < vec_size; i++)
      {
      out[i] = signal[i].real() / vec_size;
      }
  }

  template <class TPixel, unsigned int Dimension>
  bool
  VnlFFTComplexConjugateToRealImageFilter<TPixel,Dimension>::
  FullMatrix()
  {
    return true;
  }

}
#endif
