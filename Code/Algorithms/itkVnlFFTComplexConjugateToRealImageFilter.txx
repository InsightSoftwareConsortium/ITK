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
#ifndef _itkVnlFFTComplexConjugateToRealImageFilter_txx
#define _itkVnlFFTComplexConjugateToRealImageFilter_txx

#include "itkVnlFFTComplexConjugateToRealImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/algo/vnl_fft_2d.h"
#include "vnl_fft_3d.h"
#define DEBUG_PRINT(x) /* */
//#define DEBUG_PRINT(x) x

namespace itk
{

  template <typename TPixel, unsigned int Dimension>
  void
  VnlFFTComplexConjugateToRealImageFilter<TPixel,Dimension>::
  GenerateData()
  {
    unsigned int i,j,k;
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
  
    TPixel *out = outputPtr->GetBufferPointer();
  
    //
    // have to fill out last dimension
    // because of conjugate symmetry
    unsigned int inputXSize = outputSize[0] / 2 + 1;
    switch(num_dims)
      {
      case 1:
        for(i = 0; i < inputXSize; i++)
          {
          signal[i] = std::conj<TPixel>(in[i]);
          }
        for(; i < outputSize[0]; i++)
          {
          signal[i] = in[(outputSize[0] - i) % outputSize[0]];
          }
        break;
      case 2:
        DEBUG_PRINT(std::cerr << "INSIDE VNLC2R" << std::endl;)
        for(i = 0; i < outputSize[1]; i++)
          {
          unsigned int YStride = i * outputSize[0];
          unsigned int YStrideIn = i * inputXSize;
          unsigned int YStrideConj = ((outputSize[1] - i) % outputSize[1]) * inputXSize;
          for(j = 0; j < inputXSize; j++)
            {
            signal[YStride + j] = std::conj<TPixel>(in[YStrideIn+j]);
            DEBUG_PRINT(std::cerr << signal[YStride + j] << " ";)
            }
          for(;j < outputSize[0]; j++)
            {
            signal[YStride + j] = 
              in[YStrideConj + ((outputSize[0] - j) % outputSize[0])];
            DEBUG_PRINT(std::cerr << signal[YStride + j] << " ";)
            }
          DEBUG_PRINT(std::cerr << std::endl;)
          }
        DEBUG_PRINT(std::cerr << "LEAVING VNLC2R" << std::endl;)
        break;
      case 3:
        DEBUG_PRINT(std::cerr << "INSIDE VNLC2R" << std::endl;)
        for(i = 0; i < outputSize[2]; i++)
          {
          unsigned int ZStride = i * outputSize[1] * outputSize[0];
          unsigned int ZStrideIn = i * outputSize[1] * inputXSize;
          unsigned int ZStrideConj = ((outputSize[2] - i) % outputSize[2]) * 
            outputSize[1] * inputXSize;
          for(j = 0; j < outputSize[1]; j++)
            {
            unsigned int YStride = j * outputSize[0];
            unsigned int YStrideIn = j * inputXSize;
            unsigned int YStrideConj = ((outputSize[1] - j) % outputSize[1]) * 
              inputXSize;
            for(k = 0; k < inputXSize; k++)
              {
              signal[ZStride + YStride + k] =
                std::conj<TPixel>(in[ZStrideIn + YStrideIn + k]);
              DEBUG_PRINT(std::cerr << signal[ZStride + YStride + k] << " ";)
              }
            for(;k < outputSize[0]; k++)
              {
              signal[ZStride + YStride + k ] =
                in[ZStrideConj+YStrideConj+((outputSize[0] - k) % outputSize[0])];
              DEBUG_PRINT(std::cerr << signal[ZStride + YStride + k] << " ";)
              }
            DEBUG_PRINT(std::cerr << std::endl;)
            }
          }
        DEBUG_PRINT(std::cerr << "LEAVING VNLC2R" << std::endl;)
      
        break;
      default:
        break;
      }
    switch(num_dims)
      {
      case 1:
        {
        vnl_fft_1d<TPixel> v1d(vec_size);
        v1d.bwd_transform(signal);
        for(i = 0; i < vec_size; i++)
          {
          out[i] = std::real<TPixel>(signal[i]) / vec_size;
          }
        }
        break;
      case 2:
        {
        vnl_fft_2d<TPixel> v2d(outputSize[0],outputSize[1]);
        v2d.vnl_fft_2d<TPixel>::base::transform(signal.data_block(),-1);
        for(i = 0; i < outputSize[1]; i++)
          {
          unsigned int yOffset = i * outputSize[0];
          for(j = 0; j < outputSize[0]; j++)
            {
            out[yOffset + j] = std::real<TPixel>(signal[yOffset + j]) / vec_size;
            }
          }
        }
        break;
      case 3:
        {
        vnl_fft_3d<TPixel> v3d(outputSize[0],outputSize[1],outputSize[2]);
        v3d.vnl_fft_3d<TPixel>::base::transform(signal.data_block(),-1);
        for(i = 0; i < outputSize[2]; i++)
          {
          unsigned int outZStride = i * outputSize[1] * outputSize[0];
          for(j = 0; j < outputSize[1]; j++)
            {
            unsigned int outYStride = j * outputSize[0];
            for(k = 0; k < outputSize[0]; k++)
              {
              out[k + outYStride + outZStride] =
                std::real<TPixel>(signal[k + outYStride + outZStride]) / vec_size;
              }
            }
          }
        }
        break;
      default:
        break;
      }
  }


}
#endif
