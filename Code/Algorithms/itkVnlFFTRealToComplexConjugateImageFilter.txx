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
#ifndef _itkVnlFFTRealToComplexConjugateImageFilter_txx
#define _itkVnlFFTRealToComplexConjugateImageFilter_txx
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "vnl/algo/vnl_fft_base.txx"
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/algo/vnl_fft_2d.h"
#include "vnl_fft_3d.h"

VNL_FFT_BASE_INSTANTIATE(3,double);
VNL_FFT_BASE_INSTANTIATE(3,float);
#define DEBUG_PRINT(x) /* */
//#define DEBUG_PRINT(x) x
namespace itk
{

  template <class TPixel, unsigned int Dimension>
  void
  VnlFFTRealToComplexConjugateImageFilter<TPixel,Dimension>::
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
  
    const typename TInputImageType::SizeType&   inputSize
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
        for(i = 0; i < (vec_size / 2 + 1); i++)
          {
          out[i] = std::conj<TPixel>(signal[i]);
          }
        }
        break;
      case 2:
        {
        vnl_fft_2d<TPixel> v2d(inputSize[0],inputSize[1]);
        v2d.vnl_fft_2d<TPixel>::base::transform(signal.data_block(),+1);
        for(i = 0; i < inputSize[1]; i++)
          {
          unsigned int yOffset = i * inputSize[0];
          unsigned int yOffsetOut = i * (inputSize[0]/2 + 1);
          for(j = 0; j < (inputSize[0] / 2 + 1); j++)
            {
            out[yOffsetOut + j] = std::conj<TPixel>(signal[yOffset + j]);
            }
          }
        }
        break;
      case 3:
        {
#if 1
        vnl_fft_3d<TPixel> v3d(inputSize[0],inputSize[1],inputSize[2]);
        v3d.vnl_fft_3d<TPixel>::base::transform(signal.data_block(),+1);
#else
        vnl_fft_2d<TPixel> v2d(inputSize[0],inputSize[1]);
        for(i = 0; i < inputSize[2]; i++)
          {
          unsigned int ZOffset = i * inputSize[1] * inputSize[0];
          v2d.vnl_fft_2d<TPixel>::base::transform((signal.data_block()+ZOffset),+1);
          }
        v2d.vnl_fft_2d<TPixel>::base::transform(signal.data_block(),+1);
        
        for(j = 0; j < inputSize[1]; j++)
          {
          unsigned int signalYStride = j * inputSize[0];
          for(k = 0; k < inputSize[0]; k++)
            {
            vnl_vector< vcl_complex<TPixel> > ZRow(inputSize[2]);
            for(i = 0; i < inputSize[2]; i++)
              {
              unsigned int signalZStride = i * inputSize[1] * inputSize[0];
              ZRow[i] = signal[signalZStride + signalYStride + k];
              }
            vnl_fft_1d<TPixel> v1d(inputSize[2]);
            v1d.fwd_transform(ZRow);
            for(i = 0; i < inputSize[2]; i++)
              {
              unsigned int signalZStride = i * inputSize[1] * inputSize[0];
              signal[signalZStride + signalYStride + k] = ZRow[i];
              }
            
            }
          }
#endif
        DEBUG_PRINT(std::cerr << "INSIDE VNLR2C" << std::endl;)
        for(i = 0; i < inputSize[2]; i++)
          {
          unsigned int outZStride = i * inputSize[1] * (inputSize[0] / 2 + 1);
          unsigned int signalZStride = i * inputSize[1] * inputSize[0];
        
          for(j = 0; j < inputSize[1]; j++)
            {
            unsigned int outYStride = j * (inputSize[0] / 2 + 1);
            unsigned int signalYStride = j * inputSize[0];
            for(k = 0; k < (inputSize[0]/2 + 1); k++)
              {
              out[k + outYStride + outZStride] =
                std::conj<TPixel>(signal[k + signalYStride + signalZStride]);
              //  std::conj<TPixel>(signal[k + signalYStride + signalZStride]);
              DEBUG_PRINT(std::cerr << out[k + outYStride + outZStride] << " ";)
              }
            for(; k < inputSize[0]; k++)
              {
              DEBUG_PRINT(std::cerr << signal[k + signalYStride + signalZStride] << " ";)
              }
            DEBUG_PRINT(std::cerr << std::endl;)
            }
          }
        }
        DEBUG_PRINT(std::cerr << "LEAVING VNLR2C" << std::endl;)
        break;
      default:
        break;
      }
  }

}


#endif
