/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTWComplexConjugateToRealImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFFTWComplexConjugateToRealImageFilter_txx
#define _itkFFTWComplexConjugateToRealImageFilter_txx

#include "itkFFTWComplexConjugateToRealImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
namespace itk
{

template <class TPixel, unsigned int Dimension>
void
FFTWComplexConjugateToRealImageFilter<TPixel,Dimension>::
GenerateData()
{
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
  unsigned int total_size;
  if(typeid(TPixel) == typeid(double))
    {
    fftw_complex *dptr = reinterpret_cast<fftw_complex *>(in);
    double *out = reinterpret_cast<double *>(outputPtr->GetBufferPointer());
    switch(num_dims)
      {
      case 1:
        M_plan = fftw_plan_dft_c2r_1d(outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0];
        break;
      case 2:
        M_plan = fftw_plan_dft_c2r_2d(outputSize[0],outputSize[1],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1];
        break;
      case 3:
        M_plan = fftw_plan_dft_c2r_3d(outputSize[0],outputSize[1],outputSize[2],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1] * outputSize[2];
        break;
      default:
        int *sizes = new int[num_dims];
        total_size = 1;
        for(unsigned int i = 0; i < num_dims; i++)
          {
          sizes[(num_dims - 1) - i] = outputSize[i];
          total_size *= outputSize[i];
          }
      
        M_plan = fftw_plan_dft_c2r(num_dims,sizes,
                                   dptr,out,FFTW_ESTIMATE);
        delete []sizes;
      }
    M_PlanComputed = true;
    fftw_execute(M_plan);
    }
  else if(typeid(TPixel) == typeid(float))
    {
    fftwf_complex *dptr = reinterpret_cast<fftwf_complex *>(in);
    float *out = reinterpret_cast<float *>(outputPtr->GetBufferPointer());
    switch(num_dims)
      {
      case 1:
        M_planf = fftwf_plan_dft_c2r_1d(outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0];
        break;
      case 2:
        M_planf = fftwf_plan_dft_c2r_2d(outputSize[0],outputSize[1],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1];
        break;
      case 3:
        M_planf = fftwf_plan_dft_c2r_3d(outputSize[0],outputSize[1],outputSize[2],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1] * outputSize[2];
        break;
      default:
        int *sizes = new int[num_dims];
        total_size = 1;
        for(unsigned int i = 0; i < num_dims; i++)
          {
          sizes[(num_dims - 1) - i] = outputSize[i];
          total_size *= outputSize[i];
          }
      
        M_planf = fftwf_plan_dft_c2r(num_dims,sizes,
                                   dptr,out,FFTW_ESTIMATE);
        delete []sizes;
      }
    M_PlanComputedf = true;
    fftwf_execute(M_planf);
    }
     ImageRegionIterator<TOutputImageType> it(outputPtr,outputPtr->GetLargestPossibleRegion());
  while(!it.IsAtEnd())
    {
    it.Set(it.Value() / total_size);
    ++it;
    }
}


template <class TPixel, unsigned int Dimension>
void
FFTWComplexConjugateToRealImageFilter<TPixel,Dimension>::
PrintSelf(std::ostream& os,Indent indent) const
{
}
}
#include "itkFFTWComplexConjugateToRealImageFilter.h"

#endif
