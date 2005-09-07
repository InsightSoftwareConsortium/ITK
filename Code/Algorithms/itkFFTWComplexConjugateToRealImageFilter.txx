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

#if defined(USE_FFTWF) || defined(USE_FFTWD)
#include "itkFFTWComplexConjugateToRealImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
namespace itk
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/

template <unsigned int Dimension>
void
FFTWComplexConjugateToRealImageFilter<float,Dimension>::
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
  const unsigned int num_dims = outputPtr->GetImageDimension();

  if(num_dims != outputPtr->GetImageDimension())
      {
    return;
      }

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  std::complex<TPixel> *in = const_cast<std::complex<TPixel> *>
    (inputPtr->GetBufferPointer());
  unsigned int total_size=1;
    {
    fftwf_complex *dptr = reinterpret_cast<fftwf_complex *>(in);
    float *out = reinterpret_cast<float *>(outputPtr->GetBufferPointer());
    switch(num_dims)
      {
      case 1:
        M_plan = fftwf_plan_dft_c2r_1d(outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0];
        break;
      case 2:
        M_plan = fftwf_plan_dft_c2r_2d(outputSize[1],outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1];
        break;
      case 3:
        M_plan = fftwf_plan_dft_c2r_3d(outputSize[2],outputSize[1],outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1] * outputSize[2];
        break;
      default:
        int *sizes = new int[num_dims];
        for(unsigned int i = 0; i < num_dims; i++)
          {
          sizes[(num_dims - 1) - i] = outputSize[i];
          total_size *= outputSize[i];
          }
        M_plan = fftwf_plan_dft_c2r(num_dims,sizes,
                                   dptr,out,FFTW_ESTIMATE);
        delete [] sizes;
      }
    M_PlanComputed = true;
    fftwf_execute(M_plan);
    }
     ImageRegionIterator<TOutputImageType> it(outputPtr,outputPtr->GetLargestPossibleRegion());
  while(!it.IsAtEnd())
    {
    it.Set(it.Value() / total_size);
    ++it;
    }
}

template <unsigned int Dimension>
void
FFTWComplexConjugateToRealImageFilter<double,Dimension>::
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
  const unsigned int num_dims = outputPtr->GetImageDimension();

  if(num_dims != outputPtr->GetImageDimension())
      {
    return;
      }

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  std::complex<TPixel> *in = const_cast<std::complex<TPixel> *>
    (inputPtr->GetBufferPointer());
  unsigned int total_size=1;
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
        M_plan = fftw_plan_dft_c2r_2d(outputSize[1],outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1];
        break;
      case 3:
        M_plan = fftw_plan_dft_c2r_3d(outputSize[2],outputSize[1],outputSize[0],
                                      dptr,out,
                                      FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1] * outputSize[2];
        break;
      default:
        int *sizes = new int[num_dims];
        for(unsigned int i = 0; i < num_dims; i++)
          {
          sizes[(num_dims - 1) - i] = outputSize[i];
          total_size *= outputSize[i];
          }
        M_plan = fftw_plan_dft_c2r(num_dims,sizes,
                                   dptr,out,FFTW_ESTIMATE);
        delete [] sizes;
      }
    M_PlanComputed = true;
    fftw_execute(M_plan);
    }
     ImageRegionIterator<TOutputImageType> it(outputPtr,outputPtr->GetLargestPossibleRegion());
  while(!it.IsAtEnd())
    {
    it.Set(it.Value() / total_size);
    ++it;
    }
}

template <unsigned int Dimension>
bool
FFTWComplexConjugateToRealImageFilter<double,Dimension>::
FullMatrix()
{
  return false;
}
template <unsigned int Dimension>
bool
FFTWComplexConjugateToRealImageFilter<float,Dimension>::
FullMatrix()
{
  return false;
}


template <unsigned int Dimension>
void
FFTWComplexConjugateToRealImageFilter<double,Dimension>::
PrintSelf(std::ostream& os,Indent indent) const
{
}
template <unsigned int Dimension>
void
FFTWComplexConjugateToRealImageFilter<float,Dimension>::
PrintSelf(std::ostream& os,Indent indent) const
{
}
}
#endif // defined(USE_FFTWF) || defined(USE_FFTWD)
#endif
