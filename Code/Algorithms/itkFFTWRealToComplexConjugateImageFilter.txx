/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTWRealToComplexConjugateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFFTWRealToComplexConjugateImageFilter_txx
#define _itkFFTWRealToComplexConjugateImageFilter_txx
#if defined(USE_FFTWF) || defined(USE_FFTWD)
#include "itkFFTWRealToComplexConjugateImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"

namespace itk
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/

template <typename TPixel, unsigned int Dimension>
void
FFTWRealToComplexConjugateImageFilter<TPixel,Dimension>::
GenerateData()
{
  // get pointers to the input and output
  typename TInputImageType::ConstPointer  inputPtr  = this->GetInput();
  typename TOutputImageType::Pointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const typename TInputImageType::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TOutputImageType::SizeType&   outputSize
    = outputPtr->GetLargestPossibleRegion().GetSize();

  // figure out sizes
  // size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // the input size and output size only differ in the fastest moving dimension
  unsigned int total_inputSize = 1;
  unsigned int total_outputSize = 1;

  for(unsigned i = 0; i < Dimension; i++)
    {
    total_inputSize *= inputSize[i];
    total_outputSize *= outputSize[i];
    }

  FFTWProxyType proxy;          // shim class to hide FFTW API

  if(this->m_PlanComputed)            // if we've already computed a plan
    {
    // if the image sizes aren't the same,
    // we have to compute the plan again
    if(this->m_LastImageSize != total_inputSize)
      {
      delete [] this->m_InputBuffer;
      delete [] this->m_OutputBuffer;
      proxy.Destroy_plan(this->m_Plan);
      this->m_PlanComputed = false;
      }
    }
  if(!this->m_PlanComputed)
    {
    this->m_InputBuffer = new TPixel[total_inputSize];
    this->m_OutputBuffer = 
      new typename FFTWProxyType::ComplexType[total_outputSize];
    this->m_LastImageSize = total_inputSize;
    switch(Dimension)
      {
      case 1:
        this->m_Plan = proxy.Plan_dft_r2c_1d(inputSize[0],
                                             this->m_InputBuffer,
                                             this->m_OutputBuffer,
                                             FFTW_ESTIMATE);
        break;
      case 2:
        this->m_Plan = proxy.Plan_dft_r2c_2d(inputSize[1],
                                             inputSize[0],
                                             this->m_InputBuffer,
                                             this->m_OutputBuffer,
                                             FFTW_ESTIMATE);
        break;
      case 3:
        this->m_Plan = proxy.Plan_dft_r2c_3d(inputSize[2],
                                             inputSize[1],
                                             inputSize[0],
                                             this->m_InputBuffer,
                                             this->m_OutputBuffer,
                                             FFTW_ESTIMATE);
        break;
      default:
        int *sizes = new int[Dimension];
        for(unsigned int i = 0; i < Dimension; i++)
          {
          sizes[(Dimension - 1) - i] = inputSize[i];
          }

        this->m_Plan = proxy.Plan_dft_r2c(Dimension,sizes,
                                          this->m_InputBuffer,
                                          this->m_OutputBuffer,
                                          FFTW_ESTIMATE);
        delete [] sizes;
      }
    this->m_PlanComputed = true;
    }
  memcpy(this->m_InputBuffer,
         inputPtr->GetBufferPointer(),
         total_inputSize * sizeof(TPixel));
  proxy.Execute(this->m_Plan);
  memcpy(outputPtr->GetBufferPointer(),
         this->m_OutputBuffer,
         total_outputSize * sizeof(typename FFTWProxyType::ComplexType));
}

template <typename TPixel,unsigned int Dimension>
bool
FFTWRealToComplexConjugateImageFilter<TPixel,Dimension>::
FullMatrix()
{
  return false;
}

template <typename TPixel,unsigned int Dimension>
void
FFTWRealToComplexConjugateImageFilter<TPixel,Dimension>::
PrintSelf(std::ostream& os,Indent indent) const
{
}
} // namespace itk
#endif // defined(USE_FFTWF) || defined(USE_FFTWD)
#endif //_itkFFTWRealToComplexConjugateImageFilter_txx
