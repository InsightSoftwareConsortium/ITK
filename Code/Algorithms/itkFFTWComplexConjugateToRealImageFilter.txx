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
#include "itkImageRegionIterator.h"

namespace itk
{

template <typename TPixel, unsigned int Dimension>
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

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  const typename TInputImageType::SizeType&   outputSize
    = outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TOutputImageType::SizeType& inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();

  // figure out sizes
  // size of input and output aren't the same which is handled in the superclass,
  // sort of.
  // the input size and output size only differ in the fastest moving dimension
  unsigned int total_outputSize = 1;
  unsigned int total_inputSize = 1;

  for(unsigned i = 0; i < Dimension; i++)
    {
    total_outputSize *= outputSize[i];
    total_inputSize *= inputSize[i];
    }

  if(this->m_PlanComputed)            // if we've already computed a plan
    {
    // if the image sizes aren't the same,
    // we have to compute the plan again
    if(this->m_LastImageSize != total_outputSize)
      {
      delete [] this->m_InputBuffer;
      delete [] this->m_OutputBuffer;
      FFTWProxyType::DestroyPlan(this->m_Plan);
      this->m_PlanComputed = false;
      }
    }
  // either plan never computed, or need to re-compute
  if(!this->m_PlanComputed)
    {
    // if we've never computed the plan, or we need to redo it
    this->m_InputBuffer = new typename FFTWProxyType::ComplexType[total_inputSize];
    this->m_OutputBuffer = new TPixel[total_outputSize];
    this->m_LastImageSize = total_outputSize;

    switch(Dimension)
      {
      case 1:
        this->m_Plan = FFTWProxyType::Plan_dft_c2r_1d(outputSize[0],
                                       this->m_InputBuffer,this->m_OutputBuffer,
                                       FFTW_ESTIMATE);
        break;
      case 2:
        this->m_Plan = FFTWProxyType::Plan_dft_c2r_2d(outputSize[1],outputSize[0],
                                       this->m_InputBuffer,this->m_OutputBuffer,
                                       FFTW_ESTIMATE);
        break;
      case 3:
        this->m_Plan = FFTWProxyType::Plan_dft_c2r_3d(outputSize[2],outputSize[1],outputSize[0],
                                       this->m_InputBuffer,this->m_OutputBuffer,
                                       FFTW_ESTIMATE);
        break;
      default:
        int *sizes = new int[Dimension];
        for(unsigned int i = 0; i < Dimension; i++)
          {
          sizes[(Dimension - 1) - i] = outputSize[i];
          }
        this->m_Plan = FFTWProxyType::Plan_dft_c2r(Dimension,sizes,
                                    this->m_InputBuffer,
                                    this->m_OutputBuffer,FFTW_ESTIMATE);
        delete [] sizes;
      }
    this->m_PlanComputed = true;
    }
  // copy the input, because it may be destroyed by computing the plan
  memcpy(this->m_InputBuffer,
         inputPtr->GetBufferPointer(),
         total_inputSize * sizeof(typename FFTWProxyType::ComplexType));
  fftw::Proxy<TPixel>::Execute(this->m_Plan);
  // copy the output
  memcpy(outputPtr->GetBufferPointer(),
         this->m_OutputBuffer,
         total_outputSize * sizeof(TPixel));
  
  typedef ImageRegionIterator< TOutputImageType >   IteratorType;
  
  IteratorType it(outputPtr,outputPtr->GetLargestPossibleRegion());

  while( !it.IsAtEnd() )
    {
    it.Set( it.Value() / total_outputSize );
    ++it;
    }
}
template <typename TPixel,unsigned int Dimension>
bool
FFTWComplexConjugateToRealImageFilter<TPixel,Dimension>::
FullMatrix()
{
  return false;
}

}// namespace itk
#endif // defined(USE_FFTWF) || defined(USE_FFTWD)
#endif // _itkFFTWComplexConjugateToRealImageFilter_txx
