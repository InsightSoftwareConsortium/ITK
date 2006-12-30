/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTComplexConjugateToRealImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.


=========================================================================*/
#ifndef __itkFFTComplexConjugateToRealImageFilter_txx
#define __itkFFTComplexConjugateToRealImageFilter_txx
#include "itkFFTComplexConjugateToRealImageFilter.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"

#include "itkVnlFFTComplexConjugateToRealImageFilter.h"

#if defined(USE_FFTWD) || defined(USE_FFTWF)
#include "itkFFTWComplexConjugateToRealImageFilter.h"
#endif

namespace itk
{

template < class TPixel , unsigned int Dimension >
typename FFTComplexConjugateToRealImageFilter < TPixel , Dimension >::Pointer
FFTComplexConjugateToRealImageFilter < TPixel , Dimension >
::New(void)
{ 
  Pointer smartPtr = ::itk::ObjectFactory<Self>::Create();

#ifdef USE_FFTWD
  if(smartPtr.IsNull())
    {
    if (typeid(TPixel) == typeid(double))
      {
      smartPtr = dynamic_cast<Self *>(
        FFTWComplexConjugateToRealImageFilter< double, Dimension >
          ::New().GetPointer() );
      }
    }
#endif
#ifdef USE_FFTWF
  if(smartPtr.IsNull())
    {
    if (typeid(TPixel) == typeid(float))
      {
      smartPtr = dynamic_cast<Self *>(
        FFTWComplexConjugateToRealImageFilter< float, Dimension >
          ::New().GetPointer());
      }
    }
#endif

  if(smartPtr.IsNull())
    {
    smartPtr = VnlFFTComplexConjugateToRealImageFilter< TPixel, Dimension >
                  ::New().GetPointer();
    }

  return smartPtr;
}

template <class TPixel, unsigned int Dimension>
void
FFTComplexConjugateToRealImageFilter<TPixel,Dimension>::
GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
   //
  // If this implementation returns a full result
  // instead of a 'half-complex' matrix, then none of this
  // is necessary
  if(this->FullMatrix())
    return;
 
  // get pointers to the input and output
  typename TInputImageType::ConstPointer  inputPtr  = this->GetInput();
  typename TOutputImageType::Pointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // 
  // This is all based on the same function in itk::ShrinkImageFilter
  // ShrinkImageFilter also modifies the image spacing, but spacing
  // has no meaning in the result of an FFT. For an IFFT, since the
  // spacing is propagated to the complex result, we can use the spacing
  // from the input to propagate back to the output.
  unsigned int i;
  const typename TInputImageType::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImageType::IndexType&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();
  
  typename TOutputImageType::SizeType     outputSize;
  typename TOutputImageType::IndexType    outputStartIndex;
  
  //
  // in 4.3.4 of the FFT documentation, they indicate the size of
  // of a real-to-complex FFT is N * N ... + (N /2+1)
  //                              1   2        d
  // complex numbers.
  // going from complex to real, you know the output is at least
  // twice the size in the last dimension as the input, but it might
  // be 2*size+1.  Consequently, the output of the FFT:R2C operation
  // 
  MetaDataDictionary &InputDic = 
    const_cast<MetaDataDictionary &>(inputPtr->GetMetaDataDictionary());

  typedef typename TInputImageType::SizeType::SizeValueType SizeScalarType;

  SizeScalarType x;

  outputSize[0] = (inputSize[0] - 1) * 2;
  if(this->ActualXDimensionIsOdd())
    {
    outputSize[0]++;
    }
  // backwards compatible/deprecated version
  if(ExposeMetaData < SizeScalarType > 
          (InputDic,std::string("FFT_Actual_RealImage_Size"),x))
    {
    outputSize[0] = x;
    }

  outputStartIndex[0] = inputStartIndex[0];

  for (i = 1; i < TOutputImageType::ImageDimension; i++)
    {
    outputSize[i] = inputSize[i];
    outputStartIndex[i] = inputStartIndex[i];
    }
  typename TOutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );
  
  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

template <class TPixel, unsigned int Dimension>
void
FFTComplexConjugateToRealImageFilter<TPixel,Dimension>::
GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  // get pointers to the input and output
  typename TInputImageType::Pointer  inputPtr  = 
    const_cast<TInputImageType *>(this->GetInput());
  if( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

}
#endif
