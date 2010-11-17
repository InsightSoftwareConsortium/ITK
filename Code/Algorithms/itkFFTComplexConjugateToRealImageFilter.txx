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
#ifndef __itkFFTComplexConjugateToRealImageFilter_txx
#define __itkFFTComplexConjugateToRealImageFilter_txx
#include "itkMetaDataObject.h"

#include "itkVnlFFTComplexConjugateToRealImageFilter.h"

#if defined( USE_FFTWD ) || defined( USE_FFTWF )
#include "itkFFTWComplexConjugateToRealImageFilter.h"
#endif

namespace itk
{
// Partial Specialization allows avoiding runtime type choice
template <typename TSelfPointer, unsigned int VDimension, typename TPixel>
struct Dispatch_C2R_New
{
  static TSelfPointer apply()
    {
      return VnlFFTComplexConjugateToRealImageFilter< TPixel, VDimension >
        ::New().GetPointer();
    }
};

#ifdef USE_FFTWD
template <typename TSelfPointer, unsigned int VDimension>
struct Dispatch_C2R_New<TSelfPointer, VDimension, double>
{
  static TSelfPointer apply()
    {
      return FFTWComplexConjugateToRealImageFilter< double, VDimension >
        ::New().GePointer();
    }
};
#endif

#ifdef USE_FFTWF
template <typename TSelfPointer, unsigned int VDimension>
struct Dispatch_C2R_New<TSelfPointer, VDimension, float>
{
  static TSelfPointer apply()
    {
      return FFTWComplexConjugateToRealImageFilter< float, VDimension >
        ::New().GetPointer();
    }
};
#endif

template< class TPixel, unsigned int VDimension >
typename FFTComplexConjugateToRealImageFilter< TPixel, VDimension >::Pointer
FFTComplexConjugateToRealImageFilter< TPixel, VDimension >
::New(void)
{
  Pointer smartPtr = ::itk::ObjectFactory< Self >::Create();

  if ( smartPtr.IsNull() )
    {
    smartPtr = Dispatch_C2R_New<Self*, VDimension, TPixel>::apply();
    }

  return smartPtr;
}

template< class TPixel, unsigned int VDimension >
void
FFTComplexConjugateToRealImageFilter< TPixel, VDimension >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
  //
  // If this implementation returns a full result
  // instead of a 'half-complex' matrix, then none of this
  // is necessary
  if ( this->FullMatrix() )
    {
    return;
    }

  // get pointers to the input and output
  typename TInputImageType::ConstPointer inputPtr  = this->GetInput();
  typename TOutputImageType::Pointer outputPtr = this->GetOutput();

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
  const typename TInputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImageType::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TOutputImageType::SizeType outputSize;
  typename TOutputImageType::IndexType outputStartIndex;

  //
  // in 4.3.4 of the FFT documentation, they indicate the size of
  // of a real-to-complex FFT is N * N ... + (N /2+1)
  //                              1   2        d
  // complex numbers.
  // going from complex to real, you know the output is at least
  // twice the size in the last dimension as the input, but it might
  // be 2*size+1.  Consequently, the output of the FFT:R2C operation
  //
  MetaDataDictionary & InputDic =
    const_cast< MetaDataDictionary & >( inputPtr->GetMetaDataDictionary() );

  typedef typename TInputImageType::SizeType::SizeValueType SizeScalarType;

  SizeScalarType x = 0;

  outputSize[0] = ( inputSize[0] - 1 ) * 2;
  if ( this->ActualXDimensionIsOdd() )
    {
    outputSize[0]++;
    }
  // backwards compatible/deprecated version
  if ( ExposeMetaData< SizeScalarType >
         (InputDic, std::string("FFT_Actual_RealImage_Size"), x) )
    {
    outputSize[0] = x;
    }

  outputStartIndex[0] = inputStartIndex[0];

  for ( i = 1; i < TOutputImageType::ImageDimension; i++ )
    {
    outputSize[i] = inputSize[i];
    outputStartIndex[i] = inputStartIndex[i];
    }
  typename TOutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template< class TPixel, unsigned int VDimension >
void
FFTComplexConjugateToRealImageFilter< TPixel, VDimension >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  // get pointers to the input and output
  typename TInputImageType::Pointer inputPtr  =
    const_cast< TInputImageType * >( this->GetInput() );
  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< class TPixel, unsigned int VDimension >
void
FFTComplexConjugateToRealImageFilter< TPixel, VDimension >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}
}
#endif
