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
/**
 *
 * Attribution Notice. This research work was made possible by
 * Grant Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * This class was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/326
 *
 */
#ifndef __itkFFTComplexToComplexImageFilter_hxx
#define __itkFFTComplexToComplexImageFilter_hxx

#include "itkFFTComplexToComplexImageFilter.h"

#if defined( ITK_USE_FFTWD ) || defined( ITK_USE_FFTWF )
#include "itkFFTWComplexToComplexImageFilter.h"
#endif

namespace itk
{
#if defined( ITK_USE_FFTWD ) || defined( ITK_USE_FFTWF )
template< typename TImage >
class FFTWComplexToComplexImageFilter;
#endif

template< typename TImage >
typename FFTComplexToComplexImageFilter< TImage >::Pointer
FFTComplexToComplexImageFilter< TImage >
::New(void)
{
  Pointer smartPtr = ::itk::ObjectFactory< Self >::Create();

#ifdef ITK_USE_FFTWD
  if ( smartPtr.IsNull() )
    {
    if ( typeid( typename ImageType::PixelType::value_type ) == typeid( double ) )
      {
      smartPtr = dynamic_cast< Self * >(
        FFTWComplexToComplexImageFilter< TImage >::New().GetPointer() );
      }
    }
#endif
#ifdef ITK_USE_FFTWF
  if ( smartPtr.IsNull() )
    {
    if ( typeid( typename ImageType::PixelType::value_type ) == typeid( float ) )
      {
      smartPtr = dynamic_cast< Self * >(
        FFTWComplexToComplexImageFilter< TImage >::New().GetPointer() );
      }
    }
#endif

  return smartPtr;
}

template< typename TImage >
void
FFTComplexToComplexImageFilter< TImage >::GenerateOutputInformation()
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
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

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
  const typename InputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename OutputImageType::SizeType outputSize;
  typename OutputImageType::IndexType outputStartIndex;

  //
  // Size of output FFT:C2C is the same as input
  //

  outputSize[0] = inputSize[0];
  outputStartIndex[0] = inputStartIndex[0];

  for ( i = 1; i < OutputImageType::ImageDimension; i++ )
    {
    outputSize[i] = inputSize[i];
    outputStartIndex[i] = inputStartIndex[i];
    }
  typename OutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template< typename TImage >
void
FFTComplexToComplexImageFilter< TImage >::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  // get pointers to the input and output
  typename InputImageType::Pointer inputPtr  =
    const_cast< InputImageType * >( this->GetInput() );
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
}
}
#endif
