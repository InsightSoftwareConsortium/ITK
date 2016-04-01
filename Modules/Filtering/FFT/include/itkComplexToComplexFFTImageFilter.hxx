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
 * https://hdl.handle.net/1926/326
 *
 */
#ifndef itkComplexToComplexFFTImageFilter_hxx
#define itkComplexToComplexFFTImageFilter_hxx
#include "itkMetaDataObject.h"

#include "itkVnlComplexToComplexFFTImageFilter.h"

#if defined( ITK_USE_FFTWD ) || defined( ITK_USE_FFTWF )
#include "itkFFTWComplexToComplexFFTImageFilter.h"
#endif

namespace itk
{

template< typename TSelfPointer, typename TImage, typename TPixel >
struct DispatchFFTW_Complex_New
{
  static TSelfPointer Apply()
    {
      return VnlComplexToComplexFFTImageFilter< TImage >
        ::New().GetPointer();
    }
};

#ifdef ITK_USE_FFTWD
template< typename TSelfPointer, typename TImage >
struct DispatchFFTW_Complex_New< TSelfPointer, TImage, double >
{
  static TSelfPointer Apply()
    {
      return FFTWComplexToComplexFFTImageFilter< TImage >
        ::New().GetPointer();
    }
};
#endif

#ifdef ITK_USE_FFTWF
template< typename TSelfPointer, typename TImage >
struct DispatchFFTW_Complex_New< TSelfPointer, TImage, float >
{
  static TSelfPointer Apply()
    {
      return FFTWComplexToComplexFFTImageFilter< TImage >
        ::New().GetPointer();
    }
};
#endif

template< typename TImage >
typename ComplexToComplexFFTImageFilter< TImage >::Pointer
ComplexToComplexFFTImageFilter< TImage >
::New()
{
  Pointer smartPtr = ObjectFactory< Self >::Create();

  if ( smartPtr.IsNull() )
    {
    smartPtr = DispatchFFTW_Complex_New< Pointer, TImage,
                                         typename NumericTraits< typename TImage::PixelType >::ValueType >
      ::Apply();
    }

  return smartPtr;
}


template< typename TImage >
void
ComplexToComplexFFTImageFilter< TImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  // get pointers to the input and output
  typename InputImageType::Pointer input  =
    const_cast< InputImageType * >( this->GetInput() );
  input->SetRequestedRegionToLargestPossibleRegion();
}

} // end namespace itk

#endif
