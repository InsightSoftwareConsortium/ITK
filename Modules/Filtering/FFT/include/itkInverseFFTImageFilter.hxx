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
#ifndef itkInverseFFTImageFilter_hxx
#define itkInverseFFTImageFilter_hxx
#include "itkMetaDataObject.h"

#include "itkVnlInverseFFTImageFilter.h"

#if defined( ITK_USE_FFTWD ) || defined( ITK_USE_FFTWF )
#include "itkFFTWInverseFFTImageFilter.h"
#endif

namespace itk
{

// Partial specialization allows avoiding runtime type choice
template< typename TSelfPointer, typename TInputImage, typename TOutputImage, typename TPixel >
struct Dispatch_Inverse_New
{
  static TSelfPointer Apply()
    {
      return VnlInverseFFTImageFilter< TInputImage, TOutputImage >
        ::New().GetPointer();
    }
};

#ifdef ITK_USE_FFTWD
template < typename TSelfPointer, typename TInputImage, typename TOutputImage >
struct Dispatch_Inverse_New< TSelfPointer, TInputImage, TOutputImage, double >
{
  static TSelfPointer Apply()
    {
      return FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
        ::New().GetPointer();
    }
};
#endif

#ifdef ITK_USE_FFTWF
template< typename TSelfPointer, typename TInputImage, typename TOutputImage >
struct Dispatch_Inverse_New< TSelfPointer, TInputImage, TOutputImage, float >
{
  static TSelfPointer Apply()
    {
      return FFTWInverseFFTImageFilter< TInputImage, TOutputImage >
        ::New().GetPointer();
    }
};
#endif

template< typename TInputImage, typename TOutputImage >
typename InverseFFTImageFilter< TInputImage, TOutputImage >::Pointer
InverseFFTImageFilter< TInputImage, TOutputImage >
::New(void)
{
  Pointer smartPtr = ::itk::ObjectFactory< Self >::Create();

  if ( smartPtr.IsNull() )
    {
    smartPtr = Dispatch_Inverse_New<Pointer, TInputImage, TOutputImage, OutputPixelType>::Apply();
    }

  return smartPtr;
}

template< typename TInputImage, typename TOutputImage >
void
InverseFFTImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  // get pointers to the input and output
  typename InputImageType::Pointer inputPtr  =
    const_cast< InputImageType * >( this->GetInput() );
  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
InverseFFTImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
SizeValueType
InverseFFTImageFilter< TInputImage, TOutputImage >
::GetSizeGreatestPrimeFactor() const
{
  return 2;
}

}
#endif
