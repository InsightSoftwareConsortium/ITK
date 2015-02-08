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
#ifndef itkBasicErodeImageFilter_hxx
#define itkBasicErodeImageFilter_hxx

#include "itkBasicErodeImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
BasicErodeImageFilter< TInputImage, TOutputImage, TKernel >
::BasicErodeImageFilter()
{
  m_ErodeBoundaryCondition.SetConstant( NumericTraits< PixelType >::max() );
  this->OverrideBoundaryCondition(&m_ErodeBoundaryCondition);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
typename BasicErodeImageFilter< TInputImage, TOutputImage, TKernel >::PixelType
BasicErodeImageFilter< TInputImage, TOutputImage, TKernel >
::Evaluate(const NeighborhoodIteratorType & nit,
           const KernelIteratorType kernelBegin,
           const KernelIteratorType kernelEnd)
{
  unsigned int i;
  PixelType    min = NumericTraits< PixelType >::max();
  PixelType    temp;

  KernelIteratorType kernel_it;

  for ( i = 0, kernel_it = kernelBegin; kernel_it < kernelEnd; ++kernel_it, ++i )
    {
    // if structuring element is positive, use the pixel under that element
    // in the image
    if ( *kernel_it > NumericTraits< KernelPixelType >::ZeroValue() )
      {
      // note we use GetPixel() on the NeighborhoodIterator in order
      // to respect boundary conditions.
      temp = nit.GetPixel(i);

      if ( temp < min )
        {
        min = temp;
        }
      }
    }

  return min;
}
} // end namespace itk
#endif
