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
#ifndef itkBasicDilateImageFilter_hxx
#define itkBasicDilateImageFilter_hxx

#include "itkBasicDilateImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
BasicDilateImageFilter< TInputImage, TOutputImage, TKernel >
::BasicDilateImageFilter()
{
  m_DilateBoundaryCondition.SetConstant( NumericTraits< PixelType >::NonpositiveMin() );
  this->OverrideBoundaryCondition(&m_DilateBoundaryCondition);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
typename BasicDilateImageFilter< TInputImage, TOutputImage, TKernel >::PixelType
BasicDilateImageFilter< TInputImage, TOutputImage, TKernel >
::Evaluate(const NeighborhoodIteratorType & nit,
           const KernelIteratorType kernelBegin,
           const KernelIteratorType kernelEnd)
{
  unsigned int i;
  PixelType    max = NumericTraits< PixelType >::NonpositiveMin();
  PixelType    temp;

  KernelIteratorType kernel_it;

  for ( i = 0, kernel_it = kernelBegin; kernel_it < kernelEnd; ++kernel_it, ++i )
    {
    // if structuring element is positive, use the pixel under that element
    // in the image
    if ( *kernel_it > NumericTraits< KernelPixelType >::ZeroValue() )
      {
      // note we use GetPixel() on the SmartNeighborhoodIterator to
      // respect boundary conditions
      temp = nit.GetPixel(i);

      if ( temp > max )
        {
        max = temp;
        }
      }
    }

  return max;
}
} // end namespace itk
#endif
