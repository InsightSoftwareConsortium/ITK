/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleFunctionErodeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleFunctionErodeImageFilter_txx
#define __itkGrayscaleFunctionErodeImageFilter_txx

#include "itkGrayscaleFunctionErodeImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
GrayscaleFunctionErodeImageFilter< TInputImage, TOutputImage, TKernel >
::GrayscaleFunctionErodeImageFilter()
{
  m_ErodeBoundaryCondition.SetConstant( NumericTraits< PixelType >::max() );
  this->OverrideBoundaryCondition(&m_ErodeBoundaryCondition);
}

template< class TInputImage, class TOutputImage, class TKernel >
typename GrayscaleFunctionErodeImageFilter< TInputImage, TOutputImage, TKernel >::PixelType
GrayscaleFunctionErodeImageFilter< TInputImage, TOutputImage, TKernel >
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
    // in the image minus the structuring element value
    if ( *kernel_it > NumericTraits< KernelPixelType >::Zero )
      {
      // subtract the structuring element value to the pixel value,
      // note we use GetPixel() on SmartNeighborhoodIterator to respect
      // boundary condition
      temp = nit.GetPixel(i) - ( PixelType ) * kernel_it;

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
