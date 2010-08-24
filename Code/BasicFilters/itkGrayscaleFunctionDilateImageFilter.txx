/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleFunctionDilateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleFunctionDilateImageFilter_txx
#define __itkGrayscaleFunctionDilateImageFilter_txx

#include "itkGrayscaleFunctionDilateImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
GrayscaleFunctionDilateImageFilter< TInputImage, TOutputImage, TKernel >
::GrayscaleFunctionDilateImageFilter()
{
  m_DilateBoundaryCondition.SetConstant( NumericTraits< PixelType >::NonpositiveMin() );
  this->OverrideBoundaryCondition(&m_DilateBoundaryCondition);
}

template< class TInputImage, class TOutputImage, class TKernel >
typename GrayscaleFunctionDilateImageFilter< TInputImage, TOutputImage, TKernel >::PixelType
GrayscaleFunctionDilateImageFilter< TInputImage, TOutputImage, TKernel >
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
    // in the image plus the structuring element value
    if ( *kernel_it > NumericTraits< KernelPixelType >::Zero )
      {
      // add the structuring element value to the pixel value, note we use
      // GetPixel() on SmartNeighborhoodIterator to respect boundary
      // conditions
      temp = nit.GetPixel(i) + ( PixelType ) * kernel_it;

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
