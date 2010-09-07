/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicDilateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBasicDilateImageFilter_txx
#define __itkBasicDilateImageFilter_txx

#include "itkBasicDilateImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
BasicDilateImageFilter< TInputImage, TOutputImage, TKernel >
::BasicDilateImageFilter()
{
  m_DilateBoundaryCondition.SetConstant( NumericTraits< PixelType >::NonpositiveMin() );
  this->OverrideBoundaryCondition(&m_DilateBoundaryCondition);
}

template< class TInputImage, class TOutputImage, class TKernel >
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
    if ( *kernel_it > NumericTraits< KernelPixelType >::Zero )
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
