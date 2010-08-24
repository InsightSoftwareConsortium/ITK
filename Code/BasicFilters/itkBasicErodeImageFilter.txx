/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicErodeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBasicErodeImageFilter_txx
#define __itkBasicErodeImageFilter_txx

#include "itkBasicErodeImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
BasicErodeImageFilter< TInputImage, TOutputImage, TKernel >
::BasicErodeImageFilter()
{
  m_ErodeBoundaryCondition.SetConstant( NumericTraits< PixelType >::max() );
  this->OverrideBoundaryCondition(&m_ErodeBoundaryCondition);
}

template< class TInputImage, class TOutputImage, class TKernel >
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
    if ( *kernel_it > NumericTraits< KernelPixelType >::Zero )
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
