/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleErodeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleErodeImageFilter_txx
#define __itkGrayscaleErodeImageFilter_txx

#include "itkGrayscaleErodeImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
typename GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::PixelType
GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(const NeighborhoodIteratorType &nit,
           const KernelType &kernel)
{
  unsigned int i;
  PixelType min = NumericTraits<PixelType>::max() ;
  PixelType temp;

  KernelIteratorType kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  for (i=0, kernel_it=kernel.Begin(); kernel_it<kernelEnd; ++kernel_it, ++i)
    {
    // if structuring element is positive, use the pixel under that element
    // in the image
    if (*kernel_it > 0)
      {
      // note we use GetPixel() on the NeighborhoodIterator in order
      // to respect boundary conditions.
      temp = nit.GetPixel(i);

      if (temp < min)
        min = temp ;
      }
    }
  
  return min ;
} 
  
}// end namespace itk
#endif
