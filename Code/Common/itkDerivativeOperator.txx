/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDerivativeOperator_txx
#define _itkDerivativeOperator_txx
#include "itkDerivativeOperator.h"

#include "itkNumericTraits.h"

namespace itk
{

template <class TPixel, unsigned int VDimension, class TAllocator>
typename DerivativeOperator<TPixel, VDimension, TAllocator>
::CoefficientVector
DerivativeOperator<TPixel, VDimension, TAllocator>
::GenerateCoefficients()
{
  unsigned int i;
  unsigned int j;
  typedef typename NumericTraits<PixelType>::RealType PixelRealType;  
  PixelRealType previous;
  PixelRealType next;
  const unsigned int w = 2*((m_Order + 1)/2) + 1;
  CoefficientVector coeff(w);

  coeff[w/2] = 1.0;
  for (i = 0; i < m_Order/2; i++)
    {
    previous = coeff[1] - 2 * coeff[0];
    for (j = 1; j < w - 1; j++)
      {
      next =coeff[j - 1]  + coeff[j + 1] - 2*coeff[j];
      coeff[j-1] = previous;
      previous = next;
      }
    next = coeff[j - 1] - 2*coeff[j];
    coeff[j-1] = previous;
    coeff[j] = next;      
    }
  for (i = 0; i < m_Order%2; i++)    
    {
    previous =  0.5 * coeff[1];
    for (j = 1; j < w - 1; j++)
      {
      next = -0.5*coeff[j - 1] + 0.5*coeff[j + 1];
      coeff[j-1] = previous;
      previous = next;
      }
    next = -0.5 * coeff[j - 1];
    coeff[j-1] = previous;
    coeff[j] = next;      
    }

  return coeff;
}

} // namespace itk

#endif
