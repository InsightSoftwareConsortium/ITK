/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSecondDerivativeRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSecondDerivativeRecursiveGaussianImageFilter_txx
#define _itkSecondDerivativeRecursiveGaussianImageFilter_txx

#include "itkSecondDerivativeRecursiveGaussianImageFilter.h"

namespace itk
{

/**
 * Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
SecondDerivativeRecursiveGaussianImageFilter<TInputImage,TOutputImage, TComputation>
::SetUp(void)
{
  m_A0 = TComputation( -1.3310 );
  m_A1 = TComputation(  3.6610 );
  m_B0 = TComputation(  1.2400 );
  m_B1 = TComputation(  1.3140 );
  m_C0 = TComputation(  0.3225 );
  m_C1 = TComputation( -1.7380 );
  m_W0 = TComputation(  0.7480 );
  m_W1 = TComputation(  2.1660 );
  
  const TComputation sigmad = GetSigma() / m_Spacing;
  m_K = 1.0 / ( sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0 ) ) ) );
  
  const bool symmetric = true;
  ComputeFilterCoefficients(symmetric); 
}

} // end namespace itk

#endif
