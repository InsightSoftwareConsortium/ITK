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
  a0 = TComputation( -1.3310 );
  a1 = TComputation(  3.6610 );
  b0 = TComputation(  1.2400 );
  b1 = TComputation(  1.3140 );
  c0 = TComputation(  0.3225 );
  c1 = TComputation( -1.7380 );
  w0 = TComputation(  0.7480 );
  w1 = TComputation(  2.1660 );
  
  const TComputation sigmad = GetSigma() / m_Spacing;
//K = 1.0 / ( sigmad * sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
  K = 1.0 / ( sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0 ) ) ) );
  
  const bool symmetric = true;
  ComputeFilterCoefficients(symmetric); 
}

} // end namespace itk

#endif
