/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFirstDerivativeRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFirstDerivativeRecursiveGaussianImageFilter_txx
#define _itkFirstDerivativeRecursiveGaussianImageFilter_txx

#include "itkFirstDerivativeRecursiveGaussianImageFilter.h"

namespace itk
{

/**
 * Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
FirstDerivativeRecursiveGaussianImageFilter<TInputImage,TOutputImage, TComputation>
::SetUp(void)
{
  m_A0 = TComputation( -0.6472 );
  m_A1 = TComputation( -4.5310 );
  m_B0 = TComputation(  1.5270 );
  m_B1 = TComputation(  1.5160 );
  m_C0 = TComputation(  0.6494 );
  m_C1 = TComputation(  0.9557 );
  m_W0 = TComputation(  0.6719 );
  m_W1 = TComputation(  2.0720 );

  const TComputation sigmad = GetSigma() / m_Spacing;
  m_K = 1.0 / ( sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0 ) ) ) );

  const bool symmetric = false;
  ComputeFilterCoefficients(symmetric);
}

} // end namespace itk

#endif
