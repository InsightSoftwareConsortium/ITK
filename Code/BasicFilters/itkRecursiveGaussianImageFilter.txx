/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRecursiveGaussianImageFilter_txx
#define _itkRecursiveGaussianImageFilter_txx

#include "itkRecursiveGaussianImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include <new>


namespace itk
{
  
template <class TInputImage, class TOutputImage, class TComputation>
RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::RecursiveGaussianImageFilter()
{
  m_Sigma = 1.0;
  m_NormalizeAcrossScale = false;
}




/**
 *   Compute filter for Gaussian kernel
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::SetUp(void)
{
  m_A0 = TComputation(  1.680  );
  m_A1 = TComputation(  3.735  );
  m_B0 = TComputation(  1.783  );
  m_B1 = TComputation(  1.723  );
  m_C0 = TComputation( -0.6803 );
  m_C1 = TComputation( -0.2598 );
  m_W0 = TComputation(  0.6318 );
  m_W1 = TComputation(  1.9970 );
  
  if( m_Spacing < TComputation( 0.0001 ) ) return;
  
  const TComputation sigmad = m_Sigma/m_Spacing;

  if( this->GetNormalizeAcrossScale() )
    {
    m_K = 1.0 / (         sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
    }
  else
    {
    m_K = 1.0 / ( sigmad * sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
    }
  
  const bool symmetric = true;
  ComputeFilterCoefficients(symmetric);

}



/**
 * Compute Recursive Filter Coefficients 
 */
template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage, TComputation>
::ComputeFilterCoefficients(bool symmetric) 
{

  const TComputation sigmad = m_Sigma/m_Spacing;
  
  m_N00  = m_A0 + m_C0;
  m_N11  = exp(-m_B1/sigmad)*(m_C1*sin(m_W1/sigmad)-(m_C0+2*m_A0)*cos(m_W1/sigmad)); 
  m_N11 += exp(-m_B0/sigmad)*(m_A1*sin(m_W0/sigmad)-(m_A0+2*m_C0)*cos(m_W0/sigmad)); 
  m_N22  = ((m_A0+m_C0)*cos(m_W1/sigmad)*cos(m_W0/sigmad));
  m_N22 -= (m_A1*cos(m_W1/sigmad)*sin(m_W0/sigmad)+m_C1*cos(m_W0/sigmad)*sin(m_W1/sigmad));
  m_N22 *= 2*exp(-(m_B0+m_B1)/sigmad);
  m_N22 += m_C0*exp(-2*m_B0/sigmad) + m_A0*exp(-2*m_B1/sigmad);
  m_N33  = exp(-(m_B1+2*m_B0)/sigmad)*(m_C1*sin(m_W1/sigmad)-m_C0*cos(m_W1/sigmad));
  m_N33 += exp(-(m_B0+2*m_B1)/sigmad)*(m_A1*sin(m_W0/sigmad)-m_A0*cos(m_W0/sigmad));
  
  m_D44  = exp(-2*(m_B0+m_B1)/sigmad);
  m_D33  = -2*cos(m_W0/sigmad)*exp(-(m_B0+2*m_B1)/sigmad);
  m_D33 += -2*cos(m_W1/sigmad)*exp(-(m_B1+2*m_B0)/sigmad);
  m_D22  =  4*cos(m_W1/sigmad)*cos(m_W0/sigmad)*exp(-(m_B0+m_B1)/sigmad);
  m_D22 +=  exp(-2*m_B1/sigmad)+exp(-2*m_B0/sigmad);
  m_D11  =  -2*exp(-m_B1/sigmad)*cos(m_W1/sigmad)-2*exp(-m_B0/sigmad)*cos(m_W0/sigmad);
  
  if( symmetric )
    {
    m_M11 = m_N11 - m_D11 * m_N00;
    m_M22 = m_N22 - m_D22 * m_N00;
    m_M33 = m_N33 - m_D33 * m_N00;
    m_M44 =       - m_D44 * m_N00;
    }
  else
    {
    m_M11 = -( m_N11 - m_D11 * m_N00 );
    m_M22 = -( m_N22 - m_D22 * m_N00 );
    m_M33 = -( m_N33 - m_D33 * m_N00 );
    m_M44 =            m_D44 * m_N00;
    }

  // Compute Coefficients to be used at the boundaries
  // in order to prevent border effects
  const TComputation SumOfNCoefficients = m_N00 + m_N11 + m_N22 + m_N33;
  const TComputation SumOfMCoefficients = m_M11 + m_M22 + m_M33 + m_M44;
  const TComputation SumOfDCoefficients = m_D11 + m_D22 + m_D33 + m_D44;
  const TComputation CoefficientNormN    = SumOfNCoefficients / ( 1.0 + SumOfDCoefficients );
  const TComputation CoefficientNormM    = SumOfMCoefficients / ( 1.0 + SumOfDCoefficients );

  m_BN1 = m_D11 * CoefficientNormN;
  m_BN2 = m_D22 * CoefficientNormN;
  m_BN3 = m_D33 * CoefficientNormN;
  m_BN4 = m_D44 * CoefficientNormN;
  
  m_BM1 = m_D11 * CoefficientNormM;
  m_BM2 = m_D22 * CoefficientNormM;
  m_BM3 = m_D33 * CoefficientNormM;
  m_BM4 = m_D44 * CoefficientNormM;
  
}


template <class TInputImage, class TOutputImage, class TComputation>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "Sigma: " << m_Sigma << std::endl; 
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}

} // end namespace itk

#endif
