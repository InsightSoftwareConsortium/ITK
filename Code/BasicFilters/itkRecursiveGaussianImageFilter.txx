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
  
template <typename TInputImage, typename TOutputImage>
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::RecursiveGaussianImageFilter()
{
  m_Sigma = 1.0;
  m_NormalizeAcrossScale = false;
  m_Order = ZeroOrder;
}




/**
 *   Compute filter for Gaussian kernel
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetUp(void)
{
 
  const RealType spacingTolerance = 1e-8;

  RealType direction = 1.0;
  if( m_Spacing < 0.0 )
    {
    direction = -1.0;
    m_Spacing = -m_Spacing;
    }

  if( m_Spacing < spacingTolerance )
    {
    itkExceptionMacro(<<"The spacing " << m_Spacing << "is suspiciosly small in this image");
    } 
  
  const RealType sigmad = m_Sigma/m_Spacing;

  if( this->GetNormalizeAcrossScale() )
    {
    m_K = 1.0 / (         sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
    }
  else
    {
    m_K = 1.0 / ( sigmad * sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
    }

  m_K *= direction;  // take into account the sign of the spacing.

  switch( m_Order ) 
    {
    case ZeroOrder: // equivalent to convolution with a gaussian
      {
      m_A0 = static_cast<RealType>(  1.680  );
      m_A1 = static_cast<RealType>(  3.735  );
      m_B0 = static_cast<RealType>(  1.783  );
      m_B1 = static_cast<RealType>(  1.723  );
      m_C0 = static_cast<RealType>( -0.6803 );
      m_C1 = static_cast<RealType>( -0.2598 );
      m_W0 = static_cast<RealType>(  0.6318 );
      m_W1 = static_cast<RealType>(  1.9970 );
      const bool symmetric = true;
      this->ComputeFilterCoefficients(symmetric);
      break;
      }
    case FirstOrder: // equivalent to convolution with 
                     // the first derivative of a gaussian
      {
      m_A0 = static_cast<RealType>(  -0.6472 );
      m_A1 = static_cast<RealType>(  -4.5310 );
      m_B0 = static_cast<RealType>(   1.5270 );
      m_B1 = static_cast<RealType>(   1.5160 );
      m_C0 = static_cast<RealType>(   0.6494 );
      m_C1 = static_cast<RealType>(   0.9557 );
      m_W0 = static_cast<RealType>(   0.6719 );
      m_W1 = static_cast<RealType>(   2.0720 );
      const bool symmetric = false;
      this->ComputeFilterCoefficients(symmetric);
      break;
      }
    case SecondOrder: // equivalent to convolution with 
                      // the second derivative of a gaussian
      {
      m_A0 = static_cast<RealType>(  -1.3310 );
      m_A1 = static_cast<RealType>(   3.6610 );
      m_B0 = static_cast<RealType>(   1.2400 );
      m_B1 = static_cast<RealType>(   1.3140 );
      m_C0 = static_cast<RealType>(   0.3225 );
      m_C1 = static_cast<RealType>(  -1.7380 );
      m_W0 = static_cast<RealType>(   0.7480 );
      m_W1 = static_cast<RealType>(   2.1660 );
      const bool symmetric = true;
      this->ComputeFilterCoefficients(symmetric);
      break;
      }
    default:
      {
      itkExceptionMacro(<<"Unknown Order");
      return;
      }
    }


}



/**
 * Compute Recursive Filter Coefficients 
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::ComputeFilterCoefficients(bool symmetric) 
{

  const RealType spacing = (m_Spacing>0.0) ? m_Spacing : -m_Spacing;

  const RealType sigmad = m_Sigma / spacing;
  
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
  const RealType SumOfNCoefficients = m_N00 + m_N11 + m_N22 + m_N33;
  const RealType SumOfMCoefficients = m_M11 + m_M22 + m_M33 + m_M44;
  const RealType SumOfDCoefficients = m_D11 + m_D22 + m_D33 + m_D44;
  const RealType CoefficientNormN    = SumOfNCoefficients / ( 1.0 + SumOfDCoefficients );
  const RealType CoefficientNormM    = SumOfMCoefficients / ( 1.0 + SumOfDCoefficients );

  m_BN1 = m_D11 * CoefficientNormN;
  m_BN2 = m_D22 * CoefficientNormN;
  m_BN3 = m_D33 * CoefficientNormN;
  m_BN4 = m_D44 * CoefficientNormN;
  
  m_BM1 = m_D11 * CoefficientNormM;
  m_BM2 = m_D22 * CoefficientNormM;
  m_BM3 = m_D33 * CoefficientNormM;
  m_BM4 = m_D44 * CoefficientNormM;
  
}


template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "Sigma: " << m_Sigma << std::endl; 
  os << "Order: " << m_Order << std::endl; 
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}

} // end namespace itk

#endif
