/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 *   Explicitly set a zeroth order derivative
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetZeroOrder()
{
  this->SetOrder( ZeroOrder );
}

/**
 *   Explicitly set a first order derivative
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetFirstOrder()
{
  this->SetOrder( FirstOrder );
}

/**
 *   Explicitly set a second order derivative
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetSecondOrder()
{
  this->SetOrder( SecondOrder );
}


/**
 *   Compute filter for Gaussian kernel
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveGaussianImageFilter<TInputImage,TOutputImage>
::SetUp(RealType spacing)
{
 
  const RealType spacingTolerance = 1e-8;

  RealType direction = 1.0;
  if( spacing < 0.0 )
    {
    direction = -1.0;
    spacing = -spacing;
    }

  if( spacing < spacingTolerance )
    {
    itkExceptionMacro(<<"The spacing " << spacing << "is suspiciosly small in this image");
    } 
  
  const RealType sigmad = m_Sigma/spacing;

  if( this->GetNormalizeAcrossScale() )
    {
    this->m_K = 1.0 / (          sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
    }
  else
    {
    this->m_K = 1.0 / ( sigmad * sqrt( 2.0 * ( 4.0 * atan( 1.0f ) ) ) );
    }

  this->m_K *= direction;  // take into account the sign of the spacing.

  switch( this->m_Order ) 
    {
    case ZeroOrder: // equivalent to convolution with a gaussian
      {
      this->m_A0 = static_cast<RealType>(  1.680  );
      this->m_A1 = static_cast<RealType>(  3.735  );
      this->m_B0 = static_cast<RealType>(  1.783  );
      this->m_B1 = static_cast<RealType>(  1.723  );
      this->m_C0 = static_cast<RealType>( -0.6803 );
      this->m_C1 = static_cast<RealType>( -0.2598 );
      this->m_W0 = static_cast<RealType>(  0.6318 );
      this->m_W1 = static_cast<RealType>(  1.9970 );
      const bool symmetric = true;
      this->ComputeFilterCoefficients(symmetric, spacing);
      break;
      }
    case FirstOrder: // equivalent to convolution with 
                     // the first derivative of a gaussian
      {
      this->m_A0 = static_cast<RealType>(  -0.6472 );
      this->m_A1 = static_cast<RealType>(  -4.5310 );
      this->m_B0 = static_cast<RealType>(   1.5270 );
      this->m_B1 = static_cast<RealType>(   1.5160 );
      this->m_C0 = static_cast<RealType>(   0.6494 );
      this->m_C1 = static_cast<RealType>(   0.9557 );
      this->m_W0 = static_cast<RealType>(   0.6719 );
      this->m_W1 = static_cast<RealType>(   2.0720 );
      this->m_K /= sigmad;
      const bool symmetric = false;
      this->ComputeFilterCoefficients(symmetric, spacing);
      break;
      }
    case SecondOrder: // equivalent to convolution with 
                      // the second derivative of a gaussian
      {
      this->m_A0 = static_cast<RealType>(  -1.3310 );
      this->m_A1 = static_cast<RealType>(   3.6610 );
      this->m_B0 = static_cast<RealType>(   1.2400 );
      this->m_B1 = static_cast<RealType>(   1.3140 );
      this->m_C0 = static_cast<RealType>(   0.3225 );
      this->m_C1 = static_cast<RealType>(  -1.7380 );
      this->m_W0 = static_cast<RealType>(   0.7480 );
      this->m_W1 = static_cast<RealType>(   2.1660 );
      this->m_K /= sigmad * sigmad;
      const bool symmetric = true;
      this->ComputeFilterCoefficients(symmetric, spacing);
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
::ComputeFilterCoefficients(bool symmetric, RealType s) 
{

  const RealType spacing = (s>0.0) ? s : -s;

  const RealType sigmad = m_Sigma / spacing;
  
  this->m_N00  = this->m_A0 + this->m_C0;
  this->m_N11  = exp(-this->m_B1/sigmad)*(this->m_C1*sin(this->m_W1/sigmad)-(this->m_C0+2*this->m_A0)*cos(this->m_W1/sigmad)); 
  this->m_N11 += exp(-this->m_B0/sigmad)*(this->m_A1*sin(this->m_W0/sigmad)-(this->m_A0+2*this->m_C0)*cos(this->m_W0/sigmad)); 
  this->m_N22  = ((this->m_A0+this->m_C0)*cos(this->m_W1/sigmad)*cos(this->m_W0/sigmad));
  this->m_N22 -= (this->m_A1*cos(this->m_W1/sigmad)*sin(this->m_W0/sigmad)+this->m_C1*cos(this->m_W0/sigmad)*sin(this->m_W1/sigmad));
  this->m_N22 *= 2*exp(-(this->m_B0+this->m_B1)/sigmad);
  this->m_N22 += this->m_C0*exp(-2*this->m_B0/sigmad) + this->m_A0*exp(-2*this->m_B1/sigmad);
  this->m_N33  = exp(-(this->m_B1+2*this->m_B0)/sigmad)*(this->m_C1*sin(this->m_W1/sigmad)-this->m_C0*cos(this->m_W1/sigmad));
  this->m_N33 += exp(-(this->m_B0+2*this->m_B1)/sigmad)*(this->m_A1*sin(this->m_W0/sigmad)-this->m_A0*cos(this->m_W0/sigmad));
  
  this->m_D44  = exp(-2*(this->m_B0+this->m_B1)/sigmad);
  this->m_D33  = -2*cos(this->m_W0/sigmad)*exp(-(this->m_B0+2*this->m_B1)/sigmad);
  this->m_D33 += -2*cos(this->m_W1/sigmad)*exp(-(this->m_B1+2*this->m_B0)/sigmad);
  this->m_D22  =  4*cos(this->m_W1/sigmad)*cos(this->m_W0/sigmad)*exp(-(this->m_B0+this->m_B1)/sigmad);
  this->m_D22 +=  exp(-2*this->m_B1/sigmad)+exp(-2*this->m_B0/sigmad);
  this->m_D11  =  -2*exp(-this->m_B1/sigmad)*cos(this->m_W1/sigmad)-2*exp(-this->m_B0/sigmad)*cos(this->m_W0/sigmad);
  
  if( symmetric )
    {
    this->m_M11 = this->m_N11 - this->m_D11 * this->m_N00;
    this->m_M22 = this->m_N22 - this->m_D22 * this->m_N00;
    this->m_M33 = this->m_N33 - this->m_D33 * this->m_N00;
    this->m_M44 =       - this->m_D44 * this->m_N00;
    }
  else
    {
    this->m_M11 = -( this->m_N11 - this->m_D11 * this->m_N00 );
    this->m_M22 = -( this->m_N22 - this->m_D22 * this->m_N00 );
    this->m_M33 = -( this->m_N33 - this->m_D33 * this->m_N00 );
    this->m_M44 =            this->m_D44 * this->m_N00;
    }

  // Compute Coefficients to be used at the boundaries
  // in order to prevent border effects
  const RealType SumOfNCoefficients = this->m_N00 + this->m_N11 + this->m_N22 + this->m_N33;
  const RealType SumOfMCoefficients = this->m_M11 + this->m_M22 + this->m_M33 + this->m_M44;
  const RealType SumOfDCoefficients = this->m_D11 + this->m_D22 + this->m_D33 + this->m_D44;
  const RealType CoefficientNormN    = SumOfNCoefficients / ( 1.0 + SumOfDCoefficients );
  const RealType CoefficientNormM    = SumOfMCoefficients / ( 1.0 + SumOfDCoefficients );

  this->m_BN1 = this->m_D11 * CoefficientNormN;
  this->m_BN2 = this->m_D22 * CoefficientNormN;
  this->m_BN3 = this->m_D33 * CoefficientNormN;
  this->m_BN4 = this->m_D44 * CoefficientNormN;
  
  this->m_BM1 = this->m_D11 * CoefficientNormM;
  this->m_BM2 = this->m_D22 * CoefficientNormM;
  this->m_BM3 = this->m_D33 * CoefficientNormM;
  this->m_BM4 = this->m_D44 * CoefficientNormM;
  
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

