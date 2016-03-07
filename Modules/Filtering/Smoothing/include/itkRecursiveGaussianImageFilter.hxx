/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRecursiveGaussianImageFilter_hxx
#define itkRecursiveGaussianImageFilter_hxx

#include "itkRecursiveGaussianImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include <new>

namespace itk
{
template< typename TInputImage, typename TOutputImage >
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::RecursiveGaussianImageFilter()
{
  m_Sigma = 1.0;
  m_NormalizeAcrossScale = false;
  m_Order = ZeroOrder;
}

/**
 *   Explicitly set a zeroth order derivative.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetZeroOrder()
{
  this->SetOrder(ZeroOrder);
}

/**
 *   Explicitly set a first order derivative.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetFirstOrder()
{
  this->SetOrder(FirstOrder);
}

/**
 *   Explicitly set a second order derivative.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetSecondOrder()
{
  this->SetOrder(SecondOrder);
}

/**
 *   Compute filter for Gaussian kernel.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::SetUp(ScalarRealType spacing)
{
  const ScalarRealType spacingTolerance = 1e-8;

  /**  Parameters of exponential series. */
  ScalarRealType A1[3];
  ScalarRealType B1[3];
  ScalarRealType W1;
  ScalarRealType L1;
  ScalarRealType A2[3];
  ScalarRealType B2[3];
  ScalarRealType W2;
  ScalarRealType L2;

  ScalarRealType direction = 1.0;

  if ( spacing < 0.0 )
    {
    direction = -1.0;
    spacing = -spacing;
    }

  if ( spacing < spacingTolerance )
    {
    itkExceptionMacro(<< "The spacing " << spacing << "is suspiciosly small in this image");
    }

  const ScalarRealType sigmad = m_Sigma / spacing;
  ScalarRealType       across_scale_normalization = 1.0;


  A1[0] = static_cast< ScalarRealType >(  1.3530 );
  B1[0] = static_cast< ScalarRealType >(  1.8151 );
  W1    = static_cast< ScalarRealType >(  0.6681 );
  L1    = static_cast< ScalarRealType >( -1.3932 );
  A2[0] = static_cast< ScalarRealType >( -0.3531 );
  B2[0] = static_cast< ScalarRealType >(  0.0902 );
  W2    = static_cast< ScalarRealType >(  2.0787 );
  L2    = static_cast< ScalarRealType >( -1.3732 );

  A1[1] = static_cast< ScalarRealType >( -0.6724 );
  B1[1] = static_cast< ScalarRealType >( -3.4327 );
  A2[1] = static_cast< ScalarRealType >(  0.6724 );
  B2[1] = static_cast< ScalarRealType >(  0.6100 );

  A1[2] = static_cast< ScalarRealType >( -1.3563 );
  B1[2] = static_cast< ScalarRealType >(  5.2318 );
  A2[2] = static_cast< ScalarRealType >(  0.3446 );
  B2[2] = static_cast< ScalarRealType >( -2.2355 );

  ScalarRealType SD, DD, ED;
  this->ComputeDCoefficients(sigmad, W1, L1, W2, L2, SD, DD, ED);
  ScalarRealType SN, DN, EN;

  switch ( m_Order )
    {
    case ZeroOrder:
      {
      // Approximation of convolution with a gaussian.
      ComputeNCoefficients(sigmad,
                           A1[0], B1[0], W1, L1,
                           A2[0], B2[0], W2, L2,
                           this->m_N0,
                           this->m_N1,
                           this->m_N2,
                           this->m_N3,
                           SN, DN, EN);

      ScalarRealType alpha0 = 2 * SN / SD - this->m_N0;
      this->m_N0 *= across_scale_normalization / alpha0;
      this->m_N1 *= across_scale_normalization / alpha0;
      this->m_N2 *= across_scale_normalization / alpha0;
      this->m_N3 *= across_scale_normalization / alpha0;
      const bool symmetric = true;
      this->ComputeRemainingCoefficients(symmetric);
      break;
      }
    case FirstOrder:
      {
      if ( this->GetNormalizeAcrossScale() )
        {
        across_scale_normalization =  m_Sigma;
        }
      // Approximation of convolution with the first derivative of a  Gaussian
      ComputeNCoefficients(sigmad,
                           A1[1], B1[1], W1, L1,
                           A2[1], B2[1], W2, L2,
                           this->m_N0, this->m_N1, this->m_N2, this->m_N3,
                           SN, DN, EN);

      ScalarRealType alpha1 = 2 * ( SN * DD - DN * SD ) / ( SD * SD );
      // If negative spacing, negate the first derivative response.
      alpha1 *= direction;

      this->m_N0 *= across_scale_normalization / alpha1;
      this->m_N1 *= across_scale_normalization / alpha1;
      this->m_N2 *= across_scale_normalization / alpha1;
      this->m_N3 *= across_scale_normalization / alpha1;

      const bool symmetric = false;
      this->ComputeRemainingCoefficients(symmetric);
      break;
      }
    case SecondOrder:
      {
       if ( this->GetNormalizeAcrossScale() )
         {
         across_scale_normalization = itk::Math::sqr(  m_Sigma );
         }
      // Approximation of convolution with the second derivative of a
      // Gaussian.
      ScalarRealType N0_0, N1_0, N2_0, N3_0;
      ScalarRealType N0_2, N1_2, N2_2, N3_2;
      ScalarRealType SN0, DN0, EN0;
      ScalarRealType SN2, DN2, EN2;
      ComputeNCoefficients(sigmad,
                           A1[0], B1[0], W1, L1,
                           A2[0], B2[0], W2, L2,
                           N0_0, N1_0, N2_0, N3_0,
                           SN0, DN0, EN0);
      ComputeNCoefficients(sigmad,
                           A1[2], B1[2], W1, L1,
                           A2[2], B2[2], W2, L2,
                           N0_2, N1_2, N2_2, N3_2,
                           SN2, DN2, EN2);

      ScalarRealType beta = -( 2 * SN2 - SD * N0_2 ) / ( 2 * SN0 - SD * N0_0 );
      this->m_N0 = N0_2 + beta * N0_0;
      this->m_N1 = N1_2 + beta * N1_0;
      this->m_N2 = N2_2 + beta * N2_0;
      this->m_N3 = N3_2 + beta * N3_0;
      SN = SN2 + beta * SN0;
      DN = DN2 + beta * DN0;
      EN = EN2 + beta * EN0;

      ScalarRealType alpha2;
      alpha2  = EN * SD * SD - ED * SN * SD - 2 * DN * DD * SD + 2 * DD * DD * SN;
      alpha2 /= SD * SD * SD;

      this->m_N0 *= across_scale_normalization / alpha2;
      this->m_N1 *= across_scale_normalization / alpha2;
      this->m_N2 *= across_scale_normalization / alpha2;
      this->m_N3 *= across_scale_normalization / alpha2;

      const bool symmetric = true;
      this->ComputeRemainingCoefficients(symmetric);
      break;
      }
    default:
      {
      itkExceptionMacro(<< "Unknown Order");
      return;
      }
    }
}

/**
 * Compute the N coefficients.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::ComputeNCoefficients(ScalarRealType sigmad,
                       ScalarRealType A1, ScalarRealType B1, ScalarRealType W1, ScalarRealType L1,
                       ScalarRealType A2, ScalarRealType B2, ScalarRealType W2, ScalarRealType L2,
                       ScalarRealType & N0, ScalarRealType & N1, ScalarRealType & N2, ScalarRealType & N3,
                       ScalarRealType & SN, ScalarRealType & DN, ScalarRealType & EN)
{
  ScalarRealType Sin1 = std::sin(W1 / sigmad);
  ScalarRealType Sin2 = std::sin(W2 / sigmad);
  ScalarRealType Cos1 = std::cos(W1 / sigmad);
  ScalarRealType Cos2 = std::cos(W2 / sigmad);
  ScalarRealType Exp1 = std::exp(L1 / sigmad);
  ScalarRealType Exp2 = std::exp(L2 / sigmad);

  N0  = A1 + A2;
  N1  = Exp2 * ( B2 * Sin2 - ( A2 + 2 * A1 ) * Cos2 );
  N1 += Exp1 * ( B1 * Sin1 - ( A1 + 2 * A2 ) * Cos1 );
  N2  = ( A1 + A2 ) * Cos2 * Cos1;
  N2 -= B1 * Cos2 * Sin1 + B2 * Cos1 * Sin2;
  N2 *= 2 * Exp1 * Exp2;
  N2 += A2 * Exp1 * Exp1 + A1 * Exp2 * Exp2;
  N3  = Exp2 * Exp1 * Exp1 * ( B2 * Sin2 - A2 * Cos2 );
  N3 += Exp1 * Exp2 * Exp2 * ( B1 * Sin1 - A1 * Cos1 );

  SN = N0 + N1 + N2 + N3;
  DN = N1 + 2 * N2 + 3 * N3;
  EN = N1 + 4 * N2 + 9 * N3;
}

/**
 * Compute the D coefficients.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::ComputeDCoefficients(ScalarRealType sigmad,
                       ScalarRealType W1, ScalarRealType L1, ScalarRealType W2, ScalarRealType L2,
                       ScalarRealType & SD, ScalarRealType & DD, ScalarRealType & ED)
{
  //  const ScalarRealType Sin1 = std::sin(W1 / sigmad);
  //  const ScalarRealType Sin2 = std::sin(W2 / sigmad);
  const ScalarRealType Cos1 = std::cos(W1 / sigmad);
  const ScalarRealType Cos2 = std::cos(W2 / sigmad);
  const ScalarRealType Exp1 = std::exp(L1 / sigmad);
  const ScalarRealType Exp2 = std::exp(L2 / sigmad);

  this->m_D4 = Exp1 * Exp1 * Exp2 * Exp2;
  this->m_D3 = -2 * Cos1 * Exp1 * Exp2 * Exp2;
  this->m_D3 += -2 * Cos2 * Exp2 * Exp1 * Exp1;
  this->m_D2 = 4 * Cos2 * Cos1 * Exp1 * Exp2;
  this->m_D2 += Exp1 * Exp1 + Exp2 * Exp2;
  this->m_D1 = -2 * ( Exp2 * Cos2 + Exp1 * Cos1 );

  SD = 1.0 + this->m_D1 + this->m_D2 + this->m_D3 + this->m_D4;
  DD = this->m_D1 + 2 * this->m_D2 + 3 * this->m_D3 + 4 * this->m_D4;
  ED = this->m_D1 + 4 * this->m_D2 + 9 * this->m_D3 + 16 * this->m_D4;
}

/**
 * Compute the M coefficients and the boundary coefficients.
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::ComputeRemainingCoefficients(bool symmetric)
{
  if ( symmetric )
    {
    this->m_M1 = this->m_N1 - this->m_D1 * this->m_N0;
    this->m_M2 = this->m_N2 - this->m_D2 * this->m_N0;
    this->m_M3 = this->m_N3 - this->m_D3 * this->m_N0;
    this->m_M4 =            -this->m_D4 * this->m_N0;
    }
  else
    {
    this->m_M1 = -( this->m_N1 - this->m_D1 * this->m_N0 );
    this->m_M2 = -( this->m_N2 - this->m_D2 * this->m_N0 );
    this->m_M3 = -( this->m_N3 - this->m_D3 * this->m_N0 );
    this->m_M4 =                 this->m_D4 * this->m_N0;
    }

  // Compute coefficients to be used at the boundaries
  // in order to simulate edge extension boundary conditions.
  const ScalarRealType SN = this->m_N0 + this->m_N1 + this->m_N2 + this->m_N3;
  const ScalarRealType SM = this->m_M1 + this->m_M2 + this->m_M3 + this->m_M4;
  const ScalarRealType SD = 1.0 + this->m_D1 + this->m_D2 + this->m_D3 + this->m_D4;

  this->m_BN1 = this->m_D1 * SN / SD;
  this->m_BN2 = this->m_D2 * SN / SD;
  this->m_BN3 = this->m_D3 * SN / SD;
  this->m_BN4 = this->m_D4 * SN / SD;

  this->m_BM1 = this->m_D1 * SM / SD;
  this->m_BM2 = this->m_D2 * SM / SD;
  this->m_BM3 = this->m_D3 * SM / SD;
  this->m_BM4 = this->m_D4 * SM / SD;
}

template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::VerifyPreconditions()
{
  this->Superclass::VerifyPreconditions();

  if( this->m_Sigma <= 0.0 )
    {
    itkExceptionMacro( "Sigma must be greater than zero." );
    }
}

template< typename TInputImage, typename TOutputImage >
void
RecursiveGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "Sigma: " << m_Sigma << std::endl;
  os << "Order: " << m_Order << std::endl;
  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
}
} // end namespace itk

#endif
