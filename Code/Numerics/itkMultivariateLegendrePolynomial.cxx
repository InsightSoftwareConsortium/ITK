/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMultivariateLegendrePolynomial.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMultivariateLegendrePolynomial.h"
#include "itkMacro.h"

namespace itk {
MultivariateLegendrePolynomial
::MultivariateLegendrePolynomial( unsigned int dimension, 
                                  unsigned int degree, 
                                  const DomainSizeType & domainSize)
{
  if (dimension > 3 || dimension < 2) 
    {
    itkGenericExceptionMacro(<<"MultivariateLegendrePolynomial only supports 2D and 3D");
    }
  
  m_Dimension = dimension;

  m_Degree    = degree;
  
  m_DomainSize = domainSize;
  this->GetNumberOfCoefficients() ;
  
  // used as intermediate store to hold legendre polynomials
  // y_coef[i,j] = Sum (0 <= k <= m-i-j) p(i,j,k) * P(z)
  m_CachedYCoef = new CoefficientArrayType(((m_Degree+1)*(m_Degree+2))/2);  

  // used as intermediate store to hold legendre polynomials
  // x_coef[i] = Sum (0 <= j <= m-i) y_coef[i,j] * P(y) 
  m_CachedXCoef = new CoefficientArrayType( m_Degree+1 );

  m_CachedZCoef = new CoefficientArrayType( m_Degree+1 );
  
  m_CoefficientArray = new CoefficientArrayType( m_NumberOfCoefficients );

  m_CachedXCoef->Fill(0.0) ;
  m_CachedYCoef->Fill(0.0) ;
  m_CachedZCoef->Fill(0.0) ;
  m_CoefficientArray->Fill(0.0) ;

  m_PrevY = -1 ;
  m_PrevZ = -1 ;

  //    for (int i = 0 ; i < m_NoOfCoefficients ; i++)
  //      m_CoefficientArray[i] = 0 ;

  m_NormFactor = new DoubleArrayType(m_Dimension);
  for (unsigned int j = 0 ; j < m_Dimension ; j++)
    {
    (*m_NormFactor)[j] = 2.0f / 
      (static_cast<double>(m_DomainSize[j]) - 1.0f);
    }
}


MultivariateLegendrePolynomial
::~MultivariateLegendrePolynomial()
{
  delete m_CachedYCoef ;
  delete m_CachedXCoef ;
  delete m_CachedZCoef ;
  delete m_CoefficientArray ;
  delete m_NormFactor ;
}


void MultivariateLegendrePolynomial
::Initialize()
{
  //   m_CoefficientVector = CoefficientVectorType(m_NumberOfCoefficients) ;
  //   m_CoefficientVector.Fill(0.0f) ;
}


void MultivariateLegendrePolynomial
::SetCoefficients(const CoefficientArrayType & coefficients) 
  throw (MultivariateLegendrePolynomial::CoefficientVectorSizeMismatch)
{
  //  this->Initialize() ;

  if (coefficients.Size() != m_NumberOfCoefficients)
    {
    throw CoefficientVectorSizeMismatch(coefficients.size(),
                                        m_NumberOfCoefficients) ;
    }


  // copy coefficients to array of double
  for(unsigned int i = 0 ; i < m_NumberOfCoefficients; i++ )
    {
    (*m_CoefficientArray)[i] = coefficients[i] ; 
    }

  //   m_CoefficientVector = coefficients ;

  m_PrevY = -1  ;
  m_PrevZ = -1 ;
}


const MultivariateLegendrePolynomial::CoefficientArrayType* 
MultivariateLegendrePolynomial
::GetCoefficients( void ) const
{
  return m_CoefficientArray ;
}



void MultivariateLegendrePolynomial
::CalculateXCoef(double norm_y, const CoefficientArrayType* coef)
{
  // compute x_coef[i] = sum (0 <= j <= m-i) pij * P(y)]
  int offset = 0 ;

  for (unsigned int lx = 0; lx <= m_Degree; lx++) 
    {
    (*m_CachedXCoef)[lx] = 
      LegendreSum( norm_y, m_Degree-lx, coef, offset );

    offset += ( m_Degree + 1 - lx ); 
    }
}




void MultivariateLegendrePolynomial
::CalculateYCoef(double norm_z, const CoefficientArrayType* coef)
{
  // compute y_coef[i,j] = sum (0 <= k <= m-i-j) pijk * P(z)
  unsigned int ycoefp = 0;
  unsigned int coefp  = 0;
  const unsigned int lxmax = m_Degree;
  for (unsigned int lx = 0; lx <= lxmax; lx++) 
    {
    const unsigned int lymax = m_Degree - lx;
    for (unsigned int ly = 0; ly <= lymax; ly++, coefp++) 
      {
      unsigned int zcoefp = coefp;
      unsigned int lzmax  = m_Degree - lx - ly;
      for ( unsigned int lz = 0; lz <= lzmax; lz++ )
        { 
        (*m_CachedZCoef)[lz] = (*coef)[zcoefp];
        zcoefp += ((m_Degree+1-lz)*(m_Degree+2-lz)/2) - lx;
        }
      (*m_CachedYCoef)[ycoefp] = LegendreSum(norm_z, m_Degree-lx-ly, m_CachedZCoef); 
      ycoefp++;
      }
    }
}


double MultivariateLegendrePolynomial
::LegendreSum(const double x, int n, const CoefficientArrayType* coef, 
              int offset) 
  //n+1 elements !
{
  if (n == 0) 
    {
    return (*coef)[offset];
    }
  
  double ykp2 = 0, ykp1 = (*coef)[n+offset];
  
  for (int k = n-1; k>0; k--) 
    {
    double yk = x*ykp1*(2*k+1)/(k+1) - ykp2*(k+1)/(k+2) + (*coef)[k+offset];
    ykp2 = ykp1;
    ykp1 = yk;
    }
  return -ykp2/2 + x*ykp1 + (*coef)[offset];
}


unsigned int MultivariateLegendrePolynomial
::GetNumberOfCoefficients( void )
{ 
  // calculate the number of parameters
  unsigned int numerator   = 1;
  unsigned int denominator = 1;
  for (unsigned int i = 1 ; i <= m_Dimension ; i++)
    {
    numerator   *=  (m_Degree + i);
    denominator *=  i;
    }
  m_NumberOfCoefficients = numerator / denominator;

  return m_NumberOfCoefficients;
}


} // end of namespace itk
