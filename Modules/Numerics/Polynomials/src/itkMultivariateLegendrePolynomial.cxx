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
#include "itkMultivariateLegendrePolynomial.h"
#include <iostream>

namespace itk
{
MultivariateLegendrePolynomial
::MultivariateLegendrePolynomial(unsigned int dimension,
                                 unsigned int degree,
                                 const DomainSizeType & domainSize)
{
  if ( dimension > 3 || dimension < 2 )
    {
    itkGenericExceptionMacro(<< "MultivariateLegendrePolynomial only supports 2D and 3D");
    }

  m_Dimension = dimension;
  m_Degree    = degree;
  m_DomainSize = domainSize;
  m_NumberOfCoefficients = this->GetNumberOfCoefficients(dimension, degree);

  // used as intermediate store to hold legendre polynomials
  // y_coef[i,j] = Sum (0 <= k <= m-i-j) p(i,j,k) * P(z)
  m_CachedYCoef.resize( ( ( m_Degree + 1 ) * ( m_Degree + 2 ) ) / 2, 0.0 );

  // used as intermediate store to hold legendre polynomials
  // x_coef[i] = Sum (0 <= j <= m-i) y_coef[i,j] * P(y)
  m_CachedXCoef.resize(m_Degree + 1, 0.0);

  m_CachedZCoef.resize(m_Degree + 1, 0.0);

  m_CoefficientArray.resize(m_NumberOfCoefficients, 0.0);

  m_PrevY = -1;
  m_PrevZ = -1;

  m_NormFactor = DoubleArrayType(m_Dimension);
  for ( unsigned int j = 0; j < m_Dimension; j++ )
    {
    m_NormFactor[j] = 2.0f
                      / ( static_cast< double >( m_DomainSize[j] ) - 1.0f );
    }
}

MultivariateLegendrePolynomial
::~MultivariateLegendrePolynomial()
{}

void MultivariateLegendrePolynomial
::Print(std::ostream & os)
{
  itk::Indent indent(4);

  this->PrintSelf(os, indent);
}

void MultivariateLegendrePolynomial
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Dimension: " << m_Dimension << std::endl;
  os << indent << "Degree: " << m_Degree << std::endl;
  os << indent << "DomainSize: ";
  for ( unsigned int i = 0; i < m_DomainSize.size(); ++i )
    {
    os << m_DomainSize[i] << " ";
    }
  os << std::endl;

  os << indent << "Cached X coefficients: ";
  for ( unsigned int i = 0; i < m_CachedXCoef.size(); ++i )
    {
    os << m_CachedXCoef[i] << " ";
    }
  os << std::endl;

  os << indent << "Cached Y coefficients: ";
  for ( unsigned int i = 0; i < m_CachedYCoef.size(); ++i )
    {
    os << m_CachedYCoef[i] << " ";
    }
  os << std::endl;

  os << indent << "Cached Z coefficients: ";
  for ( unsigned int i = 0; i < m_CachedZCoef.size(); ++i )
    {
    os << m_CachedZCoef[i] << " ";
    }
  os << std::endl;

  os << indent << "Coefficients: ";
  for ( unsigned int i = 0; i < m_CoefficientArray.size(); ++i )
    {
    os << m_CoefficientArray[i] << " ";
    }
  os << std::endl;

  os << indent << "Normalization factors: ";
  for ( unsigned int i = 0; i < m_NormFactor.size(); ++i )
    {
    os << m_NormFactor[i] << " ";
    }
  os << std::endl;

  os << indent << "Previous Y index: " << m_PrevY << std::endl;
  os << indent << "Previous Z index: " << m_PrevZ << std::endl;
}

void MultivariateLegendrePolynomial
::SetCoefficients(const CoefficientArrayType & coefficients)
{
  if ( coefficients.size() != m_NumberOfCoefficients )
    {
    throw CoefficientVectorSizeMismatch(static_cast<int>( coefficients.size() ),
                                        static_cast<int>( m_NumberOfCoefficients) );
    }

  // copy coefficients to array of double
  m_CoefficientArray.resize(m_NumberOfCoefficients);
  for ( unsigned int i = 0; i < m_NumberOfCoefficients; i++ )
    {
    m_CoefficientArray[i] = coefficients[i];
    }

  //   m_CoefficientVector = coefficients;

  m_PrevY = -1;
  m_PrevZ = -1;
}

void MultivariateLegendrePolynomial
::SetCoefficients(const ParametersType & coefficients)
{
  if ( coefficients.size() != m_NumberOfCoefficients )
    {
    throw CoefficientVectorSizeMismatch(coefficients.size(),
                                        m_NumberOfCoefficients);
    }

  // copy coefficients to array of double
  m_CoefficientArray.resize(m_NumberOfCoefficients);
  for ( unsigned int i = 0; i < m_NumberOfCoefficients; i++ )
    {
    m_CoefficientArray[i] = coefficients[i];
    }

  //   m_CoefficientVector = coefficients;

  m_PrevY = -1;
  m_PrevZ = -1;
}

const MultivariateLegendrePolynomial::CoefficientArrayType &
MultivariateLegendrePolynomial
::GetCoefficients(void) const
{
  return m_CoefficientArray;
}

void MultivariateLegendrePolynomial
::CalculateXCoef(double norm_y, const CoefficientArrayType & coef)
{
  // compute x_coef[i] = sum (0 <= j <= m-i) pij * P(y)]
  int offset = 0;

  for ( unsigned int lx = 0; lx <= m_Degree; lx++ )
    {
    m_CachedXCoef[lx] =
      LegendreSum(norm_y, m_Degree - lx, coef, offset);

    offset += ( m_Degree + 1 - lx );
    }
}

void MultivariateLegendrePolynomial
::CalculateYCoef(double norm_z, const CoefficientArrayType & coef)
{
  // compute y_coef[i,j] = sum (0 <= k <= m-i-j) pijk * P(z)
  unsigned int       y_index = 0;
  unsigned int       c_index  = 0;
  const unsigned int lxmax = m_Degree;

  for ( unsigned int lx = 0; lx <= lxmax; lx++ )
    {
    const unsigned int lymax = m_Degree - lx;
    for ( unsigned int ly = 0; ly <= lymax; ly++, c_index++ )
      {
      unsigned int z_index = c_index;
      unsigned int lzmax  = m_Degree - lx - ly;
      for ( unsigned int lz = 0; lz <= lzmax; lz++ )
        {
        m_CachedZCoef[lz] = coef[z_index];
        z_index += ( ( m_Degree + 1 - lz ) * ( m_Degree + 2 - lz ) / 2 ) - lx;
        }
      m_CachedYCoef[y_index] = LegendreSum(norm_z, m_Degree - lx - ly, m_CachedZCoef);
      ++y_index;
      }
    }
}

double MultivariateLegendrePolynomial
::LegendreSum(const double x, int n, const CoefficientArrayType & coef,
              int offset)
//n+1 elements !
{
  if ( n == 0 )
    {
    return coef[offset];
    }

  double ykp2 = 0, ykp1 = coef[n + offset];

  for ( int k = n - 1; k > 0; k-- )
    {
    double yk = x * ykp1 * ( 2 * k + 1 ) / ( k + 1 ) - ykp2 * ( k + 1 ) / ( k + 2 ) + coef[k + offset];
    ykp2 = ykp1;
    ykp1 = yk;
    }
  return -ykp2 / 2 + x * ykp1 + coef[offset];
}

unsigned int MultivariateLegendrePolynomial
::GetNumberOfCoefficients(unsigned int dimension, unsigned int degree)
{
  // calculate the number of parameters
  unsigned int numerator   = 1;
  unsigned int denominator = 1;

  for ( unsigned int i = 1; i <= dimension; i++ )
    {
    numerator *= ( degree + i );
    denominator *= i;
    }
  return numerator / denominator;
}

unsigned int MultivariateLegendrePolynomial
::GetNumberOfCoefficients()
{
  return m_NumberOfCoefficients;
}

std::ostream & operator<<(std::ostream & os,
                          MultivariateLegendrePolynomial & poly)
{
  poly.Print(os);
  return os;
}
} // end of namespace itk
