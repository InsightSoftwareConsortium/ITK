/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultivariateLegendrePolynomial.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkMultivariateLegendrePolynomial.h"

namespace itk {
MultivariateLegendrePolynomial
::MultivariateLegendrePolynomial(int dimension, int degree, 
                                 DomainSizeType domainSize)
{
  if (dimension > 3 || dimension < 2)
    exit(0) ;
  
  m_Dimension = dimension ;

  if (degree < 0)
    exit(0) ;

  m_Degree = degree ;
  
  m_CachedYCoef = 0 ;
  m_CachedXCoef = 0 ;
  m_CachedZCoef = 0 ;
  m_CoefficientArray = 0 ;

  m_DomainSize = domainSize ;
  this->GetNoOfCoefficients() ;
  
  this->Initialize() ;
}


MultivariateLegendrePolynomial
::~MultivariateLegendrePolynomial()
{
  DeleteArrays() ;
}


void MultivariateLegendrePolynomial
::Initialize()
{


  this->DeleteArrays() ;

  // used as intermediate store to hold legendre polynomials
  // y_coef[i,j] = Sum (0 <= k <= m-i-j) p(i,j,k) * P(z)
  m_CachedYCoef = new double[(m_Degree+1)*(m_Degree+2)/2];  

  // used as intermediate store to hold legendre polynomials
  // x_coef[i] = Sum (0 <= j <= m-i) y_coef[i,j] * P(y) 
  m_CachedXCoef = new double [m_Degree+1];

  m_CachedZCoef = new double [m_Degree+1];
  
  m_CoefficientArray = new double[m_NoOfCoefficients] ;

  m_PrevY = -1 ;
  m_PrevZ = -1 ;

//    for (int i = 0 ; i < m_NoOfCoefficients ; i++)
//      m_CoefficientArray[i] = 0 ;

  m_NormFactor.resize(m_Dimension) ;
  for (int j = 0 ; j < m_Dimension ; j++)
    {
      m_NormFactor[j] = 2 / (m_DomainSize[j] - 1.0);
    }

  m_CoefficientVector.resize(m_NoOfCoefficients) ;
  m_CoefficientVector.fill(0) ;
}


void MultivariateLegendrePolynomial
::SetCoefficients(CoefficientVector coefficients) 
  throw (CoefficientVectorSizeMismatch)
{
  //  this->Initialize() ;

  if (coefficients.size() != m_NoOfCoefficients)
    {
      throw CoefficientVectorSizeMismatch(coefficients.size(),
                                          m_NoOfCoefficients) ;
    }


  // copy coefficients to array of double
  for(int i = 0 ; i < m_NoOfCoefficients ; i++)
    {
      m_CoefficientArray[i] = coefficients[i] ; 
    }

  m_CoefficientVector = coefficients ;

  m_PrevY = -1  ;
  m_PrevZ = -1 ;
}


MultivariateLegendrePolynomial::CoefficientVector& 
MultivariateLegendrePolynomial
::GetCoefficients()
{
  return m_CoefficientVector ;
}



void MultivariateLegendrePolynomial
::CalculateXCoef(double norm_y, double* coef)
{
  // compute x_coef[i] = sum (0 <= j <= m-i) pij * P(y)]
  int offset = 0 ;

  for (int lx = 0; lx <= m_Degree; lx++) 
    {
      m_CachedXCoef[lx] = LegendreSum(norm_y, m_Degree-lx, 
                                        coef+offset);
      offset += m_Degree+1-lx;
    }
}




void MultivariateLegendrePolynomial
::CalculateYCoef(double norm_z, double* coef)
{
  // compute y_coef[i,j] = sum (0 <= k <= m-i-j) pijk * P(z)
  double *ycoefp = m_CachedYCoef ;
  const double *coefp = coef;
  for (int lx = 0; lx <= m_Degree; lx++) 
    {
      for (int ly = 0; ly <= m_Degree-lx; ly++, coefp++) 
        {
          const double *zcoefp = coefp;
          for (int lz = 0, offset = 0; lz <= m_Degree-lx-ly; 
               zcoefp += ((m_Degree+1-lz)*(m_Degree+2-lz)/2) - lx, lz++) 
            m_CachedZCoef[lz] = *zcoefp;
          *ycoefp = LegendreSum(norm_z, m_Degree-lx-ly, m_CachedZCoef);	
          ycoefp++;
        }
    }
}


double MultivariateLegendrePolynomial
::LegendreSum(const double x, int n, double* coef) 
//n+1 elements !
{
  if (n == 0) 
    return coef[0];

  double ykp2 = 0, ykp1 = coef[n];

  for (int k = n-1; k>0; k--) 
    {
      double yk = x*ykp1*(2*k+1)/(k+1) - ykp2*(k+1)/(k+2) + coef[k];
      ykp2 = ykp1;
      ykp1 = yk;
    }
  return -ykp2/2 + x*ykp1 + coef[0];
}


int MultivariateLegendrePolynomial
::GetNoOfCoefficients()
{ 
  // calculate the number of parameters
  float result = 1.0 ;

  for (int i = 1 ; i <= m_Dimension ; i++)
    result *= (float) (m_Degree + i) / (float) i  ;

  m_NoOfCoefficients = (int) result ;

  return m_NoOfCoefficients ;
}

void MultivariateLegendrePolynomial
::DeleteArrays()
{
  if (m_CachedYCoef)
    delete m_CachedYCoef ;

  if (m_CachedXCoef)
    delete m_CachedXCoef ;

  if (m_CachedZCoef)
    delete m_CachedZCoef ;

  if (m_CoefficientArray)
    delete m_CoefficientArray ;

  m_CachedYCoef = 0 ;
  m_CachedXCoef = 0 ;
  m_CachedZCoef = 0 ;
  m_CoefficientArray = 0 ;
}

} // end of namespace itk
