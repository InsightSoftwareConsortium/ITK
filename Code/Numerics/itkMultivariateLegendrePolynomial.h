/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultivariateLegendrePolynomial.h
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
#ifndef __itkMultivariateLegendrePolynomial_h
#define __itkMultivariateLegendrePolynomial_h

#include <algorithm>
#include <vector>
#include <vnl/vnl_vector.h>


/** \class MultivariateLegendrePolynomial
 * \brief 2D and 3D multivariate Legendre Polynomial 
 *
 * In 2D, f(x_vector, parameter_vector) 
 * = sum_i (sum_j (parameter_i_j * P_i(x) *P_j(y)) 
 * over j from 0 to l - i)) over i from 0 to l 
 * where P_i() denoting a Legendre polynomial of degree i
 * and l it the degree of the polynomial
 *
 * In 3D, f(x_vector, parameter_vecter)
 * = sum_i (sum_j (sum_k (paramter_i_j_k * P_i(x) * P_j(y) * P_k(z)) 
 * over k from 0 to l - i - j) over j from 0 to l - i) over i from 0 to l
 *
 * The size of the parameter vector for 2D is (l + 1) * (l + 2) / 2, and
 * for 3D (l + 1) * (l + 2) / 2 * (l + 3) / 3
 *
 * To get the size of the parameter vector, users can use one of the 
 * two GetNoOfCoefficients() member functions
 *
 * To get function result, users can use the operator() or its 
 * SimpleForwardIterator's Get() method.
 *
 * This is a part of the bias correction methods and implemenations 
 * that was initially developed and implemented 
 * by Martin Styner, Univ. of North Carolina at Chapel Hill, and his
 * colleagues.
 *
 * For more details. refer to the following articles.
 * "Parametric estimate of intensity inhomogeneities applied to MRI" 
 * Martin Styner, G. Gerig, Christian Brechbuehler, Gabor Szekely,  
 * IEEE TRANSACTIONS ON MEDICAL IMAGING; 19(3), pp. 153-165, 2000, 
 * (http://www.cs.unc.edu/~styner/docs/tmi00.pdf)
 *
 * "Evaluation of 2D/3D bias correction with 1+1ES-optimization" 
 * Martin Styner, Prof. Dr. G. Gerig (IKT, BIWI, ETH Zuerich), TR-197
 * (http://www.cs.unc.edu/~styner/docs/StynerTR97.pdf)
 */

namespace itk {

class MultivariateLegendrePolynomial
{
public:
  /**
   * Legendre polynomial coefficients type
   */
  typedef vnl_vector<double> CoefficientVector ;

  /**
   * same as CoefficientVector
   * this type definition will be used by EnergyFunction object
   */
  typedef CoefficientVector ParametersType ;

  /**
   * internal coefficient storage type
   */
  typedef double* CoefficientArray ;


  typedef std::vector<unsigned long> DomainSizeType ;
  typedef std::vector<long> IndexType ;

  MultivariateLegendrePolynomial(int dimension, int degree,
                                 DomainSizeType domainSize) ;

  virtual ~MultivariateLegendrePolynomial() ;


  /**
   * constructor calls this function. So there is no need to
   * call separately. However, if you want to set parameters to
   * zeros, call this.
   * allocates memory for the internal coefficients array 
   * and caches.
   */
  void Initialize() ;

  /**
   * gets dimension
   */
  int GetDimension() { return m_Dimension ; }

  /**
   * gets degree (the degree of Legendre polynomials)
   */ 
  int GetDegree() { return m_Degree ; } 

  /**
   * gets each dimesion's size
   */
  DomainSizeType GetDomainSize() { return m_DomainSize ; }

  /**
   * exception object
   */
  class CoefficientVectorSizeMismatch 
  {
  public:
    CoefficientVectorSizeMismatch(int given, int required)
    {
      Required = required ;
      Given = given ;
    }
    
    int Required;
    int Given ;
  } ;

  /**
   * sets Legendre polynomials' parameters
   */
  void SetCoefficients(CoefficientVector coef) 
    throw (CoefficientVectorSizeMismatch) ;

  /**
   * gets coefficients
   */
  CoefficientVector& GetCoefficients() ;
 
  /**
   * In the case which the bias field is 2D, it returns bias value at
   * the point which is specified by the index
   */
  double operator() (IndexType index) 
  {
    if (m_Dimension == 2)
      {
        if (index[1] != m_PrevY)
          {
            // normalized y [-1, 1]
            double norm_y = static_cast<double>(
                              m_NormFactor[1] * index[1] - 1 );
            CalculateXCoef(norm_y, m_CoefficientArray) ;
            m_PrevY = index[1] ;
          }
        
        // normalized x [-1, 1]
        double norm_x = static_cast<double>(
                              m_NormFactor[0] * index[0] - 1 );
        
        return LegendreSum(norm_x, m_Degree, m_CachedXCoef) ;
      }
    else if (m_Dimension == 3)
      {
        if (m_PrevZ != index[2])
          {
            // normalized z [-1, 1]  
            double norm_z = static_cast<double>(
                              m_NormFactor[2] * index[2] - 1 );
            CalculateYCoef(norm_z, m_CoefficientArray) ;
            m_PrevZ = index[2] ;
          }
        
        if (m_PrevY != index[1])
          {
            // normalized y [-1, 1]
            double norm_y = static_cast<double>(
                              m_NormFactor[1] * index[1] - 1 ); 
            CalculateXCoef(norm_y, m_CachedYCoef) ;
            m_PrevY = index[1] ;
          }
        
        // normalized x [-1, 1]
        double norm_x = static_cast<double>(
                    m_NormFactor[0] * index[0] - 1 ); 
        
        return LegendreSum(norm_x, m_Degree, m_CachedXCoef);
      }
    return 0 ;
  }

  /**
   * gets the number of coefficients
   */
  int GetNoOfCoefficients() ;

  /**
   * gets the number of coefficients
   */
  int GetNoOfCoefficients(int dimension, int degree) ;


  /**
   * iterator which only supports forward iteration and Begin(), IsAtEnd()
   * , and Get() method which work just like as SimpleImageRegionIterator.
   */
  class SimpleForwardIterator
  {
  public:

    SimpleForwardIterator (MultivariateLegendrePolynomial* polynomial) 
    {
      m_MultivariateLegendrePolynomial = polynomial ;
      m_Dimension = m_MultivariateLegendrePolynomial->GetDimension() ;
      m_DomainSize = m_MultivariateLegendrePolynomial->GetDomainSize() ;
      m_Index.resize(m_Dimension) ;
    }
    
    void Begin() 
    {
      std::fill(m_Index.begin(), m_Index.end(), 0) ;
    }
    
    bool IsAtEnd()
    {
      return m_IsAtEnd ;
    }
    
    SimpleForwardIterator& operator++()
    {
      for (int dim = 0 ; dim < m_Dimension ; dim++)
        {
          if (m_Index[dim] < static_cast<unsigned int>(m_DomainSize[dim] - 1))
            {
              m_Index[dim] += 1 ;
              return *this ;
            }
          else
            {
              if (dim == m_Dimension - 1 )
                {
                  m_IsAtEnd = true ;
                  break ;
                }
              else
                {
                  m_Index[dim] = 0 ;
                }
            }
        }
        return *this ;
    }
    
    double Get()
    {
      return (*m_MultivariateLegendrePolynomial)(m_Index) ;
    }
    
  private:
    MultivariateLegendrePolynomial* m_MultivariateLegendrePolynomial ;
    int m_Dimension ;
    DomainSizeType m_DomainSize ;
    IndexType m_Index ;
    bool m_IsAtEnd ;
  } ; // end of class Iterator 
  
protected:

  double LegendreSum(const double x, int n, double* coef) ; 
  void CalculateXCoef(double norm_y, double* coef) ;
  void CalculateYCoef(double norm_z, double* coef) ;
  void DeleteArrays() ;

private:
  DomainSizeType m_DomainSize ;
  int m_Dimension ;
  int m_Degree ;
  int m_NoOfCoefficients ;
  bool m_MultiplicativeBias; 
  
  CoefficientVector m_CoefficientVector ;
  CoefficientArray m_CoefficientArray ;
  CoefficientArray m_CachedXCoef ;
  CoefficientArray m_CachedYCoef ;
  CoefficientArray m_CachedZCoef ;
  std::vector<double> m_NormFactor ;
  long m_PrevY ;
  long m_PrevZ ;
} ; // end of class

} // end of namespace itk
#endif
