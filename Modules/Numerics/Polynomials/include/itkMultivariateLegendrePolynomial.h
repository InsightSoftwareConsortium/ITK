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
#ifndef itkMultivariateLegendrePolynomial_h
#define itkMultivariateLegendrePolynomial_h

#include "itkIntTypes.h"
#include "itkIndent.h"
#include <vector>
#include "itkArray.h"
#include "ITKPolynomialsExport.h"

namespace itk
{
/** \class MultivariateLegendrePolynomial
 * \brief 2D and 3D multivariate Legendre Polynomial
 *
 * In 2D,
 * \f[
 *  f(x_{vector}, parameter_{vector}) =
 * \sum_i^l \left(
 * \sum_j^{l-i} \left( parameter_ {ij} * P_i(x) *P_j(y)) \right) \right)
 * \f]
 * where P_i() denoting a Legendre polynomial of degree i
 * and l it the degree of the polynomial
 *
 * In 3D,
 * \f[
 * f(x_{vector}, parameter_{vector}) =
 * \sum_i^l \left( \sum_j^{l-i} \left( \sum_k^{l-i-j} \left(
 * parameter_{ijk} * P_i(x) * P_j(y) * P_k(z) \right) \right) \right)
 * \f]
 *
 * The size of the parameter vector for 2D is
 * \f$\frac{(l+1)\cdot(1+2)}{2}\f$,
 * and for 3D is \f$\frac{(l+1)*(l+2)*(l+3)}{3!}\f$
 *
 * To get the size of the parameter vector, users can use one of the
 * two GetNumberOfCoefficients() member functions
 *
 * To get function result, users can use the operator() or its
 * SimpleForwardIterator's Get() method.
 *
 * This is a part of the bias correction methods and implementations
 * that was initially developed and implemented
 * by Martin Styner, Univ. of North Carolina at Chapel Hill, and his
 * colleagues.
 *
 * \note For more details. refer to the following articles.
 * "Parametric estimate of intensity inhomogeneities applied to MRI"
 * Martin Styner, G. Gerig, Christian Brechbuehler, Gabor Szekely,
 * IEEE TRANSACTIONS ON MEDICAL IMAGING; 19(3), pp. 153-165, 2000,
 * (http://www.cs.unc.edu/~styner/docs/tmi99.pdf)
 *
 * "Evaluation of 2D/3D bias correction with 1+1ES-optimization"
 * Martin Styner, Prof. Dr. G. Gerig (IKT, BIWI, ETH Zuerich), TR-197
 * (http://www.cs.unc.edu/~styner/docs/StynerTR97.pdf)
 * \ingroup ITKPolynomials
 */

class ITKPolynomials_EXPORT MultivariateLegendrePolynomial
{
public:
  typedef MultivariateLegendrePolynomial Self;

  typedef std::vector< double >        DoubleArrayType;
  typedef std::vector< unsigned long > ULongArrayType;
  typedef std::vector< long >          LongArrayType;

  /** Internal coefficient storage type. */
  typedef DoubleArrayType CoefficientArrayType;

  /** Same as CoefficientArray
   * This type definition will be used by EnergyFunction object. */
  typedef Array< double > ParametersType;

  /** The size of the domain. */
  typedef ULongArrayType DomainSizeType;
  typedef LongArrayType  IndexType;

  /** Constructor. */
  MultivariateLegendrePolynomial(unsigned int dimension,
                                 unsigned int degree,
                                 const DomainSizeType & domainSize);
  /** Destructor. */
  virtual ~MultivariateLegendrePolynomial();

  /** Gets the dimension. */
  unsigned int GetDimension(void) const
  { return m_Dimension; }

  /** Gets the degree (the degree of Legendre polynomials). */
  unsigned int GetDegree(void) const
  { return m_Degree; }

  /** Returns the number of coefficients of the polynomial
   *  This number is computed from the degree of the polynomial
   *  the SetCoefficients() method expects an array of this
   *  size, an exception is thrown otherwise
   *  \sa SetCoefficients
   */
  unsigned int GetNumberOfCoefficients(void) const
  { return m_NumberOfCoefficients; }

  /** Gets each dimesion's size. */
  const DomainSizeType & GetDomainSize(void) const
  { return m_DomainSize; }

  /** \class CoefficientVectorSizeMismatch
   * \brief Exception object.
   * \ingroup ITKPolynomials
   */
  class CoefficientVectorSizeMismatch
  {
public:
    CoefficientVectorSizeMismatch(int given, int required)
    {
      m_Required = required;
      m_Given = given;
    }

    int m_Required;
    int m_Given;
  };

  /** \brief Sets the Legendre polynomials' parameters.
   * \warning The number of coefficients provided should
   * match the number returned by GetNumberOfCoefficients()
   * otherwise an exception is thrown.  */
  void SetCoefficients(const CoefficientArrayType & coef);

  void SetCoefficients(const ParametersType & coef);

  /** \brief Gets Legendre polynomials' coefficients. */
  const CoefficientArrayType & GetCoefficients() const;

  /** In the case which the bias field is 2D, it returns bias value at
   * the point which is specified by the index */
  double Evaluate(IndexType & index)
  {
    if ( m_Dimension == 2 )
      {
      if ( index[1] != m_PrevY )
        {
        // normalized y [-1, 1]
        double norm_y =  m_NormFactor[1]
                        * static_cast< double >( index[1] - 1 );
        this->CalculateXCoef(norm_y, m_CoefficientArray);
        m_PrevY = index[1];
        }

      // normalized x [-1, 1]
      double norm_x =  m_NormFactor[0]
                      * static_cast< double >( index[0] - 1 );

      return LegendreSum(norm_x, m_Degree, m_CachedXCoef);
      }
    else if ( m_Dimension == 3 )
      {
      if ( index[2] != m_PrevZ )
        {
        // normalized z [-1, 1]
        double norm_z =  m_NormFactor[2]
                        * static_cast< double >( index[2] - 1 );
        this->CalculateYCoef(norm_z, m_CoefficientArray);
        m_PrevZ = index[2];
        }

      if ( index[1] != m_PrevY )
        {
        // normalized y [-1, 1]
        double norm_y =  m_NormFactor[1]
                        * static_cast< double >( index[1] - 1 );
        this->CalculateXCoef(norm_y, m_CachedYCoef);
        m_PrevY = index[1];
        }

      // normalized x [-1, 1]
      double norm_x =  m_NormFactor[0]
                      * static_cast< double >( index[0] - 1 );
      return this->LegendreSum(norm_x, m_Degree, m_CachedXCoef);
      }
    return 0;
  }

  /** Gets the number of coefficients. */
  unsigned int GetNumberOfCoefficients();

  /** Gets the number of coefficients. */
  unsigned int GetNumberOfCoefficients(unsigned int dimension, unsigned int degree);

  /** \class SimpleForwardIterator
   * \brief Iterator which only supports forward iteration and
   * Begin(), IsAtEnd(), and Get() method which work just like as
   * SimpleImageRegionIterator.
   * \ingroup ITKPolynomials
   */
  class SimpleForwardIterator
  {
public:
    SimpleForwardIterator (MultivariateLegendrePolynomial *polynomial) :
      m_MultivariateLegendrePolynomial(polynomial),
      m_Dimension  (m_MultivariateLegendrePolynomial->GetDimension()),
      m_DomainSize (m_MultivariateLegendrePolynomial->GetDomainSize()),
      m_IsAtEnd(false)
    {
      m_Index.resize(m_Dimension);
      std::fill(m_Index.begin(), m_Index.end(), 0);
    }

    void Begin(void)
    {
      m_IsAtEnd = false;
      for ( unsigned int dim = 0; dim < m_Dimension; dim++ )
        {
        m_Index[dim] = 0;
        }
    }

    bool IsAtEnd()
    { return m_IsAtEnd; }

    SimpleForwardIterator & operator++()
    {
      for ( unsigned int dim = 0; dim < m_Dimension; dim++ )
        {
        if ( m_Index[dim] < static_cast< int >( m_DomainSize[dim] - 1 ) )
          {
          m_Index[dim] += 1;
          return *this;
          }
        else
          {
          if ( dim == m_Dimension - 1 )
            {
            m_IsAtEnd = true;
            break;
            }
          else
            {
            m_Index[dim] = 0;
            }
          }
        }
      return *this;
    }

    double Get()
    { return m_MultivariateLegendrePolynomial->Evaluate(m_Index); }

private:
    MultivariateLegendrePolynomial *m_MultivariateLegendrePolynomial;
    unsigned int                    m_Dimension;
    DomainSizeType                  m_DomainSize;
    IndexType                       m_Index;
    bool                            m_IsAtEnd;
  };   // end of class Iterator

  void Print(std::ostream & os);

protected:
  void PrintSelf(std::ostream & os, Indent indent) const;

  double LegendreSum(const double x, int n,
                     const CoefficientArrayType & coef,
                     int offset = 0);

  void CalculateXCoef(double norm_y, const CoefficientArrayType & coef);

  void CalculateYCoef(double norm_z, const CoefficientArrayType & coef);

private:
  DomainSizeType m_DomainSize;
  unsigned int   m_Dimension;
  unsigned int   m_Degree;
  unsigned int   m_NumberOfCoefficients;

  CoefficientArrayType m_CoefficientArray;
  CoefficientArrayType m_CachedXCoef;
  CoefficientArrayType m_CachedYCoef;
  CoefficientArrayType m_CachedZCoef;

  DoubleArrayType m_NormFactor;
  IndexValueType  m_PrevY;
  IndexValueType  m_PrevZ;
}; // end of class

 ITKPolynomials_EXPORT std::ostream & operator<<(std::ostream & os,
                          MultivariateLegendrePolynomial & poly);
} // end of namespace itk
#endif
