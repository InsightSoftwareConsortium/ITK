/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMultivariateLegendrePolynomial.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultivariateLegendrePolynomial_h
#define __itkMultivariateLegendrePolynomial_h

#include "itkArray.h"

namespace itk {

/** \class MultivariateLegendrePolynomial
 * \brief 2D and 3D multivariate Legendre Polynomial 
 *
 * In 2D, 
 * \f[
 *  f(x_vector, parameter_vector) 
 * = sum_i (sum_j (parameter_{ i j } * P_i(x) *P_j(y)) 
 * over j from 0 to l - i)) over i from 0 to l 
 * \f]
 * where P_i() denoting a Legendre polynomial of degree i
 * and l it the degree of the polynomial
 *
 * In 3D,
 * \f[
 * f(x_vector, parameter_vecter)
 * = sum_i (sum_j (sum_k (parameter_{i j k} * P_i(x) * P_j(y) * P_k(z)) 
 * over k from 0 to l - i - j) over j from 0 to l - i) over i from 0 to l
 * \f]
 *
 * The size of the parameter vector for 2D is \f$ (l + 1) * (l + 2) / 2 \f$,
 * and for 3D is \f$ (l + 1) * (l + 2) / 2 * (l + 3) / 3 \f$
 *
 * To get the size of the parameter vector, users can use one of the 
 * two GetNumberOfCoefficients() member functions
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
 * (http://www.ia.unc.edu/~styner/docs/tmi00.pdf)
 *
 * "Evaluation of 2D/3D bias correction with 1+1ES-optimization" 
 * Martin Styner, Prof. Dr. G. Gerig (IKT, BIWI, ETH Zuerich), TR-197
 * (http://www.ia.unc.edu/~styner/docs/StynerTR97.pdf)
 */

class MultivariateLegendrePolynomial
{
public:
  //   /** Legendre polynomial coefficients type. */
  //   typedef Array<double>             CoefficientVectorType;

  typedef Array< double > DoubleArrayType ;
  typedef Array< unsigned long > ULongArrayType ;
  typedef Array< long > LongArrayType ;

  /** Internal coefficient storage type. */
  typedef DoubleArrayType CoefficientArrayType ;

  /** Same as CoefficientArray
   * This type definition will be used by EnergyFunction object. */
  typedef DoubleArrayType ParametersType ;

  /** The size of the domain. */
  typedef ULongArrayType DomainSizeType ;
  typedef LongArrayType IndexType ;
  
  /** Constructor. */
  MultivariateLegendrePolynomial( unsigned int dimension, 
                                  unsigned int degree,
                                  const DomainSizeType & domainSize );
  /** Destructor. */
  virtual ~MultivariateLegendrePolynomial();

  /** Constructor calls this function. So there is no need to
   * call separately. However, if you want to set parameters to
   * zeros, call this.
   * Allocates memory for the internal coefficients array 
   * and caches. */
  void Initialize(void) ;

  /** Gets the dimension. */
  unsigned int GetDimension(void) const 
  { return m_Dimension ; }

  /** Gets the degree (the degree of Legendre polynomials). */ 
  unsigned int GetDegree(void) const 
  { return m_Degree ; } 

  /** Returns the number of coefficients of the polynomial  
   *  This number is computed from the degree of the polynomial 
   *  the SetCoefficients() method expects an array of this 
   *  size, an exception is thrown otherwise
   *  \sa SetCoefficients                                   */
  unsigned int GetNumberOfCoefficients(void) const
  { return m_NumberOfCoefficients; }

  /** Gets each dimesion's size. */
  const DomainSizeType & GetDomainSize( void ) const 
  { return m_DomainSize; }

  /** Exception object. */
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

  /** Sets the Legendre polynomials' parameters. 
   * \warning The number of coefficients provided should
   * match the number returned by GetNumberOfCoefficients()
   * otherwise an exception is thrown.  */
  void SetCoefficients(const CoefficientArrayType & coef) 
    throw (CoefficientVectorSizeMismatch) ;

  /** Gets Legendre polynomials' coefficients. */
  const CoefficientArrayType* GetCoefficients(void) const;
 
  /** In the case which the bias field is 2D, it returns bias value at
   * the point which is specified by the index */
  double operator() (IndexType index) 
  {
    if (m_Dimension == 2)
      {
        if (index[1] != m_PrevY)
          {
            // normalized y [-1, 1]
            double norm_y =  (*m_NormFactor)[1] *
              static_cast<double>( index[1] - 1 );
            CalculateXCoef(norm_y, m_CoefficientArray) ;
            m_PrevY = index[1] ;
          }
        
        // normalized x [-1, 1]
        double norm_x =  (*m_NormFactor)[0] *
          static_cast<double>( index[0] - 1 );
        
        return LegendreSum(norm_x, m_Degree, m_CachedXCoef) ;
      }
    else if (m_Dimension == 3)
      {
        if (m_PrevZ != index[2])
          {
            // normalized z [-1, 1]  
            double norm_z =  (*m_NormFactor)[2] *
              static_cast<double>( index[2] - 1 );
            CalculateYCoef(norm_z, m_CoefficientArray) ;
            m_PrevZ = index[2] ;
          }
        
        if (m_PrevY != index[1])
          {
            // normalized y [-1, 1]
            double norm_y =  (*m_NormFactor)[1] *
              static_cast<double>( index[1] - 1 ); 
            CalculateXCoef(norm_y, m_CachedYCoef) ;
            m_PrevY = index[1] ;
          }
        
        // normalized x [-1, 1]
        double norm_x =  (*m_NormFactor)[0] *
          static_cast<double>( index[0] - 1 ); 
        return LegendreSum(norm_x, m_Degree, m_CachedXCoef) ;
      }
    return 0 ;
  }

  /** Gets the number of coefficients. */
  unsigned int GetNumberOfCoefficients(void);

  /** Gets the number of coefficients. */
  unsigned int GetNumberOfCoefficients(unsigned int dimension, unsigned int degree) ;

  /** Iterator which only supports forward iteration and Begin(), IsAtEnd(), 
   *  and Get() method which work just like as SimpleImageRegionIterator. */
  class SimpleForwardIterator
  {
  public:
    SimpleForwardIterator (MultivariateLegendrePolynomial* polynomial) 
    {
      m_MultivariateLegendrePolynomial = polynomial ;
      m_Dimension   = m_MultivariateLegendrePolynomial->GetDimension();
      m_DomainSize  = m_MultivariateLegendrePolynomial->GetDomainSize();
      m_Index       = IndexType(m_Dimension);
    }
    
    void Begin( void ) 
    { 
      m_Index.Fill( 0 ) ;
      m_IsAtEnd = false ;
    }
    
    bool IsAtEnd()
    { return m_IsAtEnd; }
    
    SimpleForwardIterator& operator++()
    {
      for (unsigned int dim = 0 ; dim < m_Dimension ; dim++)
        {
          if (m_Index[dim] < static_cast<int>(m_DomainSize[dim] - 1))
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
    { return (*m_MultivariateLegendrePolynomial)(m_Index); }
    
  private:
    MultivariateLegendrePolynomial* m_MultivariateLegendrePolynomial;
    unsigned int      m_Dimension; 
    DomainSizeType    m_DomainSize;
    IndexType         m_Index;
    bool              m_IsAtEnd;
  } ; // end of class Iterator 
  
protected:
  double LegendreSum(const double x, int n, const CoefficientArrayType* coef,
                     int offset = 0); 
  void CalculateXCoef(double norm_y, const CoefficientArrayType* coef);
  void CalculateYCoef(double norm_z, const CoefficientArrayType* coef);

private:
  DomainSizeType m_DomainSize;
  unsigned int m_Dimension;
  unsigned int m_Degree;
  unsigned int m_NumberOfCoefficients;
  bool m_MultiplicativeBias; 
  
  //   CoefficientVectorType   m_CoefficientVector;
  CoefficientArrayType* m_CoefficientArray;
  CoefficientArrayType* m_CachedXCoef;
  CoefficientArrayType* m_CachedYCoef;
  CoefficientArrayType* m_CachedZCoef;
  DoubleArrayType* m_NormFactor;
  long m_PrevY ;
  long m_PrevZ ;
} ; // end of class

} // end of namespace itk
#endif
