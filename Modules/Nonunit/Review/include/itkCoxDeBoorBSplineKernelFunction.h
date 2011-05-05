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
#ifndef __itkCoxDeBoorBSplineKernelFunction_h
#define __itkCoxDeBoorBSplineKernelFunction_h

#include "itkKernelFunction.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_real_polynomial.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class CoxDeBoorBSplineKernelFunction
 * \brief BSpline kernel used for density estimation and nonparameteric
 *  regression.
 *
 * This class enscapsulates BSpline kernel for
 * density estimation or nonparameteric regression.
 * See documentation for KernelFunction for more details.
 *
 * This class is templated over the spline order to cohere with
 * the previous incarnation of this class. One can change the
 * order during an instantiation's existence.  Note that
 * other authors have defined the B-spline order as being the
 * degree of spline + 1.  In the ITK context (e.g. in this
 * class), the spline order is equivalent to the degree of
 * the spline.
 *
 * \author Nicholas J. Tustison
 *
 * This code was contributed in the Insight Journal paper:
 * "N-D C^k B-Spline Scattered Data Approximation"
 * by Nicholas J. Tustison, James C. Gee
 * http://hdl.handle.net/1926/140
 * http://www.insight-journal.org/browse/publication/57
 *
 *
 * \sa KernelFunction
 *
 * \ingroup Functions
 * \ingroup ITK-Review
 */
template< unsigned int VSplineOrder = 3 >
class ITK_EXPORT CoxDeBoorBSplineKernelFunction:
  public KernelFunction
{
public:
  /** Standard class typedefs. */
  typedef CoxDeBoorBSplineKernelFunction Self;
  typedef KernelFunction                 Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CoxDeBoorBSplineKernelFunction, KernelFunction);

  typedef double                 RealType;
  typedef vnl_vector< RealType > VectorType;
  typedef vnl_real_polynomial    PolynomialType;
  typedef vnl_matrix< RealType > MatrixType;

  /** Get/Sets the Spline Order */
  void SetSplineOrder(unsigned int);

  itkGetConstMacro(SplineOrder, unsigned int);

  /** Evaluate the function. */
  inline RealType Evaluate(const RealType & u) const
  {
    RealType     absValue = vnl_math_abs(u);
    unsigned int which;

    if ( this->m_SplineOrder % 2 == 0 )
      {
      which = static_cast< unsigned int >( absValue + 0.5 );
      }
    else
      {
      which = static_cast< unsigned int >( absValue );
      }
    if ( which < this->m_BSplineShapeFunctions.rows() )
      {
      return PolynomialType(
               this->m_BSplineShapeFunctions.get_row(which) ).evaluate(absValue);
      }
    else
      {
      return NumericTraits< RealType >::Zero;
      }
  }

  /** Evaluate the first derivative. */
  inline RealType EvaluateDerivative(const double & u) const
  {
    return this->EvaluateNthDerivative(u, 1);
  }

  /** Evaluate the Nth derivative. */
  inline RealType EvaluateNthDerivative(const double & u, unsigned int n) const
  {
    RealType     absValue = vnl_math_abs(u);
    unsigned int which;

    if ( this->m_SplineOrder % 2 == 0 )
      {
      which = static_cast< unsigned int >( absValue + 0.5 );
      }
    else
      {
      which = static_cast< unsigned int >( absValue );
      }
    if ( which < this->m_BSplineShapeFunctions.rows() )
      {
      PolynomialType polynomial(
        this->m_BSplineShapeFunctions.get_row(which) );
      for ( unsigned int i = 0; i < n; i++ )
        {
        polynomial = polynomial.derivative();
        }
      RealType der = polynomial.evaluate(absValue);
      if ( u < NumericTraits< RealType >::Zero && n % 2 != 0 )
        {
        return -der;
        }
      else
        {
        return der;
        }
      }
    else
      {
      return NumericTraits< RealType >::Zero;
      }
  }

  /**
   * For a specific order, return the ceil( 0.5*(m_SplineOrder+1) )
   * pieces of the single basis function centered at zero for positive
   * parametric values.
   */
  MatrixType GetShapeFunctions()
  {
    return this->m_BSplineShapeFunctions;
  }

  /**
   * For a specific order, generate and return the (this->m_SplineOrder+1)
   * pieces of the different basis functions in the [0, 1] interval.
   */
  MatrixType GetShapeFunctionsInZeroToOneInterval();

protected:
  CoxDeBoorBSplineKernelFunction();
  ~CoxDeBoorBSplineKernelFunction();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  CoxDeBoorBSplineKernelFunction(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  /**
   * For a specific order, generate the (this->m_SplineOrder+1) pieces of
   * the single basis function centered at zero.
   */
  void GenerateBSplineShapeFunctions(unsigned int);

  /**
   * Use the CoxDeBoor recursion relation to generate the piecewise
   * polynomials which compose the basis function.
   * See, for example, L. Piegl, L. Tiller, "The NURBS Book,"
   * Springer 1997, p. 50.
   */
  PolynomialType CoxDeBoor(unsigned short,
                           VectorType, unsigned int, unsigned int);

  MatrixType   m_BSplineShapeFunctions;
  unsigned int m_SplineOrder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCoxDeBoorBSplineKernelFunction.txx"
#endif

#endif
