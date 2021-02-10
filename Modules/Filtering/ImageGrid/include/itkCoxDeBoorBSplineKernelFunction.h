/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkCoxDeBoorBSplineKernelFunction_h
#define itkCoxDeBoorBSplineKernelFunction_h

#include "itkKernelFunctionBase.h"
#include "vnl/vnl_real_polynomial.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/**
 *\class CoxDeBoorBSplineKernelFunction
 * \brief BSpline kernel used for density estimation and nonparametric
 *  regression.
 *
 * This class encapsulates BSpline kernel for
 * density estimation or nonparametric regression.
 * See documentation for KernelFunctionBase for more details.
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
 * https://www.insight-journal.org/browse/publication/57
 *
 *
 * \sa KernelFunctionBase
 *
 * \ingroup ITKImageGrid
 */
template <unsigned int VSplineOrder = 3, typename TRealValueType = double>
class ITK_TEMPLATE_EXPORT CoxDeBoorBSplineKernelFunction : public KernelFunctionBase<TRealValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CoxDeBoorBSplineKernelFunction);

  /** Standard class type aliases. */
  using Self = CoxDeBoorBSplineKernelFunction;
  using Superclass = KernelFunctionBase<TRealValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using RealType = typename Superclass::RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CoxDeBoorBSplineKernelFunction, KernelFunctionBase);

  using VectorType = vnl_vector<TRealValueType>;
  using PolynomialType = vnl_real_polynomial;
  using MatrixType = vnl_matrix<TRealValueType>;

  /** Set the spline order. */
  void
  SetSplineOrder(const unsigned int);

  /** Get the spline order. */
  itkGetConstMacro(SplineOrder, unsigned int);

  /** Evaluate the function. */
  TRealValueType
  Evaluate(const TRealValueType &) const override;

  /** Evaluate the first derivative. */
  TRealValueType
  EvaluateDerivative(const TRealValueType &) const;

  /** Evaluate the Nth derivative. */
  TRealValueType
  EvaluateNthDerivative(const TRealValueType &, const unsigned int) const;

  /**
   * For a specific order, return the ceil( 0.5*(m_SplineOrder+1) )
   * pieces of the single basis function centered at zero for positive
   * parametric values.
   */
  MatrixType
  GetShapeFunctions();

  /**
   * For a specific order, generate and return the (this->m_SplineOrder+1)
   * pieces of the different basis functions in the [0, 1] interval.
   */
  MatrixType
  GetShapeFunctionsInZeroToOneInterval();

protected:
  CoxDeBoorBSplineKernelFunction();
  ~CoxDeBoorBSplineKernelFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /**
   * For a specific order, generate the (this->m_SplineOrder+1) pieces of
   * the single basis function centered at zero.
   */
  void
  GenerateBSplineShapeFunctions(const unsigned int);

  /**
   * Use the CoxDeBoor recursion relation to generate the piecewise
   * polynomials which compose the basis function.
   * See, for example, L. Piegl, L. Tiller, "The NURBS Book,"
   * Springer 1997, p. 50.
   */
  PolynomialType
  CoxDeBoor(const unsigned short, const VectorType, const unsigned int, const unsigned int);

  MatrixType   m_BSplineShapeFunctions;
  unsigned int m_SplineOrder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCoxDeBoorBSplineKernelFunction.hxx"
#endif

#endif
