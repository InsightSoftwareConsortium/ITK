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
#ifndef itkCumulativeGaussianCostFunction_h
#define itkCumulativeGaussianCostFunction_h

#include "itkMultipleValuedCostFunction.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class CumulativeGaussianCostFunction
 * \brief Cost function for the Cumulative Gaussian Optimizer.
 *
 * The Cumulative Gaussian is defined as the integral of
 * a normalized Gaussian over the domain \f$ [-\infty, \infty] \f$.
 *
 * \par Let G(x) be the normalized Gaussian defined as
 * \f$ G(x) = \frac{1}{{\sigma \sqrt {2\pi } }}e^{ - \frac{{\left( {x -
 * \mu } \right)^2 }}{{2\sigma ^2 }}} \f$.
 * The Cumulative Gaussian, is acquired by integrating G(x) then scaling and
 * offseting it by the lower asymptotes \f$ I_1 \f$ and upper \f$ I_2 \f$:
 * \f$ C\left( x \right) = I_1  + \frac{{I_2  - I_1 }}{2}\left( {1 +
 * erf\left( {\frac{{x - \mu }}{{\sigma \sqrt 2 }}} \right)} \right) \f$, where
 * \f$ C\left( { - \infty } \right) = I_1 \f$ and
 * \f$ C\left( \infty  \right) = I_2 \f$.
 *
 * \par C(x) can only be tabulated since it's a variation of the
 * error function. It is included in this class
 * as the function EvaluateCumulativeGaussian, where the argument
 * of the function is \f$ {\frac{{x - \mu }}{{\sigma \sqrt 2 }}} \f$.
 *
 * \ingroup Numerics Cost Functions
 * \ingroup ITKOptimizers
 */

class ITKOptimizers_EXPORT CumulativeGaussianCostFunction:public MultipleValuedCostFunction
{
public:

  /** Standard typedefs. */
  typedef CumulativeGaussianCostFunction Self;
  typedef MultipleValuedCostFunction     Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CumulativeGaussianCostFunction, MultipleValuedCostFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Array Typedefs. */
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::MeasureType    MeasureType;
  typedef Superclass::DerivativeType DerivativeType;

  /** The dimensions of parameter space; mean, standard deviation, lower and
    upper asymptotes. */
  enum { SpaceDimension = 4 };

  /** Not necessary for this optimizer. */
  void GetDerivative( const ParametersType & itkNotUsed(parameters),
                      DerivativeType & itkNotUsed(derivative) ) const ITK_OVERRIDE {}

  /** Return the values evaluated for the given parameters. */
  virtual MeasureType GetValue(const ParametersType & parameters) const ITK_OVERRIDE;

  /** Return a pointer of values evaluated for the given parameters. */
  MeasureType * GetValuePointer(ParametersType & parameters);

  /** Calculate a fit error between the data and the fit curve. */
  double CalculateFitError(MeasureType *setTestArray);

  /** Given the argument of a Cumulative Gaussian, return its value. */
  double EvaluateCumulativeGaussian(double argument) const;

  /** Get the SpaceDimension. */
  virtual unsigned int GetNumberOfParameters() const ITK_OVERRIDE;

  /** Get the number Range Dimension. */
  virtual unsigned int GetNumberOfValues() const ITK_OVERRIDE;

  /** Initialize the arrays. */
  void Initialize(unsigned int rangeDimension);

  /** Set the original data array. */
  void SetOriginalDataArray(MeasureType *setOriginalDataArray);

protected:
  CumulativeGaussianCostFunction();
  virtual ~CumulativeGaussianCostFunction() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  /** Pointer to the original data array. */
  MeasureType *m_OriginalDataArray;

  /** Number of data samples. */
  unsigned int m_RangeDimension;

  /** Different arrays. */
  mutable MeasureType    m_Measure;
  mutable MeasureType *  m_MeasurePointer;
  mutable ParametersType m_Parameters;
};
} // end namespace itk

#endif
