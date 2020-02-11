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
#ifndef itkGaussianDerivativeSpatialFunction_h
#define itkGaussianDerivativeSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class GaussianDerivativeSpatialFunction
 * \brief N-dimensional Gaussian spatial function class
 *
 * GaussianDerivativeSpatialFunction implements a standard derivative of Gaussian
 * curve in N-d.
 * m_Normalized determines whether or not the Derivative of the Gaussian
 * is normalized (whether or not the sum over infinite space is 1.0).
 *
 * m_Scale scales the output of the Gaussian to span a range
 * larger than 0->1, and is often set to the maximum value
 * of the output data type (for instance, 255 for uchars).
 *
 * \ingroup SpatialFunctions
 * \ingroup ITKCommon
 */
template <typename TOutput = double, unsigned int VImageDimension = 3, typename TInput = Point<double, VImageDimension>>
class ITK_TEMPLATE_EXPORT GaussianDerivativeSpatialFunction : public SpatialFunction<TOutput, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianDerivativeSpatialFunction);

  /** Standard class type aliases. */
  using Self = GaussianDerivativeSpatialFunction;
  using Superclass = SpatialFunction<TOutput, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianDerivativeSpatialFunction, SpatialFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using OutputType = typename Superclass::OutputType;

  /** Type used to store derivatives parameters. */
  using ArrayType = FixedArray<double, VImageDimension>;

  /** Type used to return the derivatives in each direction */
  using VectorType = Vector<double, VImageDimension>;

  /** Evaluate the function at a given position and return the
   *  value in the specific direction. SetDirection() should be used
   *  to set the direction. */
  OutputType
  Evaluate(const TInput & position) const override;

  /** Evaluate the function at a given position and return a vector */
  VectorType
  EvaluateVector(const TInput & position) const;

  /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetConstMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetConstMacro(Normalized, bool);
  itkSetMacro(Sigma, ArrayType);
  itkGetConstMacro(Sigma, ArrayType);
  itkSetMacro(Mean, ArrayType);
  itkGetConstMacro(Mean, ArrayType);
  itkSetMacro(Direction, unsigned int);
  itkGetConstMacro(Direction, unsigned int);

protected:
  GaussianDerivativeSpatialFunction();
  ~GaussianDerivativeSpatialFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Current direction */
  mutable unsigned int m_Direction;

  /** The standard deviation in each direction. */
  ArrayType m_Sigma;

  /** The mean in each direction. */
  ArrayType m_Mean;

  /** A scale factor multiplied by the true value of the Gaussian. */
  double m_Scale;

  /** Whether or not to normalize the Gaussian. */
  bool m_Normalized;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianDerivativeSpatialFunction.hxx"
#endif

#endif
