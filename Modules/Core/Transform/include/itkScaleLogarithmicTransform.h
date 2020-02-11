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
#ifndef itkScaleLogarithmicTransform_h
#define itkScaleLogarithmicTransform_h

#include "itkScaleTransform.h"

namespace itk
{
/** \class ScaleLogarithmicTransform
 * \brief Logarithmic Scale transformation of a vector space (e.g. space coordinates)
 *
 * The only difference between this class and its superclass the ScaleTransform
 * is that here the parameters of the transformation are the logarithms of the
 * scales. This facilitates to linearize the expressions used for optimization.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = float, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT ScaleLogarithmicTransform : public ScaleTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScaleLogarithmicTransform);

  /** Standard class type aliases.   */
  using Self = ScaleLogarithmicTransform;
  using Superclass = ScaleTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a smart pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScaleLogarithmicTransform, ScaleTransform);

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions;

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Parameters type. */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename ParametersType::ValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename FixedParametersType::ValueType;

  /** Jacobian type. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Standard vector type for this class. */
  using ScaleType = typename Superclass::ScaleType;
  using ScalesValueType = typename ScaleType::ValueType;

  /** Standard vector type for this class. */
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;

  /** Standard covariant vector type for this class. */
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  /** Set parameters.
   * This method sets the parameters for the transform
   * value specified by the user. */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 4 parameters. The first one represents the
   * rotation, the second one the scale and the last
   * two represent the offset. */
  const ParametersType &
  GetParameters() const override;

  /** Compute the Jacobian Matrix of the transformation at one point,
   *  allowing for thread-safety. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

protected:
  /** Construct an ScaleLogarithmicTransform object. */
  ScaleLogarithmicTransform() = default;

  /** Destroy an ScaleLogarithmicTransform object. */
  ~ScaleLogarithmicTransform() override = default;

  /** Print contents of an ScaleLogarithmicTransform */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class
   // ScaleLogarithmicTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScaleLogarithmicTransform.hxx"
#endif

#endif /* itkScaleLogarithmicTransform_h */
