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
#ifndef itkCenteredAffineTransform_h
#define itkCenteredAffineTransform_h

#include "itkAffineTransform.h"

namespace itk
{
/** \class CenteredAffineTransform
 * \brief Affine transformation with a specified center of rotation.
 *
 * This class implements an Affine transform in which the rotation center
 * can be explicitly selected.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT CenteredAffineTransform : public AffineTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredAffineTransform);

  /** Standard type alias   */
  using Self = CenteredAffineTransform;
  using Superclass = AffineTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CenteredAffineTransform, AffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions * (NDimensions + 2);

  /** Types taken from the Superclass */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;
  using ScalarType = typename Superclass::ScalarType;
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using InputPointType = typename Superclass::InputPointType;
  using InputPointValueType = typename Superclass::InputPointValueType;
  using OutputVectorValueType = typename Superclass::OutputVectorValueType;
  using OutputPointType = typename Superclass::OutputPointType;
  using MatrixType = typename Superclass::MatrixType;
  using MatrixValueType = typename Superclass::MatrixValueType;
  using OffsetType = typename Superclass::OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Set/Get the transformation from a container of parameters.
   * The first (NDimension x NDimension) parameters define the
   * matrix, the next N parameters define the center of rotation
   * and the last N parameters define the translation to be applied
   * after the coordinate system has been restored to the rotation center.
   * Note that the Offset of the superclass is no longer in the
   * parameters array since it is fully dependent on the rotation
   * center and the translation parameters. */
  void
  SetParameters(const ParametersType & parameters) override;

  const ParametersType &
  GetParameters() const override;

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

protected:
  /** Construct an CenteredAffineTransform object */
  CenteredAffineTransform();

  /** Destroy an CenteredAffineTransform object */
  ~CenteredAffineTransform() override = default;
}; // class CenteredAffineTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCenteredAffineTransform.hxx"
#endif

#endif /* itkCenteredAffineTransform_h */
