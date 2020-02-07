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
#ifndef itkCenteredSimilarity2DTransform_h
#define itkCenteredSimilarity2DTransform_h

#include "itkSimilarity2DTransform.h"

namespace itk
{
/** \class CenteredSimilarity2DTransform
 *  \brief CenteredSimilarity2DTransform of a vector space
 *        (e.g. space coordinates)
 *
 * This transform applies a homogeneous scale and rigid transform in
 * 2D space. The transform is specified as a scale and rotation around
 * a arbitrary center and is followed by a translation.
 * given one angle for rotation, a homogeneous scale and a 2D offset
 * for translation.
 *
 * The main difference between this class and its superclass
 * Similarity2DTransform is that the center of transformation is exposed
 * for optimization.
 *
 * The serialization of the optimizable parameters is an array of 6 elements
 * ordered as follows:
 * p[0] = scale
 * p[1] = angle
 * p[2] = x coordinate of the center
 * p[3] = y coordinate of the center
 * p[4] = x component of the translation
 * p[5] = y component of the translation
 *
 * There are no fixed parameters.
 *
 * \sa Similarity2DTransform
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT CenteredSimilarity2DTransform : public Similarity2DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredSimilarity2DTransform);

  /** Standard class type aliases. */
  using Self = CenteredSimilarity2DTransform;
  using Superclass = Similarity2DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredSimilarity2DTransform, Similarity2DTransform);

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = 2;
  static constexpr unsigned int InputSpaceDimension = 2;
  static constexpr unsigned int OutputSpaceDimension = 2;
  static constexpr unsigned int ParametersDimension = 6;

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Parameters type. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;

  /** Jacobian type. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Offset type. */
  using OffsetType = typename Superclass::OffsetType;
  using OffsetValueType = typename Superclass::OffsetValueType;

  /** Point type. */
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using InputPointValueType = typename InputPointType::ValueType;

  /** Vector type. */
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;

  /** CovariantVector type. */
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  /** VnlVector type. */
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 6 parameters. The first one represents the
   * scale, the second represents the angle of rotation, the next
   * two represent the center of the rotation
   * and the last two represent the translation.
   *
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 6 parameters. The first one represents the
   * scale, the second represents the angle of rotation, the next
   * two represent the center of the rotation
   * and the last two represent the translation.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  const ParametersType &
  GetParameters() const override;

  /** Compute the Jacobian Matrix of the transformation at one point */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

  /** Set the fixed parameters and update internal transformation.
   * This is a null function as there are no fixed parameters. */
  void
  SetFixedParameters(const FixedParametersType &) override;

  /** Get the Fixed Parameters. An empty array is returned
   * as there are no fixed parameters. */
  const FixedParametersType &
  GetFixedParameters() const override;

  /**
   * This method creates and returns a new Rigid2DTransform object
   * which is the inverse of self. */
  void
  CloneInverseTo(Pointer & newinverse) const;

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /**
   * This method creates and returns a new Rigid2DTransform object
   * which has the same parameters. */
  void
  CloneTo(Pointer & clone) const;

protected:
  CenteredSimilarity2DTransform();
  CenteredSimilarity2DTransform(unsigned int spaceDimension, unsigned int parametersDimension);

  ~CenteredSimilarity2DTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCenteredSimilarity2DTransform.hxx"
#endif

#endif /* itkCenteredSimilarity2DTransform_h */
