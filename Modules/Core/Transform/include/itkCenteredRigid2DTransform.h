/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkCenteredRigid2DTransform_h
#define itkCenteredRigid2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{
/** \class CenteredRigid2DTransform
 * \brief CenteredRigid2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rigid transformation is 2D space.
 * The transform is specified as a rotation around arbitrary center
 * and is followed by a translation.
 *
 * The main difference between this class and its superclass
 * Rigid2DTransform is that the center of rotation is exposed
 * for optimization.
 *
 * The serialization of the optimizable parameters is an array of 5 elements
 * ordered as follows:
 * p[0] = angle
 * p[1] = x coordinate of the center
 * p[2] = y coordinate of the center
 * p[3] = x component of the translation
 * p[4] = y component of the translation
 *
 * There are no fixed parameters.
 *
 * \sa Rigid2DTransform
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT CenteredRigid2DTransform : public Rigid2DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CenteredRigid2DTransform);

  /** Standard class type aliases. */
  using Self = CenteredRigid2DTransform;
  using Superclass = Rigid2DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredRigid2DTransform, Rigid2DTransform);

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = 2;
  static constexpr unsigned int OutputSpaceDimension = 2;
  static constexpr unsigned int ParametersDimension = 5;

  /** Parameters type. */
  using typename Superclass::ScalarType;
  using typename Superclass::FixedParametersType;
  using typename Superclass::FixedParametersValueType;
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;

  /** Jacobian type. */
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;

  /** Offset type. */
  using typename Superclass::OffsetType;

  /** Point type. */
  using typename Superclass::InputPointType;
  using typename Superclass::OutputPointType;
  using typename Superclass::InputPointValueType;

  /** Vector type. */
  using typename Superclass::InputVectorType;
  using typename Superclass::OutputVectorType;
  using typename Superclass::OutputVectorValueType;

  /** CovariantVector type. */
  using typename Superclass::InputCovariantVectorType;
  using typename Superclass::OutputCovariantVectorType;

  /** VnlVector type. */
  using typename Superclass::InputVnlVectorType;
  using typename Superclass::OutputVnlVectorType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 5 parameters. The first one represents the
   * rotation, the next two the center of rotation and
   * the last two represents the offset.
   *
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 3 parameters. The first one represents the
   * rotation, the next two the center of rotation and
   * the last two represents the offset.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  const ParametersType &
  GetParameters() const override;

  /** This method computes the Jacobian matrix of the transformation
   * at a given input point.
   */
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
   * This method creates and returns a new CenteredRigid2DTransform object
   * which is the inverse of self. */
  void
  CloneInverseTo(Pointer & result) const;

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /**
   * This method creates and returns a new CenteredRigid2DTransform object
   * which has the same parameters as self. */
  void
  CloneTo(Pointer & result) const;

protected:
  CenteredRigid2DTransform();
  ~CenteredRigid2DTransform() override = default;

  CenteredRigid2DTransform(unsigned int spaceDimension, unsigned int parametersDimension);

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class CenteredRigid2DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCenteredRigid2DTransform.hxx"
#endif

#endif /* itkCenteredRigid2DTransform_h */
