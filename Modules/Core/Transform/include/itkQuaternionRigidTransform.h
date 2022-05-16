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
#ifndef itkQuaternionRigidTransform_h
#define itkQuaternionRigidTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "vnl/vnl_quaternion.h"

namespace itk
{
/** \class QuaternionRigidTransform
 * \brief QuaternionRigidTransform of a vector space (e.g. space coordinates).
 *
 * This transform applies a rotation and translation to the space given
 * a quaternion and a 3D translation. Rotation is about a user specified center.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 7 elements.
 * The first 4 elements are the components of the quaternion representation
 * of 3D rotation. The last 3 parameters defines the translation in each
 * dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT QuaternionRigidTransform : public Rigid3DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuaternionRigidTransform);

  /** Standard class type aliases.   */
  using Self = QuaternionRigidTransform;
  using Superclass = Rigid3DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuaternionRigidTransform, Rigid3DTransform);

  /** Dimension of parameters   */
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 7;

  /** Parameters Type   */
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;
  using typename Superclass::FixedParametersType;
  using typename Superclass::FixedParametersValueType;
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;
  using typename Superclass::ScalarType;
  using typename Superclass::InputPointType;
  using typename Superclass::OutputPointType;
  using typename Superclass::InputVectorType;
  using typename Superclass::OutputVectorType;
  using typename Superclass::OutputVectorValueType;
  using typename Superclass::InputVnlVectorType;
  using typename Superclass::OutputVnlVectorType;
  using typename Superclass::InputCovariantVectorType;
  using typename Superclass::OutputCovariantVectorType;
  using typename Superclass::MatrixType;
  using typename Superclass::InverseMatrixType;
  using typename Superclass::CenterType;
  using typename Superclass::OffsetType;
  using typename Superclass::TranslationType;

  /** VnlQuaternion type.  */
  using VnlQuaternionType = vnl_quaternion<TParametersValueType>;

  /** Compute the Jacobian Matrix of the transformation at one point */
  /** Set the rotation of the rigid transform.
   * This method sets the rotation of a QuaternionRigidTransform to a
   * value specified by the user. */
  void
  SetRotation(const VnlQuaternionType & rotation);

  /** Get the rotation from an QuaternionRigidTransform.
   * This method returns the value of the rotation of the
   * QuaternionRigidTransform. */
  const VnlQuaternionType &
  GetRotation() const
  {
    return m_Rotation;
  }

  /** Set the parameters to the IdentityTransform */
  void
  SetIdentity() override;

  /** Set the transformation from a container of parameters.
   * This is typically used by optimizers.
   * There are 7 parameters. The first four represents the
   * quaternion and the last three represents the
   * offset. */
  void
  SetParameters(const ParametersType & parameters) override;

  const ParametersType &
  GetParameters() const override;

  /** Compute the Jacobian of the transformation.
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

protected:
  QuaternionRigidTransform(const MatrixType & matrix, const OutputVectorType & offset);
  QuaternionRigidTransform(unsigned int parametersDimension);
  QuaternionRigidTransform();
  ~QuaternionRigidTransform() override = default;

  void
  ComputeMatrix() override;

  void
  ComputeMatrixParameters() override;

  void
  SetVarRotation(const VnlQuaternionType & rotation)
  {
    m_Rotation = rotation;
  }

  const InverseMatrixType &
  GetInverseMatrix() const;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Rotation of the transformation. */
  VnlQuaternionType m_Rotation;
}; // class QuaternionRigidTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuaternionRigidTransform.hxx"
#endif

#endif /* itkQuaternionRigidTransform_h */
