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
#ifndef itkRigid3DPerspectiveTransform_h
#define itkRigid3DPerspectiveTransform_h

#include "itkMacro.h"
#include "vnl/vnl_quaternion.h"
#include <iostream>
#include "itkTransform.h"
#include "itkVersor.h"

namespace itk
{
/** \brief Rigid3DTramsform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the 3D space
 * followed by a projection to 2D space along the Z axis.
 *
 * \ingroup ITKTransform
 */

template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT Rigid3DPerspectiveTransform : public Transform<TParametersValueType, 3, 2>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Rigid3DPerspectiveTransform);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 2;

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 6;

  /** Standard class type aliases. */
  using Self = Rigid3DPerspectiveTransform;
  using Superclass = Transform<TParametersValueType, Self::InputSpaceDimension, Self::OutputSpaceDimension>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Rigid3DPerspectiveTransform, Transform);

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Parameters type. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename FixedParametersType::ValueType;
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename ParametersType::ValueType;

  /** Jacobian types. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Standard matrix type for this class. */
  using MatrixType = Matrix<TParametersValueType, Self::InputSpaceDimension, Self::InputSpaceDimension>;

  /** Standard vector type for this class. */
  using OffsetType = Vector<TParametersValueType, Self::InputSpaceDimension>;
  using OffsetValueType = typename OffsetType::ValueType;

  /** Standard vector type for this class. */
  using InputVectorType = Vector<TParametersValueType, Self::InputSpaceDimension>;
  using OutputVectorType = Vector<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard covariant vector type for this class */
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  /** Standard coordinate point type for this class. */
  using InputPointType = Point<TParametersValueType, Self::InputSpaceDimension>;
  using OutputPointType = Point<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard vnl_quaternion type. */
  using VnlQuaternionType = vnl_quaternion<TParametersValueType>;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;

  /** Versor type. */
  using VersorType = Versor<TParametersValueType>;
  using AxisType = typename VersorType::VectorType;
  using AngleType = typename VersorType::ValueType;
  using AxisValueType = typename AxisType::ValueType;

  /** Get offset of an Rigid3DPerspectiveTransform
   * This method returns the value of the offset of the
   * Rigid3DPerspectiveTransform. */
  const OffsetType &
  GetOffset() const
  {
    return m_Offset;
  }

  /** Get rotation from an Rigid3DPerspectiveTransform.
   * This method returns the value of the rotation of the
   * Rigid3DPerspectiveTransform. */
  const VersorType &
  GetRotation() const
  {
    return m_Versor;
  }

  /** Set/Get the transformation from a container of parameters.
   * This is typically used by optimizers.
   * There are 6 parameters. The first three represent the
   * versor and the last three represents the offset. */
  void
  SetParameters(const ParametersType & parameters) override;

  const ParametersType &
  GetParameters() const override;

  /** Set the fixed parameters and update internal
   * transformation. This transform has no fixed parameters
   */
  void
  SetFixedParameters(const FixedParametersType &) override
  {}

  /** This method sets the offset of an Rigid3DPerspectiveTransform to a
   * value specified by the user. */
  void
  SetOffset(const OffsetType & offset)
  {
    m_Offset = offset;
    return;
  }

  /** This method sets the rotation of an Rigid3DPerspectiveTransform to a
   * value specified by the user.  */
  void
  SetRotation(const VersorType & rotation);

  /** Set Rotation of the Rigid transform.
   * This method sets the rotation of an Rigid3DTransform to a
   * value specified by the user using the axis of rotation an
   * the angle. */
  void
  SetRotation(const Vector<TParametersValueType, 3> & axis, double angle);

  /** Set the Focal Distance of the projection
   * This method sets the focal distance for the perspective
   * projection to a value specified by the user. */
  void
  SetFocalDistance(TParametersValueType focalDistance)
  {
    m_FocalDistance = focalDistance;
  }

  /** Return the Focal Distance */
  double
  GetFocalDistance() const
  {
    return m_FocalDistance;
  }

  /** Transform by a Rigid3DPerspectiveTransform. This method
   *  applies the transform given by self to a
   *  given point, returning the transformed point. */
  OutputPointType
  TransformPoint(const InputPointType & point) const override;

  /** These vector transforms are not implemented for this transform */
  using Superclass::TransformVector;

  OutputVectorType
  TransformVector(const InputVectorType &) const override
  {
    itkExceptionMacro(<< "TransformVector(const InputVectorType &) is not implemented for Rigid3DPerspectiveTransform");
  }

  OutputVnlVectorType
  TransformVector(const InputVnlVectorType &) const override
  {
    itkExceptionMacro(
      << "TransformVector(const InputVnlVectorType &) is not implemented for Rigid3DPerspectiveTransform");
  }

  using Superclass::TransformCovariantVector;

  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType &) const override
  {
    itkExceptionMacro(<< "TransformCovariantVector(const InputCovariantVectorType &) is not implemented for "
                         "Rigid3DPerspectiveTransform");
  }

  /** Return the rotation matrix */
  const MatrixType &
  GetRotationMatrix() const
  {
    return m_RotationMatrix;
  }

  /** Compute the matrix. */
  void
  ComputeMatrix();

  /** Compute the Jacobian Matrix of the transformation at one point,
   *  allowing for thread-safety. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

  void
  ComputeJacobianWithRespectToPosition(const InputPointType &, JacobianPositionType &) const override
  {
    itkExceptionMacro("ComputeJacobianWithRespectToPosition not yet implemented "
                      "for "
                      << this->GetNameOfClass());
  }
  using Superclass::ComputeJacobianWithRespectToPosition;

  /** Set a fixed offset: this allow to center the object to be transformed */
  itkGetConstReferenceMacro(FixedOffset, OffsetType);
  itkSetMacro(FixedOffset, OffsetType);

  /** Set the center of Rotation */
  itkSetMacro(CenterOfRotation, InputPointType);
  itkGetConstReferenceMacro(CenterOfRotation, InputPointType);

protected:
  Rigid3DPerspectiveTransform();
  ~Rigid3DPerspectiveTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Offset of the transformation. */
  OffsetType m_Offset;

  /** Rotation of the transformation. */
  VersorType m_Versor;

  /** Set Focal distance of the projection. */
  TParametersValueType m_FocalDistance;

  /** Matrix representation of the rotation. */
  MatrixType m_RotationMatrix;

  /** Fixed offset */
  OffsetType m_FixedOffset;

  /** Center of rotation */
  InputPointType m_CenterOfRotation;
}; // class Rigid3DPerspectiveTransform:
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRigid3DPerspectiveTransform.hxx"
#endif

#endif /* itkRigid3DPerspectiveTransform_h */
