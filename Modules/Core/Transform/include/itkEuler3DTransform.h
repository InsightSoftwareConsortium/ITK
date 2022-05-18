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
#ifndef itkEuler3DTransform_h
#define itkEuler3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"

namespace itk
{
/** \class Euler3DTransform
 *
 * \brief Euler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space given 3 euler
 * angles and a 3D translation. Rotation is about a user specified center.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 6 elements.
 * The first 3 represents three euler angle of rotation respectively about
 * the X, Y and Z axis. The last 3 parameters defines the translation in each
 * dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT Euler3DTransform : public Rigid3DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Euler3DTransform);

  /** Standard class type aliases. */
  using Self = Euler3DTransform;
  using Superclass = Rigid3DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Euler3DTransform, Rigid3DTransform);

  /** Dimension of the space. */
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 6;

  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;
  using typename Superclass::FixedParametersType;
  using typename Superclass::FixedParametersValueType;
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;
  using typename Superclass::ScalarType;
  using typename Superclass::InputVectorType;
  using typename Superclass::OutputVectorType;
  using typename Superclass::InputCovariantVectorType;
  using typename Superclass::OutputCovariantVectorType;
  using typename Superclass::InputVnlVectorType;
  using typename Superclass::OutputVnlVectorType;
  using typename Superclass::InputPointType;
  using typename Superclass::OutputPointType;
  using typename Superclass::MatrixType;
  using typename Superclass::InverseMatrixType;
  using typename Superclass::CenterType;
  using typename Superclass::TranslationType;
  using typename Superclass::OffsetType;
  using AngleType = typename Superclass::ScalarType;

  /** Set/Get the transformation from a container of parameters
   * This is typically used by optimizers.  There are 6 parameters. The first
   * three represent the angles to rotate around the coordinate axis, and the
   * last three represents the offset. */
  void
  SetParameters(const ParametersType & parameters) override;

  const ParametersType &
  GetParameters() const override;

  const FixedParametersType &
  GetFixedParameters() const override;
  void
  SetFixedParameters(const FixedParametersType & parameters) override;

  /** Set the rotational part of the transform. */
  void
  SetRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ);

  itkGetConstMacro(AngleX, ScalarType);
  itkGetConstMacro(AngleY, ScalarType);
  itkGetConstMacro(AngleZ, ScalarType);

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;
  using Superclass::ComputeJacobianWithRespectToPosition;

  /** The Euler angle representation of a rotation is not unique and
   * depends on the order of rotations. In general there are 12
   * options. This class supports two of them, ZXY and ZYX. The
   * default is ZXY. These functions set and get the value which
   * indicates whether the rotation is ZYX or ZXY.
   */
  virtual void
  SetComputeZYX(const bool flag);
  itkGetConstMacro(ComputeZYX, bool);

  void
  SetIdentity() override;

protected:
  Euler3DTransform(const MatrixType & matrix, const OutputPointType & offset);
  Euler3DTransform(unsigned int parametersDimension);
  Euler3DTransform();

  ~Euler3DTransform() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Set values of angles directly without recomputing other parameters. */
  void
  SetVarRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ);

  /** Compute the components of the rotation matrix in the superclass. */
  void
  ComputeMatrix() override;

  void
  ComputeMatrixParameters() override;

private:
  ScalarType m_AngleX;
  ScalarType m_AngleY;
  ScalarType m_AngleZ;
  bool       m_ComputeZYX;
}; // class Euler3DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEuler3DTransform.hxx"
#endif

#endif /* itkEuler3DTransform_h */
