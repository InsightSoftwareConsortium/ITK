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
#ifndef itkRigid3DTransform_h
#define itkRigid3DTransform_h

#include <iostream>
#include "itkMatrixOffsetTransformBase.h"
#include "itkVersor.h"

namespace itk
{
/** \class Rigid3DTransform
 * \brief Rigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation in 3D space.
 * The transform is specified as a rotation matrix around a arbitrary center
 * and is followed by a translation.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 12 elements.
 * The first 9 parameters represents the rotation matrix in row-major order
 * (where the column index varies the fastest). The last 3 parameters defines
 * the translation in each dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation in each dimension.
 *
 * The Rigid3DTransform is intended to be a base class that
 * defines a consistent family of transform types that respect
 * rigid transformations.  Only classes that derive from Rigid3DTransform
 * should be used.
 *
 * \sa Euler3DTransform
 * \sa QuaternionRigidTransform
 * \sa VersorTransform
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT Rigid3DTransform : public MatrixOffsetTransformBase<TParametersValueType, 3, 3>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Rigid3DTransform);

  /** Standard class type aliases. */
  using Self = Rigid3DTransform;
  using Superclass = MatrixOffsetTransformBase<TParametersValueType, 3, 3>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Rigid3DTransform, MatrixOffsetTransformBase);

  /** Dimension of the space. */
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 12;

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
  using OutputVectorValueType = typename Superclass::OutputVectorValueType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using MatrixType = typename Superclass::MatrixType;
  using InverseMatrixType = typename Superclass::InverseMatrixType;
  using MatrixValueType = typename Superclass::MatrixValueType;
  using CenterType = typename Superclass::CenterType;
  using TranslationType = typename Superclass::TranslationType;
  using OffsetType = typename Superclass::OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 12 parameters. The first 9 represents the rotation
   * matrix is row-major order and the last 3 represents the translation.
   *
   * \warning The rotation matrix must be orthogonal to within a specified tolerance,
   * else an exception is thrown.
   *
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Directly set the rotation matrix of the transform.
   * \warning The input matrix must be orthogonal to within a specified tolerance,
   * else an exception is thrown.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  void
  SetMatrix(const MatrixType & matrix) override;

  /** Directly set the rotation matrix of the transform.
   * \warning The input matrix must be orthogonal to within the specified tolerance,
   * else an exception is thrown.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void
  SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance);

  /**
   * Compose the transformation with a translation
   *
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise.
   */
  void
  Translate(const OffsetType & offset, bool pre = false);

  /**
   * Utility function to test if a matrix is orthogonal within a specified
   * tolerance
   */
  bool
  MatrixIsOrthogonal(
    const MatrixType &         matrix,
    const TParametersValueType tolerance = MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance());


protected:
  Rigid3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  Rigid3DTransform(unsigned int paramDim);
  Rigid3DTransform();
  ~Rigid3DTransform() override = default;

  /**
   * Print contents of an Rigid3DTransform
   */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class Rigid3DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRigid3DTransform.hxx"
#endif

#endif /* itkRigid3DTransform_h */
