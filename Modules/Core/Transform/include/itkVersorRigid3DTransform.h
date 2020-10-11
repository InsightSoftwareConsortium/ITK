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
#ifndef itkVersorRigid3DTransform_h
#define itkVersorRigid3DTransform_h

#include <iostream>
#include "itkVersorTransform.h"

namespace itk
{
/** \class VersorRigid3DTransform
 *
 * \brief VersorRigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 6 elements.
 * The first 3 elements are the components of the versor representation
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
class ITK_TEMPLATE_EXPORT VersorRigid3DTransform : public VersorTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VersorRigid3DTransform);

  /** Standard class type aliases. */
  using Self = VersorRigid3DTransform;
  using Superclass = VersorTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VersorRigid3DTransform, VersorTransform);

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 6;

  /** Parameters Type   */
  using ScalarType = typename Superclass::ScalarType;
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;
  using MatrixType = typename Superclass::MatrixType;
  using InverseMatrixType = typename Superclass::InverseMatrixType;
  using CenterType = typename Superclass::CenterType;
  using OffsetType = typename Superclass::OffsetType;
  using TranslationType = typename Superclass::TranslationType;

  /** Versor type. */
  using VersorType = typename Superclass::VersorType;
  using VectorType = typename VersorType::VectorType;

  using AxisType = typename Superclass::AxisType;
  using AngleType = typename Superclass::AngleType;

  using AxisValueType = typename Superclass::AxisValueType;
  using TranslationValueType = typename Superclass::TranslationValueType;
  using ParametersValueType = typename Superclass::ParametersValueType;

  using DerivativeType = Array<ParametersValueType>;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 6 parameters. The first three represent the
   * versor, the last three represent the translation. */
  void
  SetParameters(const ParametersType & parameters) override;

  const ParametersType &
  GetParameters() const override;

  /** Update the transform's parameters by the values in \c update.
   * \param update must be of the same length as returned by
   * GetNumberOfParameters(). Throw an exception otherwise.
   * \param factor is a scalar multiplier for each value in \c update.
   * SetParameters is called at the end of this method, to allow the transform
   * to perform any required operations on the updated parameters - typically
   * a conversion to member variables for use in TransformPoint. */
  void
  UpdateTransformParameters(const DerivativeType & update, TParametersValueType factor = 1.0) override;

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

protected:
  VersorRigid3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  VersorRigid3DTransform(unsigned int paramDim);
  VersorRigid3DTransform();
  ~VersorRigid3DTransform() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class VersorRigid3DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVersorRigid3DTransform.hxx"
#endif

#endif /* itkVersorRigid3DTransform_h */
