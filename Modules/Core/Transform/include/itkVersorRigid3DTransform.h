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
  using typename Superclass::ScalarType;
  using typename Superclass::ParametersType;
  using typename Superclass::FixedParametersType;
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;
  using typename Superclass::InputPointType;
  using typename Superclass::OutputPointType;
  using typename Superclass::InputVectorType;
  using typename Superclass::OutputVectorType;
  using typename Superclass::InputVnlVectorType;
  using typename Superclass::OutputVnlVectorType;
  using typename Superclass::InputCovariantVectorType;
  using typename Superclass::OutputCovariantVectorType;
  using typename Superclass::MatrixType;
  using typename Superclass::InverseMatrixType;
  using typename Superclass::CenterType;
  using typename Superclass::OffsetType;
  using typename Superclass::TranslationType;

  /** Versor type. */
  using typename Superclass::VersorType;
  using VectorType = typename VersorType::VectorType;

  using typename Superclass::AxisType;
  using typename Superclass::AngleType;

  using typename Superclass::AxisValueType;
  using typename Superclass::TranslationValueType;
  using typename Superclass::ParametersValueType;

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
