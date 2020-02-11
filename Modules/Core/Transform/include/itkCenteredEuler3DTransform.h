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
#ifndef itkCenteredEuler3DTransform_h
#define itkCenteredEuler3DTransform_h

#include <iostream>
#include "itkEuler3DTransform.h"
#include "itkMacro.h"
#include "itkVersor.h"

namespace itk
{
/** \class CenteredEuler3DTransform
 * \brief CenteredEuler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation about a specific coordinate or
 * centre of rotation followed by a translation.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT CenteredEuler3DTransform : public Euler3DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredEuler3DTransform);

  /** Standard class type aliases. */
  using Self = CenteredEuler3DTransform;
  using Superclass = Euler3DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredEuler3DTransform, Euler3DTransform);

  /** Dimension of the space. */
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 9;

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
  using OutputPointType = typename Superclass::OutputPointType;
  using MatrixType = typename Superclass::MatrixType;
  using InverseMatrixType = typename Superclass::InverseMatrixType;
  using CenterType = typename Superclass::CenterType;
  using TranslationType = typename Superclass::TranslationType;
  using TranslationValueType = typename Superclass::TranslationValueType;
  using OffsetType = typename Superclass::OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.  There are nine parameters. The first
   * three represent the angles of rotation (in radians) around each one of the
   * axes (X,Y,Z), the next three parameters represent the coordinates of the
   * center of rotation and the last three parameters represent the
   * translation. */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers. There are nine parameters. The first
   * three represent the angles of rotation (in radians) around each one of the
   * axes (X,Y,Z), the next three parameters represent the coordinates of the
   * center of rotation and the last three parameters represent the
   * translation. */
  const ParametersType &
  GetParameters() const override;

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

protected:
  CenteredEuler3DTransform();
  CenteredEuler3DTransform(const MatrixType & matrix, const OutputPointType & offset);
  CenteredEuler3DTransform(unsigned int ParametersDimension);
  ~CenteredEuler3DTransform() override = default;

  /**
   * Print contents of an CenteredEuler3DTransform
   */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class CenteredEuler3DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCenteredEuler3DTransform.hxx"
#endif

#endif /* itkCenteredEuler3DTransform_h */
