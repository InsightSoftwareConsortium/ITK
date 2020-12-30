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
#ifndef itkFixedCenterOfRotationAffineTransform_h
#define itkFixedCenterOfRotationAffineTransform_h

#include "itkScalableAffineTransform.h"

namespace itk
{
/**
 * \brief Affine transformation with a specified center of rotation.
 *
 * This class implements an Affine transform in which the rotation center can be explicitly selected.
 *
 * \ingroup ITKTransform
 */

template <typename TParametersValueType = double, unsigned int NDimensions = 3>
// Number of dimensions in the input space
class ITK_TEMPLATE_EXPORT FixedCenterOfRotationAffineTransform
  : public ScalableAffineTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FixedCenterOfRotationAffineTransform);

  /** Standard type alias   */
  using Self = FixedCenterOfRotationAffineTransform;
  using Superclass = ScalableAffineTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(FixedCenterOfRotationAffineTransform, ScalableAffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = NDimensions;
  static constexpr unsigned int OutputSpaceDimension = NDimensions;
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions * (NDimensions + 2);

  /** Types taken from the Superclass */
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;
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
  using OffsetType = typename Superclass::OffsetType;

  /** Set and Get the center of rotation */
  void
  SetCenterOfRotationComponent(const InputPointType & cor)
  {
    this->SetCenter(cor);
  }
  InputPointType
  GetCenterOfRotationComponent() const
  {
    return this->GetCenter();
  }

  /** Set the matrix of the transform. The matrix should not include
   *  scale */
  void
  SetMatrixComponent(const MatrixType & matrix)
  {
    this->SetMatrix(matrix);
  }
  /** Get matrix of the transform  */
  const MatrixType &
  GetMatrixComponent() const
  {
    return this->GetMatrix();
  }

  /** Set offset (origin) of the Transform. */
  void
  SetOffsetComponent(const OffsetType & offset)
  {
    this->SetTranslation(offset);
  }

  /** Get offset of the transform. */
  const OffsetType &
  GetOffsetComponent() const
  {
    return this->GetTranslation();
  }

protected:
  /** Construct an FixedCenterOfRotationAffineTransform object */
  FixedCenterOfRotationAffineTransform(const MatrixType & matrix, const OutputVectorType & offset);
  FixedCenterOfRotationAffineTransform(unsigned int outputSpaceDims, unsigned int paramsDims);
  FixedCenterOfRotationAffineTransform();

  /** Destroy an FixedCenterOfRotationAffineTransform object   */
  ~FixedCenterOfRotationAffineTransform() override = default;
}; // class FixedCenterOfRotationAffineTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFixedCenterOfRotationAffineTransform.hxx"
#endif

#endif /* itkFixedCenterOfRotationAffineTransform_h */
