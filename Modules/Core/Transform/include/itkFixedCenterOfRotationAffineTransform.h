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

template <typename TParametersValueType = double, unsigned int VDimension = 3>
// Number of dimensions in the input space
class ITK_TEMPLATE_EXPORT FixedCenterOfRotationAffineTransform
  : public ScalableAffineTransform<TParametersValueType, VDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FixedCenterOfRotationAffineTransform);

  /** Standard type alias   */
  using Self = FixedCenterOfRotationAffineTransform;
  using Superclass = ScalableAffineTransform<TParametersValueType, VDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(FixedCenterOfRotationAffineTransform, ScalableAffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = VDimension;
  static constexpr unsigned int OutputSpaceDimension = VDimension;
  static constexpr unsigned int SpaceDimension = VDimension;
  static constexpr unsigned int ParametersDimension = VDimension * (VDimension + 2);

  /** Types taken from the Superclass */
  using typename Superclass::ParametersType;
  using typename Superclass::FixedParametersType;
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
