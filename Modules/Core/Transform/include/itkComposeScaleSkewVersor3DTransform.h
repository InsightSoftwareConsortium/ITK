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
#ifndef itkComposeScaleSkewVersor3DTransform_h
#define itkComposeScaleSkewVersor3DTransform_h

#include <iostream>
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class ComposeScaleSkewVersor3DTransform
 * \brief ComposeScaleSkewVersor3DTransform of a vector space (space coords)
 *
 * This transform applies a versor rotation and translation & scale/skew
 * to the space
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 12 elements.
 * The first 3 elements are the components of the versor representation
 * of 3D rotation. The next 3 parameters defines the translation in each
 * dimension. The next 3 parameters defines scaling in each dimension.
 * The last 3 parameters defines the skew.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *The transform can be described as:
 * \f$ (\textbf{R}_v * \textbf{S} * \textbf{K})\textbf{x}  \f$
 * where \f$\textbf{R}_v\f$ is the rotation matrix given the versor,
 * where \f$\textbf{S}\f$ is the diagonal scale matrix.
 * where \f$\textbf{K}\f$ is the upper triangle skew (shear) matrix.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT ComposeScaleSkewVersor3DTransform : public VersorRigid3DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ComposeScaleSkewVersor3DTransform);

  /** Standard class type aliases. */
  using Self = ComposeScaleSkewVersor3DTransform;
  using Superclass = VersorRigid3DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComposeScaleSkewVersor3DTransform, VersorRigid3DTransform);

  /** Dimension of parameters. */
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 12;

  /** Parameters Type   */
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;
  using ScalarType = typename Superclass::ScalarType;
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

  using VersorType = typename Superclass::VersorType;
  using AxisType = typename Superclass::AxisType;
  using AngleType = typename Superclass::AngleType;

  /** Scale & Skew Vector Type. */
  using ScaleVectorType = Vector<TParametersValueType, 3>;
  using SkewVectorType = Vector<TParametersValueType, 3>;

  using ScaleVectorValueType = typename ScaleVectorType::ValueType;
  using SkewVectorValueType = typename SkewVectorType::ValueType;
  using TranslationValueType = typename TranslationType::ValueType;

  using AxisValueType = typename Superclass::AxisValueType;
  using ParametersValueType = typename Superclass::ParametersValueType;

  /** Directly set the matrix of the transform.
   *
   * Orthogonality testing is bypassed in this case.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  void
  SetMatrix(const MatrixType & matrix) override;
  void
  SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance) override;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 12 parameters:
   *   0-2   versor (right part)
   *   3-5   translation
   *   6-8   Scale
   *   9-11  Skew
   **  */
  void
  SetParameters(const ParametersType & parameters) override;

  const ParametersType &
  GetParameters() const override;

  void
  SetScale(const ScaleVectorType & scale);

  itkGetConstReferenceMacro(Scale, ScaleVectorType);

  void
  SetSkew(const SkewVectorType & skew);

  itkGetConstReferenceMacro(Skew, SkewVectorType);

  void
  SetIdentity() override;

  /* This function is not implemented for this transform.  An exception
   *   is thrown if this function is called. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

protected:
  ComposeScaleSkewVersor3DTransform();
  ComposeScaleSkewVersor3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  ComposeScaleSkewVersor3DTransform(unsigned int parametersDimension);
  ~ComposeScaleSkewVersor3DTransform() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  SetVarScale(const ScaleVectorType & scale)
  {
    m_Scale = scale;
  }

  void
  SetVarSkew(const SkewVectorType & skew)
  {
    m_Skew = skew;
  }

  /** Compute the components of the rotation matrix in the superclass. */
  void
  ComputeMatrix() override;

  void
  ComputeMatrixParameters() override;

private:
  /**  Vector containing the scale. */
  ScaleVectorType m_Scale;

  /**  Vector containing the skew */
  SkewVectorType m_Skew;
}; // class ComposeScaleSkewVersor3DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkComposeScaleSkewVersor3DTransform.hxx"
#endif

#endif /* __ComposeScaleSkewVersor3DTransform_h */
