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
#ifndef itkScaleSkewVersor3DTransform_h
#define itkScaleSkewVersor3DTransform_h

#include <iostream>
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class ScaleSkewVersor3DTransform
 * \brief ScaleSkewVersor3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a versor rotation and translation & scale/skew
 * to the space
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 15 elements.
 * The first 3 elements are the components of the versor representation
 * of 3D rotation. The next 3 parameters defines the translation in each
 * dimension. The next 3 parameters defines scaling in each dimension.
 * The last 6 parameters defines the skew.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *The transform can be described as:
 * \f$ (\textbf{R}_v + \textbf{S} + \textbf{K})\textbf{x}  \f$
 * where \f$\textbf{R}_v\f$ is the rotation matrix given the versor,
 * \f$S=\left( \begin{array}{ccc}s_0-1 & 0 & 0 \\ 0 & s_1-1 & 0 \\ 0 & 0 & s_2-1 \end{array} \right) \f$
 * , and
 * \f$K=\left( \begin{array}{ccc}0 & k_0 & k_1 \\ k_2 & 0 & k_3 \\ k_4 & k_5 & 0 \end{array} \right)\ \f$.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT ScaleSkewVersor3DTransform : public VersorRigid3DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScaleSkewVersor3DTransform);

  /** Standard class type aliases. */
  using Self = ScaleSkewVersor3DTransform;
  using Superclass = VersorRigid3DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScaleSkewVersor3DTransform, VersorRigid3DTransform);

  /** Dimension of parameters. */
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 15;

  /** Parameters Type   */
  using typename Superclass::ParametersType;
  using typename Superclass::FixedParametersType;
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;
  using typename Superclass::ScalarType;
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

  using typename Superclass::VersorType;
  using typename Superclass::AxisType;
  using typename Superclass::AngleType;

  /** Scale & Skew Vector Type. */
  using ScaleVectorType = Vector<TParametersValueType, 3>;
  using SkewVectorType = Vector<TParametersValueType, 6>;

  using ScaleVectorValueType = typename ScaleVectorType::ValueType;
  using SkewVectorValueType = typename SkewVectorType::ValueType;
  using TranslationValueType = typename TranslationType::ValueType;

  using typename Superclass::AxisValueType;
  using typename Superclass::ParametersValueType;

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
   * There are 15 parameters:
   *   0-2   versor
   *   3-5   translation
   *   6-8   Scale
   *   9-14  Skew
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

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const override;

protected:
  ScaleSkewVersor3DTransform();
  ScaleSkewVersor3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  ScaleSkewVersor3DTransform(unsigned int parametersDimension);
  ~ScaleSkewVersor3DTransform() override = default;

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
}; // class ScaleSkewVersor3DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScaleSkewVersor3DTransform.hxx"
#endif

#endif /* __ScaleSkewVersor3DTransform_h */
