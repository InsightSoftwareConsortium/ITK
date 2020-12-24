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
#ifndef itkScalableAffineTransform_h
#define itkScalableAffineTransform_h

#include "itkAffineTransform.h"

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
class ITK_TEMPLATE_EXPORT ScalableAffineTransform : public AffineTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalableAffineTransform);

  /** Standard type alias   */
  using Self = ScalableAffineTransform;
  using Superclass = AffineTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(ScalableAffineTransform, AffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = NDimensions;
  static constexpr unsigned int OutputSpaceDimension = NDimensions;
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions * (NDimensions + 1);

  /** Types taken from the Superclass */
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
  using MatrixValueType = typename Superclass::MatrixValueType;
  using InverseMatrixType = typename Superclass::InverseMatrixType;
  using CenterType = typename Superclass::CenterType;
  using OffsetType = typename Superclass::OffsetType;
  using TranslationType = typename Superclass::TranslationType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.  */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Set the transformation to an Identity
   *
   * This sets the matrix to identity and the Offset to null. */
  void
  SetIdentity() override;

  /** Set the scale of the transform */
  virtual void
  SetScale(const InputVectorType & scale);

  virtual void
  SetScaleComponent(const InputVectorType & scale)
  {
    this->SetScale(scale);
  }

  /** Set the scale of the transform */
  virtual void
  SetScale(const double scale[NDimensions]);

  virtual void
  SetScaleComponent(const double scale[NDimensions])
  {
    this->SetScale(scale);
  }

  /** Get the scale of the transform */
  virtual const double *
  GetScale() const
  {
    return m_Scale;
  }
  virtual const double *
  GetScaleComponent() const
  {
    return m_Scale;
  }

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

protected:
  /** Construct an ScalableAffineTransform object
   *
   * This method constructs a new AffineTransform object and
   * initializes the matrix and offset parts of the transformation
   * to values specified by the caller.  If the arguments are
   * omitted, then the AffineTransform is initialized to an identity
   * transformation in the appropriate number of dimensions. */
  ScalableAffineTransform(const MatrixType & matrix, const OutputVectorType & offset);
  ScalableAffineTransform(unsigned int outputSpaceDimension, unsigned int parametersDimension);
  ScalableAffineTransform(unsigned int parametersDimension);
  ScalableAffineTransform();

  void
  ComputeMatrix() override;

  /** Destroy an ScalableAffineTransform object   */
  ~ScalableAffineTransform() override = default;

  /** Print contents of an ScalableAffineTransform */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  SetVarScale(const double * scale)
  {
    for (int i = 0; i < InputSpaceDimension; i++)
    {
      m_Scale[i] = scale[i];
    }
  }

private:
  double          m_Scale[NDimensions];
  InputVectorType m_MatrixScale;
}; // class ScalableAffineTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalableAffineTransform.hxx"
#endif

#endif /* itkScalableAffineTransform_h */
