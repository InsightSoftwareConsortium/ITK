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
#ifndef itkScaleTransform_h
#define itkScaleTransform_h

#include "itkMatrixOffsetTransformBase.h"
#include "itkMacro.h"
#include "itkMatrix.h"

namespace itk
{
/** \class ScaleTransform
 * \brief Scale transformation of a vector space (e.g. space coordinates)
 *
 * The same functionality could be obtained by using the Affine transform,
 * but with a large difference in performance since the affine transform will
 * use a matrix multiplication using a diagonal matrix.
 *
 * \ingroup ITKTransform
 *
 * \sphinx
 * \sphinxexample{Core/Transform/ScaleAnImage,Scale An Image}
 * \endsphinx
 */
template <typename TParametersValueType = float, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT ScaleTransform
  : public MatrixOffsetTransformBase<TParametersValueType, NDimensions, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScaleTransform);

  /** Standard class type aliases.   */
  using Self = ScaleTransform;
  using Superclass = MatrixOffsetTransformBase<TParametersValueType, NDimensions, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a smart pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScaleTransform, Transform);

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions;

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Parameters type. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using ParametersType = typename Superclass::ParametersType;

  /** Jacobian types. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Standard vector type for this class. */
  using ScaleType = FixedArray<TParametersValueType, NDimensions>;

  /** Standard vector type for this class. */
  using InputVectorType = Vector<TParametersValueType, NDimensions>;
  using OutputVectorType = Vector<TParametersValueType, NDimensions>;

  /** Standard covariant vector type for this class. */
  using InputCovariantVectorType = CovariantVector<TParametersValueType, NDimensions>;
  using OutputCovariantVectorType = CovariantVector<TParametersValueType, NDimensions>;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = vnl_vector_fixed<TParametersValueType, NDimensions>;
  using OutputVnlVectorType = vnl_vector_fixed<TParametersValueType, NDimensions>;

  /** Standard coordinate point type for this class. */
  using InputPointType = Point<TParametersValueType, NDimensions>;
  using OutputPointType = Point<TParametersValueType, NDimensions>;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  using MatrixType = typename Superclass::MatrixType;

  /** Set parameters.  This method sets the parameters for the transform value
   *  specified by the user. The parameters are organized as scale[i] =
   *  parameter[i]. That means that in 3D the scale parameters for the coordinates
   *  {x,y,z} are {parameter[0], parameter[1], parameter[2]} respectively */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Get the parameters that uniquely define the transform This is typically
   * used by optimizers during the process of image registration.  The parameters
   * are organized as {scale X, scale Y, scale Z } = { parameter[0],
   * parameter[1], parameter[2] } respectively */
  const ParametersType &
  GetParameters() const override;

  /** Get the Jacobian matrix. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & j) const override;

  /** Get the jacobian with respect to position, which simply is the
   *  matrix because the transform is position-invariant.
   *  jac will be resized as needed, but it will be more efficient if
   *  it is already properly sized. */
  void
  ComputeJacobianWithRespectToPosition(const InputPointType & x, JacobianPositionType & jac) const override;
  using Superclass::ComputeJacobianWithRespectToPosition;

  /** Set the factors of an Scale Transform
   * This method sets the factors of an ScaleTransform to a
   * value specified by the user.
   * This method cannot be done with SetMacro because itk::Array has not an
   * operator== defined. The array of scales correspond in order to the factors
   * to be applied to each one of the coordinates. For example, in 3D,
   * scale[0] corresponds to X, scale[1] corresponds to Y and scale[2]
   * corresponds to Z. */
  void
  SetScale(const ScaleType & scale);

  void
  ComputeMatrix() override;

  /** Compose with another ScaleTransform. */
  void
  Compose(const Self * other, bool pre = false);

  /** Compose this transform transformation with another scaling.
   * The pre argument is irrelevant here since scale transforms are commutative,
   * pre and postcomposition are therefore equivalent. */
  void
  Scale(const ScaleType & scale, bool pre = false);

  /** Transform by a scale transformation
   * This method applies the scale transform given by self to a
   * given point or vector, returning the transformed point or
   * vector. */
  OutputPointType
  TransformPoint(const InputPointType & point) const override;

  using Superclass::TransformVector;
  OutputVectorType
  TransformVector(const InputVectorType & vect) const override;

  OutputVnlVectorType
  TransformVector(const InputVnlVectorType & vect) const override;

  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & vect) const override;

  /** Back transform by a scale transformation
   * This method finds the point or vector that maps to a given
   * point or vector under the scale transformation defined by
   * self.  If no such point exists, an exception is thrown. */
  inline InputPointType
  BackTransform(const OutputPointType & point) const;

  inline InputVectorType
  BackTransform(const OutputVectorType & vect) const;

  inline InputVnlVectorType
  BackTransform(const OutputVnlVectorType & vect) const;

  inline InputCovariantVectorType
  BackTransform(const OutputCovariantVectorType & vect) const;

  /** Find inverse of a scale transformation
   * This method creates and returns a new ScaleTransform object
   * which is the inverse of self.  If self is not invertible,
   * false is returned. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /** Set the transformation to an Identity
   *
   * This sets all the scales to 1.0 */
  void
  SetIdentity() override;

  /** Get access to scale values */
  itkGetConstReferenceMacro(Scale, ScaleType);

protected:
  /** Construct an ScaleTransform object. */
  ScaleTransform();

  /** Destroy an ScaleTransform object. */
  ~ScaleTransform() override = default;

  /** Print contents of an ScaleTransform */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ScaleType m_Scale; // Scales of the transformation

}; // class ScaleTransform

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScaleTransform.hxx"
#endif

#endif /* itkScaleTransform_h */
