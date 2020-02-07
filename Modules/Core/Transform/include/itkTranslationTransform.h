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
#ifndef itkTranslationTransform_h
#define itkTranslationTransform_h

#include "itkTransform.h"
#include "itkMacro.h"
#include "itkMatrix.h"

namespace itk
{

/** \class TranslationTransform
 * \brief Translation transformation of a vector space (e.g. space coordinates)
 *
 * The same functionality could be obtained by using the Affine transform,
 * but with a large difference in performance.
 *
 * \ingroup ITKTransform
 *
 * \sphinx
 * \sphinxexample{Core/Transform/TranslateAVectorImage,Translate Vector Image}
 * \sphinxexample{Registration/Common/GlobalRegistrationOfTwoImages,Global Registration Of Two Images}
 * \sphinxexample{Registration/Common/MutualInformation,Mutual Information}
 * \endsphinx
 */
template <typename TParametersValueType = double, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT TranslationTransform : public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TranslationTransform);

  /** Standard class type aliases. */
  using Self = TranslationTransform;
  using Superclass = Transform<TParametersValueType, NDimensions, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TranslationTransform, Transform);

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions;

  /** Standard scalar type for this class. */
  using ScalarType = typename Superclass::ScalarType;

  /** Standard parameters container. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using ParametersType = typename Superclass::ParametersType;

  /** Standard Jacobian containers. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** The number of parameters defining this transform. */
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

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

  /** Transform category type. */
  using TransformCategoryEnum = typename Superclass::TransformCategoryEnum;

  /** This method returns the value of the offset of the
   * TranslationTransform. */
  const OutputVectorType &
  GetOffset() const
  {
    return m_Offset;
  }

  /** This method sets the parameters for the transform
   * value specified by the user. */
  void
  SetParameters(const ParametersType & parameters) override;

  /** Get the Transformation Parameters. */
  const ParametersType &
  GetParameters() const override;

  /** Set offset of an Translation Transform.
   * This method sets the offset of an TranslationTransform to a
   * value specified by the user. */
  void
  SetOffset(const OutputVectorType & offset)
  {
    m_Offset = offset;
    return;
  }

  /** Compose with another TranslationTransform. */
  void
  Compose(const Self * other, bool pre = false);

  /** Compose affine transformation with a translation.
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise. */
  void
  Translate(const OutputVectorType & offset, bool pre = false);

  /** Transform by an affine transformation.
   * This method applies the affine transform given by self to a
   * given point or vector, returning the transformed point or
   * vector. */
  OutputPointType
  TransformPoint(const InputPointType & point) const override;

  using Superclass::TransformVector;
  OutputVectorType
  TransformVector(const InputVectorType & vector) const override;

  OutputVnlVectorType
  TransformVector(const InputVnlVectorType & vector) const override;

  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & vector) const override;

  /** This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown. */
  inline InputPointType
  BackTransform(const OutputPointType & point) const;

  inline InputVectorType
  BackTransform(const OutputVectorType & vector) const;

  inline InputVnlVectorType
  BackTransform(const OutputVnlVectorType & vector) const;

  inline InputCovariantVectorType
  BackTransform(const OutputCovariantVectorType & vector) const;

  /** Find inverse of an affine transformation.
   * This method creates and returns a new TranslationTransform object
   * which is the inverse of self.  If self is not invertible,
   * false is returned.  */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /** Compute the Jacobian Matrix of the transformation at one point */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType & point, JacobianType & j) const override;


  /** Get the jacobian with respect to position, which simply is an identity
   *  jacobian because the transform is position-invariant.
   *  jac will be resized as needed, but it will be more efficient if
   *  it is already properly sized. */
  void
  ComputeJacobianWithRespectToPosition(const InputPointType & x, JacobianPositionType & jac) const override;
  using Superclass::ComputeJacobianWithRespectToPosition;

  /** Set the parameters to the IdentityTransform */
  void
  SetIdentity();

  /** Return the number of parameters that completely define the Transfom  */
  NumberOfParametersType
  GetNumberOfParameters() const override
  {
    return NDimensions;
  }

  /** Indicates that this transform is linear. That is, given two
   * points P and Q, and scalar coefficients a and b, then
   *
   * \f[ T( a*P + b*Q ) = a * T(P) + b * T(Q) \f]
   */
  bool
  IsLinear() const override
  {
    return true;
  }

  /** Indicates the category transform.
   *  e.g. an affine transform, or a local one, e.g. a deformation field.
   */
  TransformCategoryEnum
  GetTransformCategory() const override
  {
    return Self::TransformCategoryEnum::Linear;
  }

  /** Set the fixed parameters and update internal transformation.
   * The Translation Transform does not require fixed parameters,
   * therefore the implementation of this method is a null operation. */
  void
  SetFixedParameters(const FixedParametersType &) override
  {}

  /** Get the Fixed Parameters. The TranslationTransform does not
   * require Fixed parameters, therefore this method returns an
   * parameters array of size zero. */
  const FixedParametersType &
  GetFixedParameters() const override
  {
    this->m_FixedParameters.SetSize(0);
    return this->m_FixedParameters;
  }

protected:
  TranslationTransform();
  ~TranslationTransform() override = default;
  /** Print contents of an TranslationTransform. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  JacobianType     m_IdentityJacobian;
  OutputVectorType m_Offset; // Offset of the transformation
};                           // class TranslationTransform

// Back transform a point
template <typename TParametersValueType, unsigned int NDimensions>
inline typename TranslationTransform<TParametersValueType, NDimensions>::InputPointType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputPointType & point) const
{
  return point - m_Offset;
}

// Back transform a vector
template <typename TParametersValueType, unsigned int NDimensions>
inline typename TranslationTransform<TParametersValueType, NDimensions>::InputVectorType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputVectorType & vect) const
{
  return vect;
}

// Back transform a vnl_vector
template <typename TParametersValueType, unsigned int NDimensions>
inline typename TranslationTransform<TParametersValueType, NDimensions>::InputVnlVectorType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputVnlVectorType & vect) const
{
  return vect;
}

// Back Transform a CovariantVector
template <typename TParametersValueType, unsigned int NDimensions>
inline typename TranslationTransform<TParametersValueType, NDimensions>::InputCovariantVectorType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputCovariantVectorType & vect) const
{
  return vect;
}

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTranslationTransform.hxx"
#endif

#endif /* itkTranslationTransform_h */
