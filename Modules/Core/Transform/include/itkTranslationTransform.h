/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
 * but with a large difference in performace.
 *
 * \ingroup ITKTransform
 *
 * \wiki
 * \wikiexample{SimpleOperations/TranslationTransform,Translate an image}
 * \wikiexample{VectorImages/VectorResampleImageFilter,Translate a vector image}
 * \wikiexample{Registration/ImageRegistrationMethod,A basic global registration of two images}
 * \wikiexample{Registration/MutualInformation,Mutual Information}
 * \endwiki
 */
template<typename TParametersValueType=double,
           unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT TranslationTransform :
  public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef TranslationTransform                                      Self;
  typedef Transform<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TranslationTransform, Transform);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int, NDimensions);

  /** Standard scalar type for this class. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard parameters container. */
  typedef typename Superclass::FixedParametersType FixedParametersType;
  typedef typename Superclass::ParametersType      ParametersType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Standard vector type for this class. */
  typedef Vector<TParametersValueType, NDimensions> InputVectorType;
  typedef Vector<TParametersValueType, NDimensions> OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector<TParametersValueType, NDimensions> InputCovariantVectorType;
  typedef CovariantVector<TParametersValueType, NDimensions> OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TParametersValueType, NDimensions> InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType, NDimensions> OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef Point<TParametersValueType, NDimensions> InputPointType;
  typedef Point<TParametersValueType, NDimensions> OutputPointType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType TransformCategoryType;

  /** This method returns the value of the offset of the
   * TranslationTransform. */
  const OutputVectorType & GetOffset(void) const
  {
    return m_Offset;
  }

  /** This method sets the parameters for the transform
   * value specified by the user. */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the Transformation Parameters. */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** Set offset of an Translation Transform.
   * This method sets the offset of an TranslationTransform to a
   * value specified by the user. */
  void SetOffset(const OutputVectorType & offset)
  {
    m_Offset = offset; return;
  }

  /** Compose with another TranslationTransform. */
  void Compose(const Self *other, bool pre = 0);

  /** Compose affine transformation with a translation.
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise. */
  void Translate(const OutputVectorType & offset, bool pre = 0);

  /** Transform by an affine transformation.
   * This method applies the affine transform given by self to a
   * given point or vector, returning the transformed point or
   * vector. */
  OutputPointType     TransformPoint(const InputPointType  & point) const ITK_OVERRIDE;

  using Superclass::TransformVector;
  OutputVectorType    TransformVector(const InputVectorType & vector) const ITK_OVERRIDE;

  OutputVnlVectorType TransformVector(const InputVnlVectorType & vector) const ITK_OVERRIDE;

  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType & vector) const ITK_OVERRIDE;

  /** This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown. */
  inline InputPointType    BackTransform(const OutputPointType  & point) const;

  inline InputVectorType   BackTransform(const OutputVectorType & vector) const;

  inline InputVnlVectorType BackTransform(const OutputVnlVectorType & vector) const;

  inline InputCovariantVectorType BackTransform(const OutputCovariantVectorType & vector) const;

  /** Find inverse of an affine transformation.
   * This method creates and returns a new TranslationTransform object
   * which is the inverse of self.  If self is not invertible,
   * false is returned.  */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /** Compute the Jacobian Matrix of the transformation at one point */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType & point, JacobianType & j) const ITK_OVERRIDE;

  /** Get the jacobian with respect to position, which simply is an identity
   *  jacobian because the transform is position-invariant.
   *  jac will be resized as needed, but it will be more efficient if
   *  it is already properly sized. */
  virtual void ComputeJacobianWithRespectToPosition(const InputPointType & x, JacobianType & jac) const ITK_OVERRIDE;

  /** Set the parameters to the IdentityTransform */
  void SetIdentity();

  /** Return the number of parameters that completely define the Transfom  */
  virtual NumberOfParametersType GetNumberOfParameters() const ITK_OVERRIDE
  {
    return NDimensions;
  }

  /** Indicates that this transform is linear. That is, given two
   * points P and Q, and scalar coefficients a and b, then
   *
   * \f[ T( a*P + b*Q ) = a * T(P) + b * T(Q) \f]
   */
  virtual bool IsLinear() const ITK_OVERRIDE
  {
    return true;
  }

  /** Indicates the category transform.
   *  e.g. an affine transform, or a local one, e.g. a deformation field.
   */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE
  {
    return Self::Linear;
  }

  /** Set the fixed parameters and update internal transformation.
   * The Translation Transform does not require fixed parameters,
   * therefore the implementation of this method is a null operation. */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE
  {
  }

  /** Get the Fixed Parameters. The TranslationTransform does not
   * require Fixed parameters, therefore this method returns an
   * parameters array of size zero. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE
  {
    this->m_FixedParameters.SetSize(0);
    return this->m_FixedParameters;
  }

protected:
  TranslationTransform();
  ~TranslationTransform() ITK_OVERRIDE;
  /** Print contents of an TranslationTransform. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TranslationTransform);

  JacobianType     m_IdentityJacobian;
  OutputVectorType m_Offset; // Offset of the transformation
};                           // class TranslationTransform

// Back transform a point
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename TranslationTransform<TParametersValueType, NDimensions>::InputPointType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputPointType & point) const
{
  return point - m_Offset;
}

// Back transform a vector
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename TranslationTransform<TParametersValueType, NDimensions>::InputVectorType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputVectorType & vect) const
{
  return vect;
}

// Back transform a vnl_vector
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename TranslationTransform<TParametersValueType, NDimensions>::InputVnlVectorType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputVnlVectorType & vect) const
{
  return vect;
}

// Back Transform a CovariantVector
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename TranslationTransform<TParametersValueType, NDimensions>::InputCovariantVectorType
TranslationTransform<TParametersValueType, NDimensions>::BackTransform(const OutputCovariantVectorType & vect) const
{
  return vect;
}

}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTranslationTransform.hxx"
#endif

#endif /* itkTranslationTransform_h */
