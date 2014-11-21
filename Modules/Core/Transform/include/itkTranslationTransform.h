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
#ifndef __itkTranslationTransform_h
#define __itkTranslationTransform_h

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
template < typename TScalar = double,          // Data type for scalars (float or
                                       // double)
           unsigned int NDimensions = 3>
class TranslationTransform :
  public Transform< TScalar, NDimensions, NDimensions >
{
public:
  /** Standard class typedefs. */
  typedef TranslationTransform                           Self;
  typedef Transform< TScalar, NDimensions, NDimensions > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

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
  typedef typename Superclass::ParametersType ParametersType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Standard vector type for this class. */
  typedef Vector<TScalar, NDimensions> InputVectorType;
  typedef Vector<TScalar, NDimensions> OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector<TScalar, NDimensions> InputCovariantVectorType;
  typedef CovariantVector<TScalar, NDimensions> OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TScalar, NDimensions> InputVnlVectorType;
  typedef vnl_vector_fixed<TScalar, NDimensions> OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef Point<TScalar, NDimensions> InputPointType;
  typedef Point<TScalar, NDimensions> OutputPointType;

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
  OutputPointType     TransformPoint(const InputPointType  & point) const;

  using Superclass::TransformVector;
  OutputVectorType    TransformVector(const InputVectorType & vector) const;

  OutputVnlVectorType TransformVector(const InputVnlVectorType & vector) const;

  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType & vector) const;

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
  void SetIdentity(void);

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
  virtual void SetFixedParameters(const ParametersType &) ITK_OVERRIDE
  {
  }

  /** Get the Fixed Parameters. The TranslationTransform does not
   * require Fixed parameters, therefore this method returns an
   * parameters array of size zero. */
  virtual const ParametersType & GetFixedParameters() const ITK_OVERRIDE
  {
    this->m_FixedParameters.SetSize(0);
    return this->m_FixedParameters;
  }

protected:
  TranslationTransform();
  ~TranslationTransform();
  /** Print contents of an TranslationTransform. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  TranslationTransform(const Self &); // purposely not implemented
  void operator=(const Self &);       // purposely not implemented

  JacobianType     m_IdentityJacobian;
  OutputVectorType m_Offset; // Offset of the transformation
};                           // class TranslationTransform

// Back transform a point
template <typename TScalar, unsigned int NDimensions>
inline
typename TranslationTransform<TScalar, NDimensions>::InputPointType
TranslationTransform<TScalar, NDimensions>::BackTransform(const OutputPointType & point) const
{
  return point - m_Offset;
}

// Back transform a vector
template <typename TScalar, unsigned int NDimensions>
inline
typename TranslationTransform<TScalar, NDimensions>::InputVectorType
TranslationTransform<TScalar, NDimensions>::BackTransform(const OutputVectorType & vect) const
{
  return vect;
}

// Back transform a vnl_vector
template <typename TScalar, unsigned int NDimensions>
inline
typename TranslationTransform<TScalar, NDimensions>::InputVnlVectorType
TranslationTransform<TScalar, NDimensions>::BackTransform(const OutputVnlVectorType & vect) const
{
  return vect;
}

// Back Transform a CovariantVector
template <typename TScalar, unsigned int NDimensions>
inline
typename TranslationTransform<TScalar, NDimensions>::InputCovariantVectorType
TranslationTransform<TScalar, NDimensions>::BackTransform(const OutputCovariantVectorType & vect) const
{
  return vect;
}

}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTranslationTransform.hxx"
#endif

#endif /* __itkTranslationTransform_h */
