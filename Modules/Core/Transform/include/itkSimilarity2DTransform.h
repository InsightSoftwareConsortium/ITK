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
#ifndef itkSimilarity2DTransform_h
#define itkSimilarity2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{
/** \class Similarity2DTransform
 * \brief Similarity2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a homogenous scale and rigid transform in
 * 2D space. The transform is specified as a scale and rotation around
 * a arbitrary center and is followed by a translation.
 * given one angle for rotation, a homogeneous scale and a 2D offset for translation.
 *
 * The parameters for this transform can be set either using
 * individual Set methods or in serialized form using
 * SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 3 elements
 * ordered as follows:
 * p[0] = scale
 * p[1] = angle
 * p[2] = x component of the translation
 * p[3] = y component of the translation
 *
 * The serialization of the fixed parameters is an array of 2 elements
 * ordered as follows:
 * p[0] = x coordinate of the center
 * p[1] = y coordinate of the center
 *
 * Access methods for the center, translation and underlying matrix
 * offset vectors are documented in the superclass MatrixOffsetTransformBase.
 *
 * Access methods for the angle are documented in superclass Rigid2DTransform.
 *
 * \sa Transform
 * \sa MatrixOffsetTransformBase
 * \sa Rigid2DTransform
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT Similarity2DTransform :
  public Rigid2DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef Similarity2DTransform                  Self;
  typedef Rigid2DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Similarity2DTransform, Rigid2DTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension,           unsigned int, 2);
  itkStaticConstMacro(InputSpaceDimension,      unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension,     unsigned int, 2);
  itkStaticConstMacro(ParametersDimension,      unsigned int, 4);

  typedef typename Superclass::ScalarType ScalarType;
  typedef          TParametersValueType   ScaleType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Offset type. */
  typedef typename Superclass::OffsetType      OffsetType;
  typedef typename Superclass::OffsetValueType OffsetValueType;

  /** Matrix type. */
  typedef typename Superclass::MatrixType      MatrixType;
  typedef typename Superclass::MatrixValueType MatrixValueType;

  /** Point type. */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;

  /** Vector type. */
  typedef typename Superclass::InputVectorType  InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

  /** CovariantVector type. */
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;

  /** VnlVector type. */
  typedef typename Superclass::InputVnlVectorType  InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType OutputVnlVectorType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set the Scale part of the transform. */
  void SetScale(ScaleType scale);

  itkGetConstReferenceMacro(Scale, ScaleType);

  /** Set the transformation from a container of parameters
    * This is typically used by optimizers.
    * There are 4 parameters. The first one represents the
    * scale, the second represents the angle of rotation
    * and the last two represent the translation.
    * The center of rotation is fixed.
    *
    * \sa Transform::SetParameters()
    * \sa Transform::SetFixedParameters() */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 4 parameters. The first one represents the
   * scale, the second represents the angle of rotation,
   * and the last two represent the translation.
   * The center of rotation is fixed.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** This method computes the Jacobian matrix of the transformation
  * at a given input point.
  */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /** Set the transformation to an identity. */
  virtual void SetIdentity() ITK_OVERRIDE;

  /**
   * This method creates and returns a new Similarity2DTransform object
   * which is the inverse of self.
   */
  void CloneInverseTo(Pointer & newinverse) const;

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /**
   * This method creates and returns a new Similarity2DTransform object
   * which has the same parameters.
   */
  void CloneTo(Pointer & clone) const;

  /**
   * Set the rotation Matrix of a Similarity 2D Transform
   *
   * This method sets the 2x2 matrix representing a similarity
   * transform.  The Matrix is expected to be a valid
   * similarity transform with a certain tolerance.
   *
   * \warning This method will throw an exception if the matrix
   * provided as argument is not valid.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix()
   *
   */
  virtual void SetMatrix(const MatrixType & matrix) ITK_OVERRIDE;

  /**
   * Set the rotation Matrix of a Similarity 2D Transform
   *
   * This method sets the 2x2 matrix representing a similarity
   * transform.  The Matrix is expected to be a valid
   * similarity transform within the given tolerance.
   *
   * \warning This method will throw an exception if the matrix
   * provided as argument is not valid.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix()
   *
   */
  virtual void SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance) ITK_OVERRIDE;

protected:
  Similarity2DTransform(unsigned int outputSpaceDimension, unsigned int parametersDimension);
  Similarity2DTransform(unsigned int parametersDimension);
  Similarity2DTransform();

  ~Similarity2DTransform() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Compute matrix from angle and scale. This is used in Set methods
   * to update the underlying matrix whenever a transform parameter
   * is changed. */
  virtual void ComputeMatrix(void) ITK_OVERRIDE;

  /** Compute the angle and scale from the matrix. This is used to compute
   * transform parameters from a given matrix. This is used in
   * MatrixOffsetTransformBase::Compose() and
   * MatrixOffsetTransformBase::GetInverse(). */
  virtual void ComputeMatrixParameters(void) ITK_OVERRIDE;

  /** Set the scale without updating underlying variables. */
  void SetVarScale(ScaleType scale)
  {
    m_Scale = scale;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Similarity2DTransform);

  ScaleType m_Scale;
}; // class Similarity2DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarity2DTransform.hxx"
#endif

#endif /* itkSimilarity2DTransform_h */
