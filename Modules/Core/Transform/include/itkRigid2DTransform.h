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
#ifndef itkRigid2DTransform_h
#define itkRigid2DTransform_h

#include "itkMatrixOffsetTransformBase.h"

namespace itk
{
/** \class Rigid2DTransform
 * \brief Rigid2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rigid transformation in 2D space.
 * The transform is specified as a rotation around a arbitrary center
 * and is followed by a translation.
 *
 * The parameters for this transform can be set either using
 * individual Set methods or in serialized form using
 * SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 3 elements
 * ordered as follows:
 * p[0] = angle
 * p[1] = x component of the translation
 * p[2] = y component of the translation
 *
 * The serialization of the fixed parameters is an array of 2 elements
 * ordered as follows:
 * p[0] = x coordinate of the center
 * p[1] = y coordinate of the center
 *
 * Access methods for the center, translation and underlying matrix
 * offset vectors are documented in the superclass MatrixOffsetTransformBase.
 *
 * \sa Transfrom
 * \sa MatrixOffsetTransformBase
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT Rigid2DTransform :
  public MatrixOffsetTransformBase<TParametersValueType, 2, 2>
{
public:
  /** Standard class typedefs. */
  typedef Rigid2DTransform                                      Self;
  typedef MatrixOffsetTransformBase<TParametersValueType, 2, 2> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Rigid2DTransform, MatrixOffsetTransformBase);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension of the space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 2);
  itkStaticConstMacro(ParametersDimension, unsigned int, 3);

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  // / Standard matrix type for this class
  typedef typename Superclass::MatrixType      MatrixType;
  typedef typename Superclass::MatrixValueType MatrixValueType;

  // / Standard vector type for this class
  typedef typename Superclass::OffsetType      OffsetType;
  typedef typename Superclass::OffsetValueType OffsetValueType;

  // / Standard vector type for this class
  typedef typename Superclass::InputVectorType       InputVectorType;
  typedef typename Superclass::OutputVectorType      OutputVectorType;
  typedef typename Superclass::OutputVectorValueType OutputVectorValueType;

  // / Standard covariant vector type for this class
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;

  // / Standard vnl_vector type for this class
  typedef typename Superclass::InputVnlVectorType  InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType OutputVnlVectorType;

  // / Standard coordinate point type for this class
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /**
   * Set the rotation Matrix of a Rigid2D Transform
   *
   * This method sets the 2x2 matrix representing the rotation
   * in the transform.  The Matrix is expected to be orthogonal
   * with a certain tolerance.
   *
   * \warning This method will throw an exception is the matrix
   * provided as argument is not orthogonal.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix()
   */
  virtual void SetMatrix(const MatrixType & matrix) ITK_OVERRIDE;

  /**
   * Set the rotation Matrix of a Rigid2D Transform
   *
   * This method sets the 2x2 matrix representing the rotation
   * in the transform.  The Matrix is expected to be orthogonal
   * with a certain tolerance.
   *
   * \warning This method will throw an exception is the matrix
   * provided as argument is not orthogonal within the given tolerance.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix()
   */
  virtual void SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance);

  /**
   * Compose the transformation with a translation
   *
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise.
   */
  void Translate(const OffsetType & offset, bool pre = false);

  /**
   * Back transform by an rigid transformation.
   *
   * The BackTransform() methods are slated to be removed from ITK.
   * Instead, please use GetInverse() or CloneInverseTo() to generate
   * an inverse transform and  then perform the transform using that
   * inverted transform.
   */
  inline InputPointType      BackTransform(const OutputPointType  & point) const;

  inline InputVectorType     BackTransform(const OutputVectorType & vector) const;

  inline InputVnlVectorType  BackTransform(const OutputVnlVectorType & vector) const;

  inline InputCovariantVectorType BackTransform(const OutputCovariantVectorType & vector) const;

  /** Set/Get the angle of rotation in radians */
  void SetAngle(TParametersValueType angle);

  itkGetConstReferenceMacro(Angle, TParametersValueType);

  /** Set the angle of rotation in degrees. */
  void SetAngleInDegrees(TParametersValueType angle);

  /** Set/Get the angle of rotation in radians. These methods
   * are old and are retained for backward compatibility.
   * Instead, use SetAngle() and GetAngle(). */
  void SetRotation(TParametersValueType angle)
  {
    this->SetAngle(angle);
  }
  virtual const TParametersValueType & GetRotation() const
  {
    return m_Angle;
  }

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 3 parameters. The first one represents the
   * angle of rotation in radians and the last two represents the translation.
   * The center of rotation is fixed.
   *
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 3 parameters. The first one represents the
   * angle or rotation in radians and the last two represents the translation.
   * The center of rotation is fixed.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** Compute the Jacobian Matrix of the transformation at one point,
   *  allowing for thread-safety. */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /**
   * This method creates and returns a new Rigid2DTransform object
   * which is the inverse of self.
   */
  void CloneInverseTo(Pointer & newinverse) const;

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /**
   * This method creates and returns a new Rigid2DTransform object
   * which has the same parameters.
   */
  void CloneTo(Pointer & clone) const;

  /** Reset the parameters to create and identity transform. */
  virtual void SetIdentity() ITK_OVERRIDE;

#ifdef ITKV3_COMPATIBILITY
  /**
   * \deprecated
   * Set/Get the rotation matrix. These methods are old and are
   * retained for backward compatibility. Instead, use SetMatrix()
   * GetMatrix().
   */
  itkLegacyMacro(virtual void SetRotationMatrix(const MatrixType & matrix));
  itkLegacyMacro(const MatrixType & GetRotationMatrix() const);
#endif

protected:
  Rigid2DTransform(unsigned int outputSpaceDimension, unsigned int parametersDimension);
  Rigid2DTransform(unsigned int parametersDimension);
  Rigid2DTransform();

  ~Rigid2DTransform() ITK_OVERRIDE;

  /**
    * Print contents of an Rigid2DTransform
    */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Compute the matrix from angle. This is used in Set methods
   * to update the underlying matrix whenever a transform parameter
   * is changed. */
  virtual void ComputeMatrix() ITK_OVERRIDE;

  /** Compute the angle from the matrix. This is used to compute
   * transform parameters from a given matrix. This is used in
   * MatrixOffsetTransformBase::Compose() and
   * MatrixOffsetTransformBase::GetInverse(). */
  virtual void ComputeMatrixParameters() ITK_OVERRIDE;

  /** Update angle without recomputation of other internal variables. */
  void SetVarAngle(TParametersValueType angle)
  {
    m_Angle = angle;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Rigid2DTransform);

  TParametersValueType m_Angle;

}; // class Rigid2DTransform

// Back transform a point
template<typename TParametersValueType>
inline
typename Rigid2DTransform<TParametersValueType>::InputPointType
Rigid2DTransform<TParametersValueType>::BackTransform(const OutputPointType & point) const
{
  itkWarningMacro(
    <<
    "BackTransform(): This method is slated to be removed from ITK.  Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform."
    );
  return this->GetInverseMatrix() * ( point - this->GetOffset() );
}

// Back transform a vector
template<typename TParametersValueType>
inline
typename Rigid2DTransform<TParametersValueType>::InputVectorType
Rigid2DTransform<TParametersValueType>::BackTransform(const OutputVectorType & vect) const
{
  itkWarningMacro(
    <<
    "BackTransform(): This method is slated to be removed from ITK.  Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform."
    );
  return this->GetInverseMatrix() * vect;
}

// Back transform a vnl_vector
template<typename TParametersValueType>
inline
typename Rigid2DTransform<TParametersValueType>::InputVnlVectorType
Rigid2DTransform<TParametersValueType>::BackTransform(const OutputVnlVectorType & vect) const
{
  itkWarningMacro(
    <<
    "BackTransform(): This method is slated to be removed from ITK.  Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform."
    );
  return this->GetInverseMatrix() * vect;
}

// Back Transform a CovariantVector
template<typename TParametersValueType>
inline
typename Rigid2DTransform<TParametersValueType>::InputCovariantVectorType
Rigid2DTransform<TParametersValueType>::BackTransform(const OutputCovariantVectorType & vect) const
{
  itkWarningMacro(
    <<
    "BackTransform(): This method is slated to be removed from ITK.  Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform."
    );
  return this->GetMatrix() * vect;
}

}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRigid2DTransform.hxx"
#endif

#endif /* itkRigid2DTransform_h */
