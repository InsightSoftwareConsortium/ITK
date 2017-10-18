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
#ifndef itkRigid3DTransform_h
#define itkRigid3DTransform_h

#include <iostream>
#include "itkMatrixOffsetTransformBase.h"
#include "itkVersor.h"

namespace itk
{
/** \class Rigid3DTransform
 * \brief Rigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation in 3D space.
 * The transform is specified as a rotation matrix around a arbitrary center
 * and is followed by a translation.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 12 elements.
 * The first 9 parameters represents the rotation matrix in row-major order
 * (where the column index varies the fastest). The last 3 parameters defines
 * the translation in each dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation in each dimension.
 *
 * The Rigid3DTransform is intended to be a base class that
 * defines a consistent family of transform types that respect
 * rigid transformations.  Only classes that derive from Rigid3DTransform
 * should be used.
 *
 * \sa Euler3DTransform
 * \sa QuaternionRigidTransform
 * \sa VersorTransform
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT Rigid3DTransform:
  public MatrixOffsetTransformBase<TParametersValueType, 3, 3>
{
public:
  /** Standard class typedefs. */
  typedef Rigid3DTransform                                      Self;
  typedef MatrixOffsetTransformBase<TParametersValueType, 3, 3> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

#ifdef ITKV3_COMPATIBILITY
  /** Run-time type information (and related methods).   */
  itkNewMacro(Self);
#endif

  /** Run-time type information (and related methods). */
  itkTypeMacro(Rigid3DTransform, MatrixOffsetTransformBase);

  /** Dimension of the space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 12);

  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::ParametersValueType       ParametersValueType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
  typedef typename Superclass::FixedParametersValueType  FixedParametersValueType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::ScalarType                ScalarType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::OutputVectorValueType     OutputVectorValueType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::MatrixType                MatrixType;
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::MatrixValueType           MatrixValueType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::TranslationType           TranslationType;
  typedef typename Superclass::OffsetType                OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 12 parameters. The first 9 represents the rotation
   * matrix is row-major order and the last 3 represents the translation.
   *
   * \warning The rotation matrix must be orthogonal to within a specified tolerance,
   * else an exception is thrown.
   *
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Directly set the rotation matrix of the transform.
   * \warning The input matrix must be orthogonal to within a specified tolerance,
   * else an exception is thrown.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType & matrix) ITK_OVERRIDE;

  /** Directly set the rotation matrix of the transform.
   * \warning The input matrix must be orthogonal to within the specified tolerance,
   * else an exception is thrown.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance );

  /**
   * Compose the transformation with a translation
   *
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise.
   */
  void Translate(const OffsetType & offset, bool pre = false);

  /**
   * Utility function to test if a matrix is orthogonal within a specified
   * tolerance
   */
  bool MatrixIsOrthogonal(const MatrixType & matrix,
              const TParametersValueType tolerance =
                  MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance());

#ifdef ITKV3_COMPATIBILITY
  /** Get an inverse of this transform. */
  //NOTE: itkLegacyRemove can not be used for GetInverse
  //      because in itkV3 mode these functions
  //      must be traversed when calling the child classes
  //      member functions
  //      (with no real effect) for backwards compatibility.
  //      In ITKv4 mode only the super class is needed
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  //NOTE: itkLegacyRemove can not be used for GetInverseTransform
  //      because in itkV3 mode these functions
  //      must be traversed when calling the child classes
  //      member functions
  //      (with no real effect) for backwards compatibility.
  //      In ITKv4 mode only the super class is needed
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;
  /**
   * Get rotation Matrix from an Rigid3DTransform
   *
   * This method returns the value of the rotation of the
   * Rigid3DTransform.
   *
   * \deprecated Use GetMatrix instead
   */
  itkLegacyMacro(const MatrixType & GetRotationMatrix() const);
  /**
   * Set the rotation Matrix of a Rigid3D Transform
   *
   * This method sets the 3x3 matrix representing a rotation
   * in the transform.  The Matrix is expected to be orthogonal
   * with a certain tolerance.
   *
   * \deprecated Use SetMatrix instead
   *
   */
  itkLegacyMacro(virtual void SetRotationMatrix(const MatrixType & matrix) );
#endif

  /**
   * Back transform by an affine transformation
   *
   * This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown.
   *
   * \deprecated Please use GetInverseTransform and then call the forward
   *   transform using the result.
   *
   */
  itkLegacyMacro(InputPointType      BackTransform(const OutputPointType & point) const);
  itkLegacyMacro(InputVectorType     BackTransform(const OutputVectorType & vector) const);
  itkLegacyMacro(InputVnlVectorType  BackTransform(const OutputVnlVectorType & vector) const);
  itkLegacyMacro(InputCovariantVectorType BackTransform(const OutputCovariantVectorType & vector) const);

protected:
  Rigid3DTransform(const MatrixType & matrix,
                   const OutputVectorType & offset);
  Rigid3DTransform(unsigned int paramDim);
  Rigid3DTransform();
  ~Rigid3DTransform() ITK_OVERRIDE;

  /**
   * Print contents of an Rigid3DTransform
   */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Rigid3DTransform);
};                                //class Rigid3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRigid3DTransform.hxx"
#endif

#endif /* itkRigid3DTransform_h */
