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
#ifndef __itkQuaternionRigidTransform_h
#define __itkQuaternionRigidTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "vnl/vnl_quaternion.h"

namespace itk
{
/** \class QuaternionRigidTransform
 * \brief QuaternionRigidTransform of a vector space (e.g. space coordinates).
 *
 * This transform applies a rotation and translation to the space given
 * a quaternion and a 3D translation. Rotation is about a user specified center.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 7 elements.
 * The first 4 elements are the components of the quaternion representation
 * of 3D rotation. The last 3 parameters defines the translation in each
 * dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *
 * \ingroup ITKTransform
 */
template <class TScalarType = double>
// Data type for scalars (float or double)
class QuaternionRigidTransform :
  public Rigid3DTransform<TScalarType>
{
public:
  /** Standard class typedefs.   */
  typedef QuaternionRigidTransform      Self;
  typedef Rigid3DTransform<TScalarType> Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuaternionRigidTransform, Rigid3DTransform);

  /** Dimension of parameters   */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 7);

  /** Parameters Type   */
  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::ParametersValueType       ParametersValueType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::ScalarType                ScalarType;
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::OutputVectorValueType     OutputVectorValueType;
  typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::MatrixType                MatrixType;
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::OffsetType                OffsetType;
  typedef typename Superclass::TranslationType           TranslationType;

  /** VnlQuaternion type.  */
  typedef vnl_quaternion<TScalarType> VnlQuaternionType;

  /** Compute the Jacobian Matrix of the transformation at one point */
  /** Set the rotation of the rigid transform.
   * This method sets the rotation of a QuaternionRigidTransform to a
   * value specified by the user. */
  void SetRotation(const VnlQuaternionType & rotation);

  /** Get the rotation from an QuaternionRigidTransform.
   * This method returns the value of the rotation of the
   * QuaternionRigidTransform. */
  const VnlQuaternionType & GetRotation(void) const
  {
    return m_Rotation;
  }

  /** Set the parameters to the IdentityTransform */
  virtual void SetIdentity(void);

  /** Set the transformation from a container of parameters.
   * This is typically used by optimizers.
   * There are 7 parameters. The first four represents the
   * quaternion and the last three represents the
   * offset. */
  void SetParameters(const ParametersType & parameters);

  virtual const ParametersType & GetParameters() const;

  /** Compute the Jacobian of the transformation.
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const;

protected:
  QuaternionRigidTransform(const MatrixType & matrix, const OutputVectorType & offset);
  QuaternionRigidTransform(unsigned int paramDims);
  QuaternionRigidTransform();
  ~QuaternionRigidTransform()
  {
  }

  void ComputeMatrix();

  void ComputeMatrixParameters();

  void SetVarRotation(const VnlQuaternionType & rotation)
  {
    m_Rotation = rotation;
  }

  const InverseMatrixType & GetInverseMatrix(void) const;

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  QuaternionRigidTransform(const Self &); // purposely not implemented
  void operator=(const Self &);           // purposely not implemented

  /** Rotation of the transformation. */
  VnlQuaternionType m_Rotation;
}; // class QuaternionRigidTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidTransform.hxx"
#endif

#endif /* __itkQuaternionRigidTransform_h */
