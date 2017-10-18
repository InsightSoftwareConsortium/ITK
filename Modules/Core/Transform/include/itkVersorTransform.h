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
#ifndef itkVersorTransform_h
#define itkVersorTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "vnl/vnl_quaternion.h"

namespace itk
{
/** \class VersorTransform
 *
 * VersorTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation to the space. Rotation is about
 * a user specified center.
 *
 * The serialization of the optimizable parameters is an array of 3 elements
 * representing the right part of the versor.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 * TODO: Need to make sure that the translation parameters in the baseclass
 * cannot be set to non-zero values.
 *
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT VersorTransform : public Rigid3DTransform<TParametersValueType>
{
public:
  /** Standard Self Typedef */
  typedef VersorTransform                        Self;
  typedef Rigid3DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;

  /** Run-time type information (and related methods).  */
  itkTypeMacro(VersorTransform, Rigid3DTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension of parameters */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 3);

  /** Parameters Type   */
  typedef typename Superclass::ScalarType                ScalarType;
  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::MatrixType                MatrixType;
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::OffsetType                OffsetType;

  /** VnlQuaternion Type */
  typedef vnl_quaternion<TParametersValueType> VnlQuaternionType;

  /** Versor Type */
  typedef Versor<TParametersValueType>       VersorType;
  typedef typename VersorType::VectorType    AxisType;
  typedef typename VersorType::ValueType     AngleType;
  typedef typename AxisType::ValueType       AxisValueType;
  typedef typename ParametersType::ValueType ParametersValueType;

  /**
   * Set the transformation from a container of parameters
   * This is typically used by optimizers.
   *
   * There are 3 parameters. They represent the components
   * of the right part of the versor. This can be seen
   * as the components of the vector parallel to the rotation
   * axis and multiplied by std::sin( angle / 2 ). */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the Transformation Parameters. */
  const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  /** Set the rotational part of the transform */
  void SetRotation(const VersorType & versor);

  void SetRotation(const AxisType & axis, AngleType angle);

  itkGetConstReferenceMacro(Versor, VersorType);

  /** Set the parameters to the IdentityTransform */
  virtual void SetIdentity(void) ITK_OVERRIDE;

  /** Compute the Jacobian of the transformation
   *  This method computes the Jacobian matrix of the transformation.
   *  given point or vector, returning the transformed point or
   *  vector. The rank of the Jacobian will also indicate if the
   *  transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

protected:

  /** Construct an VersorTransform object */
  VersorTransform(const MatrixType & matrix, const OutputVectorType & offset);
  VersorTransform(unsigned int paramDims);
  VersorTransform();

  /** Destroy an VersorTransform object */
  ~VersorTransform() ITK_OVERRIDE {}

#ifdef ITKV3_COMPATIBILITY
  /** \deprecated This method must be made protected here because it is not a safe way of
   * initializing the Versor */
  itkLegacyMacro( virtual void SetRotationMatrix(const MatrixType & matrix) ITK_OVERRIDE);
#endif

  void SetVarVersor(const VersorType & newVersor)
  {
    m_Versor = newVersor;
  }

  /** Print contents of a VersorTransform */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Compute Matrix
   *  Compute the components of the rotation matrix in the superclass */
  void ComputeMatrix(void) ITK_OVERRIDE;

  void ComputeMatrixParameters(void) ITK_OVERRIDE;

private:
  /** Copy a VersorTransform object */
  VersorTransform(const Self & other); // Not implemented

  /** Assignment operator */
  const Self & operator=(const Self &);   // Not implemented

  /** Versor containing the rotation */
  VersorType m_Versor;
}; // class VersorTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorTransform.hxx"
#endif

#endif /* itkVersorTransform_h */
