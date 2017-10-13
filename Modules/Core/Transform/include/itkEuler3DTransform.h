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
#ifndef itkEuler3DTransform_h
#define itkEuler3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"

namespace itk
{
/** \class Euler3DTransform
 *
 * \brief Euler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space given 3 euler
 * angles and a 3D translation. Rotation is about a user specified center.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 6 elements.
 * The first 3 represents three euler angle of rotation respectively about
 * the X, Y and Z axis. The last 3 parameters defines the translation in each
 * dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double >
class ITK_TEMPLATE_EXPORT Euler3DTransform :
  public Rigid3DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef Euler3DTransform                       Self;
  typedef Rigid3DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Euler3DTransform, Rigid3DTransform);

  /** Dimension of the space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 6);

  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::ParametersValueType       ParametersValueType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
  typedef typename Superclass::FixedParametersValueType  FixedParametersValueType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::ScalarType                ScalarType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::MatrixType                MatrixType;
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::TranslationType           TranslationType;
  typedef typename Superclass::OffsetType                OffsetType;
  typedef typename Superclass::ScalarType                AngleType;

  /** Set/Get the transformation from a container of parameters
   * This is typically used by optimizers.  There are 6 parameters. The first
   * three represent the angles to rotate around the coordinate axis, and the
   * last three represents the offset. */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;
  virtual void SetFixedParameters(const FixedParametersType & parameters) ITK_OVERRIDE;

  /** Set the rotational part of the transform. */
  void SetRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ);

  itkGetConstMacro(AngleX, ScalarType);
  itkGetConstMacro(AngleY, ScalarType);
  itkGetConstMacro(AngleZ, ScalarType);

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /** The Euler angle representation of a rotation is not unique and
   * depends on the order of rotations. In general there are 12
   * options. This class supports two of them, ZXY and ZYX. The
   * default is ZXY. These functions set and get the value which
   * indicates whether the rotation is ZYX or ZXY.
   */
  virtual void SetComputeZYX (const bool flag);
  itkGetConstMacro(ComputeZYX, bool);

  virtual void SetIdentity(void) ITK_OVERRIDE;

protected:
  Euler3DTransform(const MatrixType & matrix, const OutputPointType & offset);
  Euler3DTransform(unsigned int paramsSpaceDims);
  Euler3DTransform();

  ~Euler3DTransform() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Set values of angles directly without recomputing other parameters. */
  void SetVarRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ);

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrix(void) ITK_OVERRIDE;

  void ComputeMatrixParameters(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Euler3DTransform);

  ScalarType m_AngleX;
  ScalarType m_AngleY;
  ScalarType m_AngleZ;
  bool       m_ComputeZYX;
}; // class Euler3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuler3DTransform.hxx"
#endif

#endif /* itkEuler3DTransform_h */
