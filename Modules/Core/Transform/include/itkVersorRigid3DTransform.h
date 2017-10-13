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
#ifndef itkVersorRigid3DTransform_h
#define itkVersorRigid3DTransform_h

#include <iostream>
#include "itkVersorTransform.h"

namespace itk
{
/** \class VersorRigid3DTransform
 *
 * \brief VersorRigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 6 elements.
 * The first 3 elements are the components of the versor representation
 * of 3D rotation. The last 3 parameters defines the translation in each
 * dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT VersorRigid3DTransform :
  public VersorTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef VersorRigid3DTransform                Self;
  typedef VersorTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VersorRigid3DTransform, VersorTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 6);

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
  typedef typename Superclass::TranslationType           TranslationType;

  /** Versor type. */
  typedef typename Superclass::VersorType VersorType;
  typedef typename VersorType::VectorType VectorType;

  typedef typename Superclass::AxisType   AxisType;
  typedef typename Superclass::AngleType  AngleType;

  typedef typename Superclass::AxisValueType        AxisValueType;
  typedef typename Superclass::TranslationValueType TranslationValueType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  typedef  Array<ParametersValueType>               DerivativeType;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 6 parameters. The first three represent the
   * versor, the last three represent the translation. */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  virtual const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  /** Update the transform's parameters by the values in \c update.
   * \param update must be of the same length as returned by
   * GetNumberOfParameters(). Throw an exception otherwise.
   * \param factor is a scalar multiplier for each value in \c update.
   * SetParameters is called at the end of this method, to allow the transform
   * to perform any required operations on the updated parameters - typically
   * a conversion to member variables for use in TransformPoint. */
  virtual void UpdateTransformParameters( const DerivativeType & update, TParametersValueType factor = 1.0 ) ITK_OVERRIDE;

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

protected:
  VersorRigid3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  VersorRigid3DTransform(unsigned int paramDim);
  VersorRigid3DTransform();
  ~VersorRigid3DTransform() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VersorRigid3DTransform);

};                                      // class VersorRigid3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorRigid3DTransform.hxx"
#endif

#endif /* itkVersorRigid3DTransform_h */
