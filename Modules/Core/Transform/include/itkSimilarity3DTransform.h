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
#ifndef itkSimilarity3DTransform_h
#define itkSimilarity3DTransform_h

#include <iostream>
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class Similarity3DTransform
 * \brief Similarity3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation, translation and isotropic scaling to the space.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 7 elements.
 * The first 3 elements are the components of the versor representation
 * of 3D rotation. The next 3 parameters defines the translation in each
 * dimension. The last parameter defines the isotropic scaling.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *
 * \sa VersorRigid3DTransform
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT Similarity3DTransform :
  public VersorRigid3DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef Similarity3DTransform                        Self;
  typedef VersorRigid3DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Similarity3DTransform, VersorRigid3DTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 7);

  /** Parameters Type   */
  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::ScalarType                ScalarType;
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
  typedef typename Superclass::AxisType   AxisType;
  typedef typename Superclass::AngleType  AngleType;
  typedef          TParametersValueType   ScaleType;

  /** Set the parameters to the IdentityTransform */
  virtual void SetIdentity(void) ITK_OVERRIDE;

  /** Directly set the rotation matrix of the transform.
   *
   * \warning The input matrix must be orthogonal with isotropic scaling
   * to within a specified tolerance, else an exception is thrown.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType & matrix) ITK_OVERRIDE;

  /** Directly set the rotation matrix of the transform.
   *
   * \warning The input matrix must be orthogonal with isotropic scaling
   * to within the specified tolerance, else an exception is thrown.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance) ITK_OVERRIDE;

  /** Set the transformation from a container of parameters This is typically
   * used by optimizers.  There are 7 parameters. The first three represent the
   * versor, the next three represent the translation and the last one
   * represents the scaling factor. */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  virtual const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  /** Set/Get the value of the isotropic scaling factor */
  void SetScale(ScaleType scale);

  itkGetConstReferenceMacro(Scale, ScaleType);

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

protected:
  Similarity3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  Similarity3DTransform(unsigned int paramDim);
  Similarity3DTransform();
  ~Similarity3DTransform() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Recomputes the matrix by calling the Superclass::ComputeMatrix() and then
   * applying the scale factor. */
  void ComputeMatrix() ITK_OVERRIDE;

  /** Computes the parameters from an input matrix. */
  void ComputeMatrixParameters() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Similarity3DTransform);

  ScaleType m_Scale;
}; // class Similarity3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarity3DTransform.hxx"
#endif

#endif /* itkSimilarity3DTransform_h */
