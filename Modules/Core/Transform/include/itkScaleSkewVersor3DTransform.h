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
#ifndef itkScaleSkewVersor3DTransform_h
#define itkScaleSkewVersor3DTransform_h

#include <iostream>
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class ScaleSkewVersor3DTransform
 * \brief ScaleSkewVersor3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a versor rotation and translation & scale/skew
 * to the space
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 15 elements.
 * The first 3 elements are the components of the versor representation
 * of 3D rotation. The next 3 parameters defines the translation in each
 * dimension. The next 3 parameters defines scaling in each dimension.
 * The last 6 parameters defines the skew.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *The transform can be described as:
 * \f$ (\textbf{R}_v + \textbf{S} + \textbf{K})\textbf{x}  \f$
 * where \f$\textbf{R}_v\f$ is the rotation matrix given the versor,
 * \f$S=\left( \begin{array}{ccc}s_0-1 & 0 & 0 \\ 0 & s_1-1 & 0 \\ 0 & 0 & s_2-1 \end{array} \right) \f$
 * , and
 * \f$K=\left( \begin{array}{ccc}0 & k_0 & k_1 \\ k_2 & 0 & k_3 \\ k_4 & k_5 & 0 \end{array} \right)\ \f$.
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT ScaleSkewVersor3DTransform :
  public VersorRigid3DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef ScaleSkewVersor3DTransform                   Self;
  typedef VersorRigid3DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScaleSkewVersor3DTransform, VersorRigid3DTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 15);

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

  typedef typename Superclass::VersorType VersorType;
  typedef typename Superclass::AxisType   AxisType;
  typedef typename Superclass::AngleType  AngleType;

  /** Scale & Skew Vector Type. */
  typedef Vector<TParametersValueType, 3> ScaleVectorType;
  typedef Vector<TParametersValueType, 6> SkewVectorType;

  typedef typename ScaleVectorType::ValueType ScaleVectorValueType;
  typedef typename SkewVectorType::ValueType  SkewVectorValueType;
  typedef typename TranslationType::ValueType TranslationValueType;

  typedef typename Superclass::AxisValueType       AxisValueType;
  typedef typename Superclass::ParametersValueType ParametersValueType;

  /** Directly set the matrix of the transform.
   *
   * Orthogonality testing is bypassed in this case.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType & matrix) ITK_OVERRIDE;
  virtual void SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance) ITK_OVERRIDE;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 15 parameters:
   *   0-2   versor
   *   3-5   translation
   *   6-8   Scale
   *   9-14  Skew
   **  */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  virtual const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  void SetScale(const ScaleVectorType & scale);

  itkGetConstReferenceMacro(Scale, ScaleVectorType);

  void SetSkew(const SkewVectorType & skew);

  itkGetConstReferenceMacro(Skew, SkewVectorType);

  void SetIdentity() ITK_OVERRIDE;

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

protected:
  ScaleSkewVersor3DTransform();
  ScaleSkewVersor3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  ScaleSkewVersor3DTransform(unsigned int paramDims);
  ~ScaleSkewVersor3DTransform() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void SetVarScale(const ScaleVectorType & scale)
  {
    m_Scale = scale;
  }

  void SetVarSkew(const SkewVectorType & skew)
  {
    m_Skew = skew;
  }

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrix(void) ITK_OVERRIDE;

  void ComputeMatrixParameters(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScaleSkewVersor3DTransform);

  /**  Vector containing the scale. */
  ScaleVectorType m_Scale;

  /**  Vector containing the skew */
  SkewVectorType m_Skew;
}; // class ScaleSkewVersor3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScaleSkewVersor3DTransform.hxx"
#endif

#endif /* __ScaleSkewVersor3DTransform_h */
