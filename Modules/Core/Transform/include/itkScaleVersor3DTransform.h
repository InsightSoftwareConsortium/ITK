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
#ifndef itkScaleVersor3DTransform_h
#define itkScaleVersor3DTransform_h

#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class ScaleVersor3DTransform
 *
 * \brief This transform applies a Versor rotation, translation and
 * anisotropic scale to the space.
 *
 * The transform can be described as:
 * \f$ (\textbf{R}_v + \textbf{S})\textbf{x} \f$ where \f$\textbf{R}_v\f$ is the
 * rotation matrix given the versor, and
 * \f$S=\left( \begin{array}{ccc}s_0-1 & 0 & 0 \\ 0 & s_1-1 & 0 \\ 0 & 0 & s_2-1 \end{array} \right)\ \f$
 *
 *
 * \note This transform's scale parameters are not related to the
 * uniform scaling parameter of the Similarity3DTransform.
 *
 * \author Johnson H.J., Harris G., Williams K. University of Iowa Carver
 * College of Medicine, Department of Psychiatry NeuroImaging Center
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/1291  or
 * http://www.insight-journal.org/browse/publication/180
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT ScaleVersor3DTransform : public VersorRigid3DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef ScaleVersor3DTransform                       Self;
  typedef VersorRigid3DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScaleVersor3DTransform, VersorRigid3DTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(InputSpaceDimension,  unsigned int,  3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int,  3);
  itkStaticConstMacro(ParametersDimension,  unsigned int,  9);

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

  /** Scale Vector Type. */
  typedef Vector<TParametersValueType, 3> ScaleVectorType;

  /** Directly set the matrix of the transform.
   *
   * Orthogonality testing is bypassed in this case.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType & matrix) ITK_OVERRIDE;
  virtual void SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance) ITK_OVERRIDE;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 9 parameters:
   *   0-2   versor
   *   3-5   translation
   *   6-8   Scale
   **  */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  virtual const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  /** Set/Get the scale vector. These scale factors are associated to the axis
   * of coordinates. */
  void SetScale(const ScaleVectorType & scale);

  itkGetConstReferenceMacro(Scale, ScaleVectorType);

  /** Set the internal parameters of the transform in order to represent an
   * Identity transform. */
  void SetIdentity() ITK_OVERRIDE;

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

protected:
  ScaleVersor3DTransform();
  ScaleVersor3DTransform(const MatrixType & matrix, const OutputVectorType & offset);
  ScaleVersor3DTransform(unsigned int paramDims);
  ~ScaleVersor3DTransform() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void SetVarScale(const ScaleVectorType & scale)
  {
    m_Scale = scale;
  }

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrix(void) ITK_OVERRIDE;

  void ComputeMatrixParameters(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScaleVersor3DTransform);

  /**  Vector containing the scale. */
  ScaleVectorType m_Scale;
}; // class ScaleVersor3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScaleVersor3DTransform.hxx"
#endif

#endif /* __ScaleVersor3DTransform_h */
