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
#ifndef itkFixedCenterOfRotationAffineTransform_h
#define itkFixedCenterOfRotationAffineTransform_h

#include "itkScalableAffineTransform.h"

namespace itk
{
/**
 * \brief Affine transformation with a specified center of rotation.
 *
 * This class implements an Affine transform in which the rotation center can be explicitly selected.
 *
 * \ingroup ITKTransform
 */

template<
  typename TParametersValueType=double,
  unsigned int NDimensions=3>
// Number of dimensions in the input space
class ITK_TEMPLATE_EXPORT FixedCenterOfRotationAffineTransform:
  public ScalableAffineTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard typedefs   */
  typedef FixedCenterOfRotationAffineTransform                         Self;
  typedef ScalableAffineTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                         Pointer;
  typedef SmartPointer<const Self>                                   ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(FixedCenterOfRotationAffineTransform, ScalableAffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro( ParametersDimension, unsigned int,
                       NDimensions * ( NDimensions + 2 ) );

  /** Types taken from the Superclass */
  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
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

  /** Set and Get the center of rotation */
  void SetCenterOfRotationComponent(const InputPointType & cor)
  { this->SetCenter(cor); }
  InputPointType GetCenterOfRotationComponent(void) const
  { return this->GetCenter(); }

  /** Set the matrix of the transform. The matrix should not include
   *  scale */
  void SetMatrixComponent(const MatrixType & matrix)
  { this->SetMatrix(matrix); }
  /** Get matrix of the transform  */
  const MatrixType & GetMatrixComponent() const
  { return this->GetMatrix(); }

  /** Set offset (origin) of the Transform. */
  void SetOffsetComponent(const OffsetType & offset)
  { this->SetTranslation(offset); }

  /** Get offset of the transform. */
  const OffsetType & GetOffsetComponent(void) const
  { return this->GetTranslation(); }

protected:
  /** Construct an FixedCenterOfRotationAffineTransform object */
  FixedCenterOfRotationAffineTransform(const MatrixType & matrix,
                                       const OutputVectorType & offset);
  FixedCenterOfRotationAffineTransform(unsigned int outputSpaceDimension,
                                       unsigned int parametersDimension);
  FixedCenterOfRotationAffineTransform();

  /** Destroy an FixedCenterOfRotationAffineTransform object   */
  virtual ~FixedCenterOfRotationAffineTransform() ITK_OVERRIDE;

private:
  FixedCenterOfRotationAffineTransform(const Self & other);
  const Self & operator=(const Self &);
}; //class FixedCenterOfRotationAffineTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFixedCenterOfRotationAffineTransform.hxx"
#endif

#endif /* itkFixedCenterOfRotationAffineTransform_h */
