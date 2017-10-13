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
#ifndef itkScalableAffineTransform_h
#define itkScalableAffineTransform_h

#include "itkAffineTransform.h"

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
  unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT ScalableAffineTransform:
  public AffineTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard typedefs   */
  typedef ScalableAffineTransform                            Self;
  typedef AffineTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                 Pointer;
  typedef SmartPointer<const Self>                           ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(ScalableAffineTransform, AffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro( ParametersDimension, unsigned int,
                       NDimensions * ( NDimensions + 1 ) );

  /** Types taken from the Superclass */
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
  typedef typename Superclass::MatrixValueType           MatrixValueType;
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::OffsetType                OffsetType;
  typedef typename Superclass::TranslationType           TranslationType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.  */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set the transformation to an Identity
   *
   * This sets the matrix to identity and the Offset to null. */
  void SetIdentity(void) ITK_OVERRIDE;

  /** Set the scale of the transform */
  virtual void SetScale(const InputVectorType & scale);

  virtual void SetScaleComponent(const InputVectorType & scale)
  { this->SetScale(scale); }

  /** Set the scale of the transform */
  virtual void SetScale(const double scale[NDimensions]);

  virtual void SetScaleComponent(const double scale[NDimensions])
  { this->SetScale(scale); }

  /** Get the scale of the transform */
  virtual const double * GetScale() const
  { return m_Scale; }
  virtual const double * GetScaleComponent() const
  { return m_Scale; }

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /** Set the matrix of the transform. The matrix should not include
   *  scale.
   *  \deprecated use SetMatrix instead */
  itkLegacyMacro(void SetMatrixComponent(const MatrixType & matrix));

  /** Get matrix of the transform.
   * \deprecated use GetMatrix instead  */
  itkLegacyMacro(const MatrixType & GetMatrixComponent() const);

  /** Set offset (origin) of the Transform.
   * \deprecated use SetTranslation instead. */
  itkLegacyMacro(void SetOffsetComponent(const OffsetType & offset) );

  /** Get offset of the transform
   * \deprecated use GetTranslation instead. */
  itkLegacyMacro(const OffsetType & GetOffsetComponent(void) const );

protected:
  /** Construct an ScalableAffineTransform object
   *
   * This method constructs a new AffineTransform object and
   * initializes the matrix and offset parts of the transformation
   * to values specified by the caller.  If the arguments are
   * omitted, then the AffineTransform is initialized to an identity
   * transformation in the appropriate number of dimensions. */
  ScalableAffineTransform(const MatrixType & matrix,
                          const OutputVectorType & offset);
  ScalableAffineTransform(unsigned int outputSpaceDimension,
                          unsigned int parametersDimension);
  ScalableAffineTransform(unsigned int parametersDimension);
  ScalableAffineTransform();

  void ComputeMatrix() ITK_OVERRIDE;

  /** Destroy an ScalableAffineTransform object   */
  virtual ~ScalableAffineTransform() ITK_OVERRIDE;

  /** Print contents of an ScalableAffineTransform */
  void PrintSelf(std::ostream & s, Indent indent) const ITK_OVERRIDE;

  void SetVarScale(const double *scale)
  { for ( int i = 0; i < InputSpaceDimension; i++ ) { m_Scale[i] = scale[i]; } }

private:

  ScalableAffineTransform(const Self & other);
  const Self & operator=(const Self &);

  double          m_Scale[NDimensions];
  InputVectorType m_MatrixScale;
}; //class ScalableAffineTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalableAffineTransform.hxx"
#endif

#endif /* itkScalableAffineTransform_h */
