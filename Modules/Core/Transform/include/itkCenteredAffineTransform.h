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
#ifndef itkCenteredAffineTransform_h
#define itkCenteredAffineTransform_h

#include "itkAffineTransform.h"

namespace itk
{
/** \class CenteredAffineTransform
 * \brief Affine transformation with a specified center of rotation.
 *
 * This class implements an Affine transform in which the rotation center
 * can be explicitly selected.
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double,
         unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT CenteredAffineTransform : public AffineTransform<TParametersValueType,
                                             NDimensions>
{
public:
  /** Standard typedefs   */
  typedef CenteredAffineTransform                Self;
  typedef AffineTransform<TParametersValueType,
                           NDimensions >         Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CenteredAffineTransform, AffineTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro( ParametersDimension, unsigned int,
                       NDimensions * ( NDimensions + 2 ) );

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

  typedef typename Superclass::InputVnlVectorType    InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType   OutputVnlVectorType;
  typedef typename Superclass::InputPointType        InputPointType;
  typedef typename Superclass::InputPointValueType   InputPointValueType;
  typedef typename Superclass::OutputVectorValueType OutputVectorValueType;
  typedef typename Superclass::OutputPointType       OutputPointType;
  typedef typename Superclass::MatrixType            MatrixType;
  typedef typename Superclass::MatrixValueType       MatrixValueType;
  typedef typename Superclass::OffsetType            OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set/Get the transformation from a container of parameters.
   * The first (NDimension x NDimension) parameters define the
   * matrix, the next N parameters define the center of rotation
   * and the last N parameters define the translation to be applied
   * after the coordinate system has been restored to the rotation center.
   * Note that the Offset of the superclass is no longer in the
   * parameters array since it is fully dependent on the rotation
   * center and the translation parameters. */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

protected:
  /** Construct an CenteredAffineTransform object */
  CenteredAffineTransform();

  /** Destroy an CenteredAffineTransform object */
  virtual ~CenteredAffineTransform() ITK_OVERRIDE;

private:
  CenteredAffineTransform(const Self & other);
  const Self & operator=(const Self &);

}; // class CenteredAffineTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredAffineTransform.hxx"
#endif

#endif /* itkCenteredAffineTransform_h */
