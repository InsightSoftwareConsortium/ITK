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
#ifndef itkCenteredEuler3DTransform_h
#define itkCenteredEuler3DTransform_h

#include <iostream>
#include "itkEuler3DTransform.h"
#include "itkMacro.h"
#include "itkVersor.h"

namespace itk
{
/** \class CenteredEuler3DTransform
 * \brief CenteredEuler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation about a specific coordinate or
 * centre of rotation followed by a translation.
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT CenteredEuler3DTransform :
  public Euler3DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef CenteredEuler3DTransform               Self;
  typedef Euler3DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredEuler3DTransform, Euler3DTransform);

  /** Dimension of the space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 9);

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

  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;
  typedef typename Superclass::InputPointType       InputPointType;
  typedef typename Superclass::OutputPointType      OutputPointType;
  typedef typename Superclass::MatrixType           MatrixType;
  typedef typename Superclass::InverseMatrixType    InverseMatrixType;
  typedef typename Superclass::CenterType           CenterType;
  typedef typename Superclass::TranslationType      TranslationType;
  typedef typename Superclass::TranslationValueType TranslationValueType;
  typedef typename Superclass::OffsetType           OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.  There are nine parameters. The first
   * three represent the angles of rotation (in radians) around each one of the
   * axes (X,Y,Z), the next three parameters represent the coordinates of the
   * center of rotation and the last three parameters represent the
   * translation. */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers. There are nine parameters. The first
   * three represent the angles of rotation (in radians) around each one of the
   * axes (X,Y,Z), the next three parameters represent the coordinates of the
   * center of rotation and the last three parameters represent the
   * translation. */
  const ParametersType & GetParameters(void) const ITK_OVERRIDE;

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the
   * transform is invertible at this point. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

protected:
  CenteredEuler3DTransform();
  CenteredEuler3DTransform(const MatrixType & matrix, const OutputPointType & offset);
  CenteredEuler3DTransform(unsigned int ParametersDimension);
  ~CenteredEuler3DTransform() ITK_OVERRIDE;

  /**
   * Print contents of an CenteredEuler3DTransform
   */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredEuler3DTransform);

};                                        // class CenteredEuler3DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredEuler3DTransform.hxx"
#endif

#endif /* itkCenteredEuler3DTransform_h */
