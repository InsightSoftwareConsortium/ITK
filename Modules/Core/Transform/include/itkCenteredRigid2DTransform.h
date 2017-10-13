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
#ifndef itkCenteredRigid2DTransform_h
#define itkCenteredRigid2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{
/** \class CenteredRigid2DTransform
 * \brief CenteredRigid2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rigid transformation is 2D space.
 * The transform is specified as a rotation around arbitrary center
 * and is followed by a translation.
 *
 * The main difference between this class and its superclass
 * Rigid2DTransform is that the center of rotation is exposed
 * for optimization.
 *
 * The serialization of the optimizable parameters is an array of 5 elements
 * ordered as follows:
 * p[0] = angle
 * p[1] = x coordinate of the center
 * p[2] = y coordinate of the center
 * p[3] = x component of the translation
 * p[4] = y component of the translation
 *
 * There are no fixed parameters.
 *
 * \sa Rigid2DTransform
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT CenteredRigid2DTransform :
  public Rigid2DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef CenteredRigid2DTransform               Self;
  typedef Rigid2DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredRigid2DTransform, Rigid2DTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 2);
  itkStaticConstMacro(ParametersDimension, unsigned int, 5);

  /** Parameters type. */
  typedef typename Superclass::ScalarType               ScalarType;
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Offset type. */
  typedef typename Superclass::OffsetType OffsetType;

  /** Point type. */
  typedef typename Superclass::InputPointType      InputPointType;
  typedef typename Superclass::OutputPointType     OutputPointType;
  typedef typename Superclass::InputPointValueType InputPointValueType;

  /** Vector type. */
  typedef typename Superclass::InputVectorType       InputVectorType;
  typedef typename Superclass::OutputVectorType      OutputVectorType;
  typedef typename Superclass::OutputVectorValueType OutputVectorValueType;

  /** CovariantVector type. */
  typedef typename Superclass::InputCovariantVectorType
  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType
  OutputCovariantVectorType;

  /** VnlVector type. */
  typedef typename Superclass::InputVnlVectorType  InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType OutputVnlVectorType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 5 parameters. The first one represents the
   * rotation, the next two the center of rotation and
   * the last two represents the offset.
   *
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 3 parameters. The first one represents the
   * rotation, the next two the center of rotation and
   * the last two represents the offset.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** This method computes the Jacobian matrix of the transformation
   * at a given input point.
   */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /** Set the fixed parameters and update internal transformation.
   * This is a null function as there are no fixed parameters. */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE;

  /** Get the Fixed Parameters. An empty array is returned
   * as there are no fixed parameters. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;

  /**
   * This method creates and returns a new CenteredRigid2DTransform object
   * which is the inverse of self. */
  void CloneInverseTo(Pointer & newinverse) const;

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /**
   * This method creates and returns a new CenteredRigid2DTransform object
   * which has the same parameters as self. */
  void CloneTo(Pointer & clone) const;

protected:
  CenteredRigid2DTransform();
  ~CenteredRigid2DTransform() ITK_OVERRIDE {}

  CenteredRigid2DTransform(unsigned int outputSpaceDimension, unsigned int parametersDimension);

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredRigid2DTransform);

};                                        // class CenteredRigid2DTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredRigid2DTransform.hxx"
#endif

#endif /* itkCenteredRigid2DTransform_h */
