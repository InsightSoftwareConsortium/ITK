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
#ifndef itkCenteredSimilarity2DTransform_h
#define itkCenteredSimilarity2DTransform_h

#include "itkSimilarity2DTransform.h"

namespace itk
{
/** \class CenteredSimilarity2DTransform
 *  \brief CenteredSimilarity2DTransform of a vector space
 *        (e.g. space coordinates)
 *
 * This transform applies a homogenous scale and rigid transform in
 * 2D space. The transform is specified as a scale and rotation around
 * a arbitrary center and is followed by a translation.
 * given one angle for rotation, a homogeneous scale and a 2D offset
 * for translation.
 *
 * The main difference between this class and its superclass
 * Similarity2DTransform is that the center of transformation is exposed
 * for optimization.
 *
 * The serialization of the optimizable parameters is an array of 6 elements
 * ordered as follows:
 * p[0] = scale
 * p[1] = angle
 * p[2] = x coordinate of the center
 * p[3] = y coordinate of the center
 * p[4] = x component of the translation
 * p[5] = y component of the translation
 *
 * There are no fixed parameters.
 *
 * \sa Similarity2DTransform
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT CenteredSimilarity2DTransform :
  public Similarity2DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef CenteredSimilarity2DTransform               Self;
  typedef Similarity2DTransform<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredSimilarity2DTransform, Similarity2DTransform);

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension,           unsigned int, 2);
  itkStaticConstMacro(InputSpaceDimension,      unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension,     unsigned int, 2);
  itkStaticConstMacro(ParametersDimension,      unsigned int, 6);

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Parameters type. */
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Offset type. */
  typedef typename Superclass::OffsetType      OffsetType;
  typedef typename Superclass::OffsetValueType OffsetValueType;

  /** Point type. */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;
  typedef typename InputPointType::ValueType   InputPointValueType;

  /** Vector type. */
  typedef typename Superclass::InputVectorType  InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

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
    * There are 6 parameters. The first one represents the
    * scale, the second represents the angle of rotation, the next
    * two represent the center of the rotation
    * and the last two represent the translation.
    *
    * \sa Transform::SetParameters()
    * \sa Transform::SetFixedParameters() */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 6 parameters. The first one represents the
   * scale, the second represents the angle of rotation, the next
   * two represent the center of the rotation
   * and the last two represent the translation.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** Compute the Jacobian Matrix of the transformation at one point */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  /** Set the fixed parameters and update internal transformation.
   * This is a null function as there are no fixed parameters. */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE;

  /** Get the Fixed Parameters. An empty array is returned
   * as there are no fixed parameters. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;

  /**
   * This method creates and returns a new Rigid2DTransform object
   * which is the inverse of self. */
  void CloneInverseTo(Pointer & newinverse) const;

  /** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /**
   * This method creates and returns a new Rigid2DTransform object
   * which has the same parameters. */
  void CloneTo(Pointer & clone) const;

protected:
  CenteredSimilarity2DTransform();
  CenteredSimilarity2DTransform(unsigned int spaceDimension, unsigned int parametersDimension);

  virtual ~CenteredSimilarity2DTransform() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CenteredSimilarity2DTransform);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredSimilarity2DTransform.hxx"
#endif

#endif /* itkCenteredSimilarity2DTransform_h */
