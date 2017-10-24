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
#ifndef itkTimeVaryingVelocityFieldTransformParametersAdaptor_h
#define itkTimeVaryingVelocityFieldTransformParametersAdaptor_h

#include "itkTransformParametersAdaptor.h"

namespace itk
{
/** \class TimeVaryingVelocityFieldTransformParametersAdaptor
 * \brief TimeVaryingVelocityFieldTransformParametersAdaptor is a helper class intended to
 * definition.
 *
 * The fixed parameters store the following information:
 * field size
 * field origin
 * field spacing
 * field direction
 * During multiresolution image registration it is often desired to also increase
 * the displacement field resolution for greater flexibility in optimizing the
 * transform.  As defined in the base class, the user can change the resolution via
 *
 *   \code
 *   transformAdaptor->SetTransform( transform );
 *   transformAdaptor->SetRequiredFixedParameters( fixedParameters );
 *   transformAdaptor->AdaptTransformParameters();
 *   \endcode
 *
 * or the user can use the more intuitive API for setting the fixed parameters.
 * E.g., often the user will want to maintain the same transform domain spatial
 * extent but only increase the field size and decrease the spacing.  This can
 * be done as follows:
 *
 *   \code
 *   transformAdaptor->SetTransform( transform );
 *   transformAdaptor->SetRequiredOrigin( displacementField->GetOrigin() );
 *   transformAdaptor->SetRequiredDirection( displacementField->GetDirection() );
 *   transformAdaptor->SetRequiredSize( requiredSize );
 *   transformAdaptor->SetRequiredSpacing( requiredSpacing );
 *   transformAdaptor->AdaptTransformParameters();
 *   \endcode
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationCommon
 */
template<typename TTransform>
class ITK_TEMPLATE_EXPORT TimeVaryingVelocityFieldTransformParametersAdaptor
: public TransformParametersAdaptor<TTransform>
{
public:

  /** Standard class typedefs. */
  typedef TimeVaryingVelocityFieldTransformParametersAdaptor   Self;
  typedef TransformParametersAdaptor<TTransform>               Superclass;
  typedef SmartPointer<Self>                                   Pointer;
  typedef SmartPointer<const Self>                             ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( TimeVaryingVelocityFieldTransformParametersAdaptor, TransformParametersAdaptor );

  /** Typedefs associated with the transform */
  typedef TTransform                                     TransformType;
  typedef typename TransformType::Pointer                TransformPointer;
  typedef typename TransformType::ParametersType         ParametersType;
  typedef typename TransformType::ParametersValueType    ParametersValueType;

  typedef typename TransformType::TimeVaryingVelocityFieldType  TimeVaryingVelocityFieldType;
  typedef typename TimeVaryingVelocityFieldType::Pointer        TimeVaryingVelocityFieldPointer;
  typedef typename TimeVaryingVelocityFieldType::PointType      PointType;
  typedef typename TimeVaryingVelocityFieldType::SizeType       SizeType;
  typedef typename TimeVaryingVelocityFieldType::DirectionType  DirectionType;
  typedef typename TimeVaryingVelocityFieldType::SpacingType    SpacingType;

  /** Dimension of parameters. */
  itkStaticConstMacro( TotalDimension, unsigned int, TransformType::Dimension + 1 );

  /** Alternative method for setting the required size. */
  void SetRequiredSize( const SizeType & );

  /** Get the required size. */
  virtual const SizeType GetRequiredSize() const;

  /** Alternative method for setting the required origin. */
  void SetRequiredOrigin( const PointType & );

  /** Get the required origin. */
  virtual const PointType GetRequiredOrigin() const;

  /** Alternative method for setting the required spacing. */
  void SetRequiredSpacing( const SpacingType & );

  /** Get the required spacing. */
  virtual const SpacingType GetRequiredSpacing() const;

  /** Alternative method for setting the required direction. */
  void SetRequiredDirection( const DirectionType & );

  /** Get the required direction. */
  virtual const DirectionType GetRequiredDirection() const;

  /** Initialize the transform using the specified fixed parameters */
  virtual void AdaptTransformParameters() ITK_OVERRIDE;

protected:
  TimeVaryingVelocityFieldTransformParametersAdaptor();
  ~TimeVaryingVelocityFieldTransformParametersAdaptor() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TimeVaryingVelocityFieldTransformParametersAdaptor);

}; //class TimeVaryingVelocityFieldTransformParametersAdaptor
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTimeVaryingVelocityFieldTransformParametersAdaptor.hxx"
#endif

#endif /* itkTimeVaryingVelocityFieldTransformParametersAdaptor_h */
