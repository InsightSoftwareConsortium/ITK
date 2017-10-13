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
#ifndef itkConstantVelocityFieldTransform_h
#define itkConstantVelocityFieldTransform_h

#include "itkDisplacementFieldTransform.h"

namespace itk
{

/** \class ConstantVelocityFieldTransform
 * \brief Provides local/dense/high-dimensionality transformation via a
 * a constant velocity field.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template
<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT ConstantVelocityFieldTransform :
  public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef ConstantVelocityFieldTransform                                Self;
  typedef DisplacementFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( ConstantVelocityFieldTransform, DisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** InverseTransform type. */
  typedef typename Superclass:: InverseTransformBasePointer InverseTransformBasePointer;

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Type of the input parameters. */
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;

  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType TransformCategoryType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType  InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

  typedef typename Superclass::InputVectorPixelType  InputVectorPixelType;
  typedef typename Superclass::OutputVectorPixelType OutputVectorPixelType;

  /** Derivative type */
  typedef typename Superclass::DerivativeType DerivativeType;

  /** Dimension of the constant velocity field . */
  itkStaticConstMacro( ConstantVelocityFieldDimension, unsigned int, NDimensions );

  /** Dimension of the vector spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Define the displacement field type and corresponding interpolator type. */
  typedef typename Superclass::DisplacementFieldType   DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer      DisplacementFieldPointer;

  /** Define the displacement field type and corresponding interpolator type. */
  typedef Image<OutputVectorType, ConstantVelocityFieldDimension>    ConstantVelocityFieldType;
  typedef typename ConstantVelocityFieldType::Pointer                ConstantVelocityFieldPointer;

  /** Standard types for the velocity Field */
  typedef typename ConstantVelocityFieldType::IndexType      IndexType;
  typedef typename ConstantVelocityFieldType::RegionType     RegionType;
  typedef typename ConstantVelocityFieldType::SizeType       SizeType;
  typedef typename ConstantVelocityFieldType::SpacingType    SpacingType;
  typedef typename ConstantVelocityFieldType::DirectionType  DirectionType;
  typedef typename ConstantVelocityFieldType::PointType      PointType;
  typedef typename ConstantVelocityFieldType::PixelType      PixelType;

  typedef ConstantVelocityFieldType                         VelocityFieldType;

  typedef VectorInterpolateImageFunction<ConstantVelocityFieldType, ScalarType>
    ConstantVelocityFieldInterpolatorType;
  typedef typename ConstantVelocityFieldInterpolatorType::Pointer
    ConstantVelocityFieldInterpolatorPointer;

  /** Define the internal parameter helper used to access the field */
  typedef ImageVectorOptimizerParametersHelper<ScalarType, Dimension, ConstantVelocityFieldDimension>
    OptimizerParametersHelperType;

  /** Get/Set the velocity field.
   * Set the displacement field. Create special set accessor to update
   * interpolator and assign displacement field to transform parameters
   * container. */
  virtual void SetConstantVelocityField( ConstantVelocityFieldType * );
  itkGetModifiableObjectMacro(ConstantVelocityField, ConstantVelocityFieldType );

  virtual void SetFixedParameters( const FixedParametersType & ) ITK_OVERRIDE;

  /** Get/Set the interpolator.
   * Create out own set accessor that assigns the velocity field */
  virtual void SetConstantVelocityFieldInterpolator( ConstantVelocityFieldInterpolatorType * );
  itkGetModifiableObjectMacro(ConstantVelocityFieldInterpolator, ConstantVelocityFieldInterpolatorType );

  /** Get the modification time of velocity field */
  itkGetConstReferenceMacro( ConstantVelocityFieldSetTime, ModifiedTimeType );

  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /** Return an inverse of this transform. */
  bool GetInverse( Self *inverse ) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /** Trigger the computation of the displacement field by integrating
   * the constant velocity field. */
  virtual void IntegrateVelocityField();

  // Set/get compute number of exp. integration steps automatically
  itkSetMacro( CalculateNumberOfIntegrationStepsAutomatically, bool );
  itkGetConstMacro( CalculateNumberOfIntegrationStepsAutomatically, bool );
  itkBooleanMacro( CalculateNumberOfIntegrationStepsAutomatically );

  /**
   * Set the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro( LowerTimeBound, ScalarType, 0, 1 );

  /**
   * Get the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro( LowerTimeBound, ScalarType );

  /**
   * Set the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro( UpperTimeBound, ScalarType, 0, 1 );

  /**
   * Get the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro( UpperTimeBound, ScalarType );

  /**
   * Set the number of integration steps.  Default = 100;
   */
  itkSetMacro( NumberOfIntegrationSteps, unsigned int );

  /**
   * Get the number of integration steps.  Default = 100;
   */
  itkGetConstMacro( NumberOfIntegrationSteps, unsigned int );

protected:

  ConstantVelocityFieldTransform();
  virtual ~ConstantVelocityFieldTransform() ITK_OVERRIDE;
  void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** Clone the current transform */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  typename DisplacementFieldType::Pointer CopyDisplacementField( const DisplacementFieldType * ) const;

  ConstantVelocityFieldPointer              m_ConstantVelocityField;

  bool                                      m_CalculateNumberOfIntegrationStepsAutomatically;

  /** The interpolator. */
  ConstantVelocityFieldInterpolatorPointer  m_ConstantVelocityFieldInterpolator;

  /** Track when the VELOCITY field was last set/assigned, as
   * distinct from when it may have had its contents modified. */
  ModifiedTimeType m_ConstantVelocityFieldSetTime;

  ScalarType                                m_LowerTimeBound;
  ScalarType                                m_UpperTimeBound;

  unsigned int                              m_NumberOfIntegrationSteps;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConstantVelocityFieldTransform);

  /**
   * Convenience method which reads the information from the current
   * velocity field into m_FixedParameters.
   */
  virtual void SetFixedParametersFromConstantVelocityField() const;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstantVelocityFieldTransform.hxx"
#endif

#endif // itkConstantVelocityFieldTransform_h
