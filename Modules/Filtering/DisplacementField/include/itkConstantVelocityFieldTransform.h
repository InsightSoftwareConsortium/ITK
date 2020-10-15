/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT ConstantVelocityFieldTransform
  : public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConstantVelocityFieldTransform);

  /** Standard class type aliases. */
  using Self = ConstantVelocityFieldTransform;
  using Superclass = DisplacementFieldTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConstantVelocityFieldTransform, DisplacementFieldTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** InverseTransform type. */
  using InverseTransformBasePointer = typename Superclass::InverseTransformBasePointer;

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Type of the input parameters. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;

  /** Transform category type. */
  using TransformCategoryEnum = typename Superclass::TransformCategoryEnum;

  /** The number of parameters defining this transform. */
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /** Standard coordinate point type for this class. */
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  /** Standard vector type for this class. */
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;

  using InputVectorPixelType = typename Superclass::InputVectorPixelType;
  using OutputVectorPixelType = typename Superclass::OutputVectorPixelType;

  /** Derivative type */
  using DerivativeType = typename Superclass::DerivativeType;

  /** Dimension of the constant velocity field . */
  static constexpr unsigned int ConstantVelocityFieldDimension = NDimensions;

  /** Dimension of the vector spaces. */
  static constexpr unsigned int Dimension = NDimensions;

  /** Define the displacement field type and corresponding interpolator type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;

  /** Define the displacement field type and corresponding interpolator type. */
  using ConstantVelocityFieldType = Image<OutputVectorType, ConstantVelocityFieldDimension>;
  using ConstantVelocityFieldPointer = typename ConstantVelocityFieldType::Pointer;

  /** Standard types for the velocity Field */
  using IndexType = typename ConstantVelocityFieldType::IndexType;
  using RegionType = typename ConstantVelocityFieldType::RegionType;
  using SizeType = typename ConstantVelocityFieldType::SizeType;
  using SpacingType = typename ConstantVelocityFieldType::SpacingType;
  using DirectionType = typename ConstantVelocityFieldType::DirectionType;
  using PointType = typename ConstantVelocityFieldType::PointType;
  using PixelType = typename ConstantVelocityFieldType::PixelType;

  using VelocityFieldType = ConstantVelocityFieldType;

  using ConstantVelocityFieldInterpolatorType = VectorInterpolateImageFunction<ConstantVelocityFieldType, ScalarType>;
  using ConstantVelocityFieldInterpolatorPointer = typename ConstantVelocityFieldInterpolatorType::Pointer;

  /** Define the internal parameter helper used to access the field */
  using OptimizerParametersHelperType =
    ImageVectorOptimizerParametersHelper<ScalarType, Dimension, ConstantVelocityFieldDimension>;

  /** Get/Set the velocity field.
   * Set the displacement field. Create special set accessor to update
   * interpolator and assign displacement field to transform parameters
   * container. */
  virtual void
  SetConstantVelocityField(ConstantVelocityFieldType *);
  itkGetModifiableObjectMacro(ConstantVelocityField, ConstantVelocityFieldType);

  void
  SetFixedParameters(const FixedParametersType &) override;

  /** Get/Set the interpolator.
   * Create out own set accessor that assigns the velocity field */
  virtual void
  SetConstantVelocityFieldInterpolator(ConstantVelocityFieldInterpolatorType *);
  itkGetModifiableObjectMacro(ConstantVelocityFieldInterpolator, ConstantVelocityFieldInterpolatorType);

  /** Get the modification time of velocity field */
  itkGetConstReferenceMacro(ConstantVelocityFieldSetTime, ModifiedTimeType);

  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /** Return an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /** Trigger the computation of the displacement field by integrating
   * the constant velocity field. */
  virtual void
  IntegrateVelocityField();

  // Set/get compute number of exp. integration steps automatically
  itkSetMacro(CalculateNumberOfIntegrationStepsAutomatically, bool);
  itkGetConstMacro(CalculateNumberOfIntegrationStepsAutomatically, bool);
  itkBooleanMacro(CalculateNumberOfIntegrationStepsAutomatically);

  /**
   * Set the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro(LowerTimeBound, ScalarType, 0, 1);

  /**
   * Get the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro(LowerTimeBound, ScalarType);

  /**
   * Set the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro(UpperTimeBound, ScalarType, 0, 1);

  /**
   * Get the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro(UpperTimeBound, ScalarType);

  /**
   * Set the number of integration steps.  Default = 100;
   */
  itkSetMacro(NumberOfIntegrationSteps, unsigned int);

  /**
   * Get the number of integration steps.  Default = 100;
   */
  itkGetConstMacro(NumberOfIntegrationSteps, unsigned int);

protected:
  ConstantVelocityFieldTransform();
  ~ConstantVelocityFieldTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Clone the current transform */
  typename LightObject::Pointer
  InternalClone() const override;

  typename DisplacementFieldType::Pointer
  CopyDisplacementField(const DisplacementFieldType *) const;

  ConstantVelocityFieldPointer m_ConstantVelocityField;

  bool m_CalculateNumberOfIntegrationStepsAutomatically{ false };

  /** The interpolator. */
  ConstantVelocityFieldInterpolatorPointer m_ConstantVelocityFieldInterpolator;

  /** Track when the VELOCITY field was last set/assigned, as
   * distinct from when it may have had its contents modified. */
  ModifiedTimeType m_ConstantVelocityFieldSetTime{ 0 };

  ScalarType m_LowerTimeBound;
  ScalarType m_UpperTimeBound;

  unsigned int m_NumberOfIntegrationSteps;

private:
  /**
   * Convenience method which reads the information from the current
   * velocity field into m_FixedParameters.
   */
  virtual void
  SetFixedParametersFromConstantVelocityField() const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConstantVelocityFieldTransform.hxx"
#endif

#endif // itkConstantVelocityFieldTransform_h
