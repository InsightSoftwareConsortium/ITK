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
#ifndef itkVelocityFieldTransform_h
#define itkVelocityFieldTransform_h

#include "itkDisplacementFieldTransform.h"

namespace itk
{

/** \class VelocityFieldTransform
 * \brief Provides local/dense/high-dimensionality transformation via a
 * a velocity field.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template <typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT VelocityFieldTransform : public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VelocityFieldTransform);

  /** Standard class type aliases. */
  using Self = VelocityFieldTransform;
  using Superclass = DisplacementFieldTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VelocityFieldTransform, DisplacementFieldTransform);

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

  /** Dimension of the velocity field . */
  static constexpr unsigned int VelocityFieldDimension = NDimensions + 1;

  /** Dimension of the vector spaces. */
  static constexpr unsigned int Dimension = NDimensions;

  /** Define the displacement field type and corresponding interpolator type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;

  /** Define the displacement field type and corresponding interpolator type. */
  using VelocityFieldType = Image<OutputVectorType, VelocityFieldDimension>;
  using VelocityFieldPointer = typename VelocityFieldType::Pointer;

  /** Standard types for the velocity Field */
  using IndexType = typename VelocityFieldType::IndexType;
  using RegionType = typename VelocityFieldType::RegionType;
  using SizeType = typename VelocityFieldType::SizeType;
  using SpacingType = typename VelocityFieldType::SpacingType;
  using DirectionType = typename VelocityFieldType::DirectionType;
  using PointType = typename VelocityFieldType::PointType;
  using PixelType = typename VelocityFieldType::PixelType;

  using VelocityFieldInterpolatorType = VectorInterpolateImageFunction<VelocityFieldType, ScalarType>;
  using VelocityFieldInterpolatorPointer = typename VelocityFieldInterpolatorType::Pointer;

  /** Define the internal parameter helper used to access the field */
  using OptimizerParametersHelperType =
    ImageVectorOptimizerParametersHelper<ScalarType, Dimension, VelocityFieldDimension>;

  /** Get/Set the velocity field.
   * Set the displacement field. Create special set accessor to update
   * interpolator and assign displacement field to transform parameters
   * container. */
  virtual void
  SetVelocityField(VelocityFieldType *);
  itkGetModifiableObjectMacro(VelocityField, VelocityFieldType);

  void
  SetFixedParameters(const FixedParametersType &) override;

  /** Get/Set the interpolator.
   * Create out own set accessor that assigns the velocity field */
  virtual void
  SetVelocityFieldInterpolator(VelocityFieldInterpolatorType *);
  itkGetModifiableObjectMacro(VelocityFieldInterpolator, VelocityFieldInterpolatorType);

  /** Get the modification time of velocity field */
  itkGetConstReferenceMacro(VelocityFieldSetTime, unsigned long);

  /**
   * Set the deformation field. We want to override the base class
   * implementation since we don't want to optimize over the deformation
   * field for this class but rather the time-varying velocity field
   */
  void
  SetDisplacementField(DisplacementFieldType * displacementField) override
  {
    itkDebugMacro("setting DisplacementField to " << displacementField);
    if (this->m_DisplacementField != displacementField)
    {
      this->m_DisplacementField = displacementField;
      this->Modified();
    }
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /** Return an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /** Trigger the computation of the displacement field by integrating the velocity field. */
  virtual void
  IntegrateVelocityField(){};

  /**
   * Set the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro(LowerTimeBound, ScalarType, 0.0, 1.0);

  /**
   * Get the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro(LowerTimeBound, ScalarType);

  /**
   * Set the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro(UpperTimeBound, ScalarType, 0.0, 1.0);

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
  VelocityFieldTransform();
  ~VelocityFieldTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Clone the current transform */
  typename LightObject::Pointer
  InternalClone() const override;

  typename DisplacementFieldType::Pointer
  CopyDisplacementField(const DisplacementFieldType *) const;

  ScalarType m_LowerTimeBound;
  ScalarType m_UpperTimeBound;

  unsigned int m_NumberOfIntegrationSteps;

  VelocityFieldPointer m_VelocityField;

  /** The interpolator. */
  typename VelocityFieldInterpolatorType::Pointer m_VelocityFieldInterpolator;

  /** Track when the VELOCITY field was last set/assigned, as
   * distinct from when it may have had its contents modified. */
  unsigned long m_VelocityFieldSetTime;

private:
  /**
   * Convenience method which reads the information from the current
   * velocity field into m_FixedParameters.
   */
  virtual void
  SetFixedParametersFromVelocityField() const;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVelocityFieldTransform.hxx"
#endif

#endif // itkVelocityFieldTransform_h
