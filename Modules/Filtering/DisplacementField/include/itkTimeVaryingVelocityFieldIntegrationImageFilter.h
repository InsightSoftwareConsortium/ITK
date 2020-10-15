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
#ifndef itkTimeVaryingVelocityFieldIntegrationImageFilter_h
#define itkTimeVaryingVelocityFieldIntegrationImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkVectorInterpolateImageFunction.h"

namespace itk
{
/**
 * \class TimeVaryingVelocityFieldIntegrationImageFilter
 * \brief Integrate a time-varying velocity field using 4th order Runge-Kutta.
 *
 * Diffeomorphisms are topology-preserving mappings that are useful for
 * describing biologically plausible deformations.  Mathematically, a
 * diffeomorphism, \f$ \phi \f$, is generated from a time-varying velocity field, v, as
 * described by the integral equation:
 *
 * \f[
 * \phi(t_b) = \phi(t_a) + \int_{t_a}^{t_b} v(\phi(t),t) dt
 * \f]
 *
 * In this class, the input is the time-varying velocity field and an initial
 * diffeomorophism.  The output diffeomorphism is produced using fourth order
 * Runge-Kutta.
 *
 * \warning The output deformation field needs to have dimensionality of 1
 * less than the input time-varying velocity field.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template <typename TTimeVaryingVelocityField,
          typename TDisplacementField =
            Image<typename TTimeVaryingVelocityField::PixelType, TTimeVaryingVelocityField::ImageDimension - 1>>
class ITK_TEMPLATE_EXPORT TimeVaryingVelocityFieldIntegrationImageFilter
  : public ImageToImageFilter<TTimeVaryingVelocityField, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TimeVaryingVelocityFieldIntegrationImageFilter);

  using Self = TimeVaryingVelocityFieldIntegrationImageFilter;
  using Superclass = ImageToImageFilter<TTimeVaryingVelocityField, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(TimeVaryingVelocityFieldIntegrationImageFilter, ImageToImageFilter);

  /**
   * Dimensionality of input data is assumed to be one more than the output
   * data the same. */
  static constexpr unsigned int InputImageDimension = TTimeVaryingVelocityField::ImageDimension;

  static constexpr unsigned int OutputImageDimension = TDisplacementField::ImageDimension;

  using TimeVaryingVelocityFieldType = TTimeVaryingVelocityField;
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;
  using VectorType = typename DisplacementFieldType::PixelType;
  using RealType = typename VectorType::RealValueType;
  using ScalarType = typename VectorType::ValueType;
  using PointType = typename DisplacementFieldType::PointType;
  using OutputRegionType = typename DisplacementFieldType::RegionType;

  using VelocityFieldInterpolatorType = VectorInterpolateImageFunction<TimeVaryingVelocityFieldType, ScalarType>;
  using VelocityFieldInterpolatorPointer = typename VelocityFieldInterpolatorType::Pointer;

  using DisplacementFieldInterpolatorType = VectorInterpolateImageFunction<DisplacementFieldType, ScalarType>;
  using DisplacementFieldInterpolatorPointer = typename DisplacementFieldInterpolatorType::Pointer;

  /** Get/Set the time-varying velocity field interpolator.  Default = linear. */
  itkSetObjectMacro(VelocityFieldInterpolator, VelocityFieldInterpolatorType);
  itkGetModifiableObjectMacro(VelocityFieldInterpolator, VelocityFieldInterpolatorType);

  /**
   * Get/Set the deformation field interpolator for the initial diffeomorphism
   * (if set).  Default = linear.
   */
  itkSetObjectMacro(DisplacementFieldInterpolator, DisplacementFieldInterpolatorType);
  itkGetModifiableObjectMacro(DisplacementFieldInterpolator, DisplacementFieldInterpolatorType);

  /**
   * Get/Set the initial diffeomorphism
   */
  itkSetObjectMacro(InitialDiffeomorphism, DisplacementFieldType);
  itkGetModifiableObjectMacro(InitialDiffeomorphism, DisplacementFieldType);

  /**
   * Set the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkSetClampMacro(LowerTimeBound, RealType, 0, 1);

  /**
   * Get the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkGetConstMacro(LowerTimeBound, RealType);

  /**
   * Set the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkSetClampMacro(UpperTimeBound, RealType, 0, 1);

  /**
   * Get the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1].
   */
  itkGetConstMacro(UpperTimeBound, RealType);

  /**
   * Set the number of integration steps used in the Runge-Kutta solution of the
   * initial value problem.  Default = 10.
   */
  itkSetMacro(NumberOfIntegrationSteps, unsigned int);

  /**
   * Get the number of integration steps used in the Runge-Kutta solution of the
   * initial value problem.  Default = 10.
   */
  itkGetConstMacro(NumberOfIntegrationSteps, unsigned int);

protected:
  TimeVaryingVelocityFieldIntegrationImageFilter();
  ~TimeVaryingVelocityFieldIntegrationImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateOutputInformation() override;

  void
  BeforeThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputRegionType &) override;


  VectorType
  IntegrateVelocityAtPoint(const PointType & initialSpatialPoint, const TimeVaryingVelocityFieldType * inputField);

  RealType m_LowerTimeBound;
  RealType m_UpperTimeBound;

  DisplacementFieldPointer m_InitialDiffeomorphism;

  unsigned int m_NumberOfIntegrationSteps;

  unsigned int m_NumberOfTimePoints;

  DisplacementFieldInterpolatorPointer m_DisplacementFieldInterpolator;

private:
  VelocityFieldInterpolatorPointer m_VelocityFieldInterpolator;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTimeVaryingVelocityFieldIntegrationImageFilter.hxx"
#endif

#endif
