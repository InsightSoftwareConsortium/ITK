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
#ifndef itkGPUDemonsRegistrationFunction_h
#define itkGPUDemonsRegistrationFunction_h

#include "itkGPUPDEDeformableRegistrationFunction.h"
#include "itkPoint.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkGPUReduction.h"

namespace itk
{
/**
 * \class GPUDemonsRegistrationFunction
 *
 * This class encapsulate the PDE which drives the demons registration
 * algorithm. It is used by DemonsRegistrationFilter to compute the
 * output deformation field which will map a moving image onto a
 * a fixed image.
 *
 * Non-integer moving image values are obtained by using
 * interpolation. The default interpolator is of type
 * LinearInterpolateImageFunction. The user may set other
 * interpolators via method SetMovingImageInterpolator. Note that the input
 * interpolator must derive from baseclass InterpolateImageFunction.
 *
 * This class is templated over the fixed image type, moving image type,
 * and the deformation field type.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * \sa DemonsRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKGPUPDEDeformableRegistration
 */
/** Create a helper GPU Kernel class for GPUDemonsRegistrationFunction */
itkGPUKernelClassMacro(GPUDemonsRegistrationFunctionKernel);

template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT GPUDemonsRegistrationFunction
  : public GPUPDEDeformableRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUDemonsRegistrationFunction);

  /** Standard class type aliases. */
  using Self = GPUDemonsRegistrationFunction;
  using Superclass = GPUPDEDeformableRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUDemonsRegistrationFunction, DemonsRegistrationFunction);

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** FixedImage image type. */
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointer = typename Superclass::FixedImagePointer;
  using IndexType = typename FixedImageType::IndexType;
  using SizeType = typename FixedImageType::SizeType;
  using SpacingType = typename FixedImageType::SpacingType;

  /** Deformation field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldTypePointer = typename Superclass::DisplacementFieldTypePointer;

  /** Inherit some enums from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Inherit some enums from the superclass. */
  using PixelType = typename Superclass::PixelType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using TimeStepType = typename Superclass::TimeStepType;

  /** Interpolator type. */
  using CoordRepType = double;
  using InterpolatorType = InterpolateImageFunction<MovingImageType, CoordRepType>;
  using InterpolatorPointer = typename InterpolatorType::Pointer;
  using PointType = typename InterpolatorType::PointType;
  using DefaultInterpolatorType = LinearInterpolateImageFunction<MovingImageType, CoordRepType>;

  /** Covariant vector type. */
  using CovariantVectorType = CovariantVector<double, Self::ImageDimension>;

  /** Fixed image gradient calculator type. */
  using GradientCalculatorType = CentralDifferenceImageFunction<FixedImageType>;
  using GradientCalculatorPointer = typename GradientCalculatorType::Pointer;

  /** Moving image gradient calculator type. */
  using MovingImageGradientCalculatorType = CentralDifferenceImageFunction<MovingImageType, CoordRepType>;
  using MovingImageGradientCalculatorPointer = typename MovingImageGradientCalculatorType::Pointer;

  /** GPU data pointer type. */
  using GPUDataPointer = GPUDataManager::Pointer;

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUDemonsRegistrationFunctionKernel);

  /** Set the moving image interpolator. */
  void
  SetMovingImageInterpolator(InterpolatorType * ptr)
  {
    m_MovingImageInterpolator = ptr;
  }

  /** Get the moving image interpolator. */
  InterpolatorType *
  GetMovingImageInterpolator()
  {
    return m_MovingImageInterpolator;
  }

  /** This class uses a constant timestep of 1. */
  TimeStepType
  ComputeGlobalTimeStep(void * itkNotUsed(GlobalData)) const override
  {
    return m_TimeStep;
  }

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  void *
  GetGlobalDataPointer() const override
  {
    auto * global = new GlobalDataStruct();

    global->m_SumOfSquaredDifference = 0.0;
    global->m_NumberOfPixelsProcessed = 0L;
    global->m_SumOfSquaredChange = 0;
    return global;
  }

  /** Release memory for global data structure. */
  void
  ReleaseGlobalDataPointer(void * GlobalData) const override;

  /** Allocate GPU buffers for computing metric statistics
   * */
  void
  GPUAllocateMetricData(unsigned int numPixels) override;

  /** Release GPU buffers for computing metric statistics
   * */
  void
  GPUReleaseMetricData() override;

  /** Set the object's state before each iteration. */
  void
  InitializeIteration() override;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

  void
  GPUComputeUpdate(const DisplacementFieldTypePointer output, DisplacementFieldTypePointer update, void * gd) override;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images. */
  virtual double
  GetMetric() const
  {
    return m_Metric;
  }

  /** Get the rms change in deformation field. */
  virtual double
  GetRMSChange() const
  {
    return m_RMSChange;
  }

  /** Select if the fixed image or moving image gradient is used for
   * computing the demon forces. The fixed image gradient is used
   * by default. */
  virtual void
  SetUseMovingImageGradient(bool flag)
  {
    m_UseMovingImageGradient = flag;
  }
  virtual bool
  GetUseMovingImageGradient() const
  {
    return m_UseMovingImageGradient;
  }

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(double);

  virtual double
  GetIntensityDifferenceThreshold() const;

protected:
  GPUDemonsRegistrationFunction();
  ~GPUDemonsRegistrationFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** FixedImage image neighborhood iterator type. */
  using FixedImageNeighborhoodIteratorType = ConstNeighborhoodIterator<FixedImageType>;

  /** A global data type for this class of equation. Used to store
   * information for computing the metric. */
  struct GlobalDataStruct
  {
    double        m_SumOfSquaredDifference;
    SizeValueType m_NumberOfPixelsProcessed;
    double        m_SumOfSquaredChange;
  };

  /* GPU kernel handle for GPUComputeUpdate */
  int m_ComputeUpdateGPUKernelHandle;

private:
  /** Cache fixed image information. */
  // SpacingType                  m_FixedImageSpacing;
  // PointType                    m_FixedImageOrigin;
  PixelType m_ZeroUpdateReturn;
  double    m_Normalizer;

  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the moving image. */
  MovingImageGradientCalculatorPointer m_MovingImageGradientCalculator;
  bool                                 m_UseMovingImageGradient;

  /** Function to interpolate the moving image. */
  InterpolatorPointer m_MovingImageInterpolator;

  /** The global timestep. */
  TimeStepType m_TimeStep;

  /** Threshold below which the denominator term is considered zero. */
  double m_DenominatorThreshold;

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold;

  /** The metric value is the mean square difference in intensity between
   * the fixed image and transforming moving image computed over the
   * the overlapping region between the two images. */
  mutable double        m_Metric;
  mutable double        m_SumOfSquaredDifference;
  mutable SizeValueType m_NumberOfPixelsProcessed;
  mutable double        m_RMSChange;
  mutable double        m_SumOfSquaredChange;

  mutable GPUReduction<int>::Pointer   m_GPUPixelCounter;
  mutable GPUReduction<float>::Pointer m_GPUSquaredChange;
  mutable GPUReduction<float>::Pointer m_GPUSquaredDifference;

  /** Mutex lock to protect modification to metric. */
  mutable std::mutex m_MetricCalculationLock;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUDemonsRegistrationFunction.hxx"
#endif

#endif
