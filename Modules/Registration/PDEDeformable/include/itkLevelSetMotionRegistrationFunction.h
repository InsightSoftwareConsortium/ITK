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
#ifndef itkLevelSetMotionRegistrationFunction_h
#define itkLevelSetMotionRegistrationFunction_h

#include "itkPDEDeformableRegistrationFunction.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include <mutex>

namespace itk
{
/**
 * \class LevelSetMotionRegistrationFunction
 *
 * This class encapsulate the PDE which drives the demons registration
 * algorithm. It is used by LevelSetMotionRegistrationFilter to compute the
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
 * \sa LevelSetMotionRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKPDEDeformableRegistration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT LevelSetMotionRegistrationFunction
  : public PDEDeformableRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetMotionRegistrationFunction);

  /** Standard class type aliases. */
  using Self = LevelSetMotionRegistrationFunction;
  using Superclass = PDEDeformableRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetMotionRegistrationFunction, PDEDeformableRegistrationFunction);

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;
  using MovingSpacingType = typename MovingImageType::SpacingType;

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

  /** Vector types. */
  using VectorType = Vector<double, Self::ImageDimension>;
  using CovariantVectorType = CovariantVector<double, Self::ImageDimension>;

  /** Moving image gradient calculator type. */
  using MovingImageSmoothingFilterType = SmoothingRecursiveGaussianImageFilter<MovingImageType>;
  using MovingImageSmoothingFilterPointer = typename MovingImageSmoothingFilterType::Pointer;

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

  /** Compute the time step that can taken for this iterations.  In
   * this context, the timestep is a function of the maximum gradients. */
  TimeStepType
  ComputeGlobalTimeStep(void * GlobalData) const override;

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  void *
  GetGlobalDataPointer() const override
  {
    auto * global = new GlobalDataStruct();

    global->m_SumOfSquaredDifference = 0.0;
    global->m_NumberOfPixelsProcessed = 0L;
    global->m_SumOfSquaredChange = 0;
    global->m_MaxL1Norm = NumericTraits<double>::NonpositiveMin();
    return global;
  }

  /** Release memory for global data structure. */
  void
  ReleaseGlobalDataPointer(void * gd) const override;

  /** Set the object's state before each iteration. */
  void
  InitializeIteration() override;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  PixelType
  ComputeUpdate(const NeighborhoodType & it, void * gd, const FloatOffsetType & offset = FloatOffsetType(0.0)) override;

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

  /** Set/Get the parameter alpha.  Alpha is added to the calculated
   * gradient magnitude prior to normalizing the gradient to protect
   * against numerical instability as the gradient magnitude
   * approaches zero.  This should be set as a small fraction of the
   * intensity dynamic range, for instance 0.04%. Default is the
   * absolute (not percentage) value of 0.1. */
  virtual void
  SetAlpha(double);

  virtual double
  GetAlpha() const;

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(double);

  virtual double
  GetIntensityDifferenceThreshold() const;

  /** Set/Get the threshold below which the gradient magnitude is
   * considered the zero vector. Default is 1e-9. */
  virtual void
  SetGradientMagnitudeThreshold(double);

  virtual double
  GetGradientMagnitudeThreshold() const;

  /** Set/Get the standard deviation used for smoothing the moving
   * image prior to calculating gradients. */
  virtual void
  SetGradientSmoothingStandardDeviations(double);

  virtual double
  GetGradientSmoothingStandardDeviations() const;

  /** Use the image spacing information in calculations. Use this option if you
   * want derivatives in physical space. Default is UseImageSpacing ON, due to a
   * backward compatibility state. */
  void
  SetUseImageSpacing(bool);

  bool
  GetUseImageSpacing() const;

protected:
  LevelSetMotionRegistrationFunction();
  ~LevelSetMotionRegistrationFunction() override = default;
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
    double        m_MaxL1Norm;
  };

private:
  /** Cache fixed image information. */
  SpacingType m_FixedImageSpacing;
  PointType   m_FixedImageOrigin;

  /** Function to compute derivatives of the moving image. */
  MovingImageSmoothingFilterPointer m_MovingImageSmoothingFilter;

  /** Function to interpolate the moving image. */
  InterpolatorPointer m_MovingImageInterpolator;
  InterpolatorPointer m_SmoothMovingImageInterpolator;

  /** Stabilization factor for normalizing gradients to protect
   * against small gradient magnitudes */
  double m_Alpha;

  /** Threshold below which the gradient is considered zero. */
  double m_GradientMagnitudeThreshold;

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold;

  /** Smoothing parameter for gradient calculation */
  double m_GradientSmoothingStandardDeviations;

  /** The metric value is the mean square difference in intensity between
   * the fixed image and transforming moving image computed over the
   * the overlapping region between the two images. */
  mutable double        m_Metric;
  mutable double        m_SumOfSquaredDifference;
  mutable SizeValueType m_NumberOfPixelsProcessed;
  mutable double        m_RMSChange;
  mutable double        m_SumOfSquaredChange;

  /** Mutex lock to protect modification to metric. */
  mutable std::mutex m_MetricCalculationLock;

  bool m_UseImageSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetMotionRegistrationFunction.hxx"
#endif

#endif
