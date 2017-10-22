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
#ifndef itkLevelSetMotionRegistrationFunction_h
#define itkLevelSetMotionRegistrationFunction_h

#include "itkPDEDeformableRegistrationFunction.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"

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
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
class ITK_TEMPLATE_EXPORT LevelSetMotionRegistrationFunction:
  public PDEDeformableRegistrationFunction< TFixedImage,
                                            TMovingImage,
                                            TDisplacementField >
{
public:
  /** Standard class typedefs. */
  typedef LevelSetMotionRegistrationFunction Self;
  typedef PDEDeformableRegistrationFunction< TFixedImage,
                                             TMovingImage, TDisplacementField
                                             > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetMotionRegistrationFunction,
               PDEDeformableRegistrationFunction);

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;
  typedef typename MovingImageType::SpacingType   MovingSpacingType;

  /** FixedImage image type. */
  typedef typename Superclass::FixedImageType    FixedImageType;
  typedef typename Superclass::FixedImagePointer FixedImagePointer;
  typedef typename FixedImageType::IndexType     IndexType;
  typedef typename FixedImageType::SizeType      SizeType;
  typedef typename FixedImageType::SpacingType   SpacingType;

  /** Deformation field type. */
  typedef typename Superclass::DisplacementFieldType DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer
  DisplacementFieldTypePointer;

  /** Inherit some enums from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned
                      int, Superclass::ImageDimension);

  /** Inherit some enums from the superclass. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::TimeStepType     TimeStepType;

  /** Interpolator type. */
  typedef double                                                    CoordRepType;
  typedef InterpolateImageFunction< MovingImageType, CoordRepType > InterpolatorType;
  typedef typename InterpolatorType::Pointer                        InterpolatorPointer;
  typedef typename InterpolatorType::PointType                      PointType;
  typedef LinearInterpolateImageFunction< MovingImageType, CoordRepType >
  DefaultInterpolatorType;

  /** Vector types. */
  typedef Vector< double, itkGetStaticConstMacro(ImageDimension) > VectorType;
  typedef CovariantVector< double, itkGetStaticConstMacro(ImageDimension) >
  CovariantVectorType;

  /** Moving image gradient calculator type. */
  typedef SmoothingRecursiveGaussianImageFilter< MovingImageType >
  MovingImageSmoothingFilterType;
  typedef typename MovingImageSmoothingFilterType::Pointer
  MovingImageSmoothingFilterPointer;

  /** Set the moving image interpolator. */
  void SetMovingImageInterpolator(InterpolatorType *ptr)
  { m_MovingImageInterpolator = ptr; }

  /** Get the moving image interpolator. */
  InterpolatorType * GetMovingImageInterpolator(void)
  { return m_MovingImageInterpolator; }

  /** Compute the time step that can taken for this iterations.  In
   * this context, the timestep is a function of the maximum gradients. */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const ITK_OVERRIDE;

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  virtual void * GetGlobalDataPointer() const ITK_OVERRIDE
  {
    GlobalDataStruct *global = new GlobalDataStruct();

    global->m_SumOfSquaredDifference  = 0.0;
    global->m_NumberOfPixelsProcessed = 0L;
    global->m_SumOfSquaredChange      = 0;
    global->m_MaxL1Norm            = NumericTraits< double >::NonpositiveMin();
    return global;
  }

  /** Release memory for global data structure. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const ITK_OVERRIDE;

  /** Set the object's state before each iteration. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  virtual PixelType  ComputeUpdate( const NeighborhoodType & neighborhood,
                                    void *globalData,
                                    const FloatOffsetType & offset =
                                      FloatOffsetType(0.0) ) ITK_OVERRIDE;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images. */
  virtual double GetMetric() const
  { return m_Metric; }

  /** Get the rms change in deformation field. */
  virtual double GetRMSChange() const
  { return m_RMSChange; }

  /** Set/Get the parameter alpha.  Alpha is added to the calculated
   * gradient magnitude prior to normalizing the gradient to protect
   * against numerical instability as the gradient magnitude
   * approaches zero.  This should be set as a small fraction of the
   * intensity dynamic range, for instance 0.04%. Default is the
   * absolute (not percentage) value of 0.1. */
  virtual void SetAlpha(double);

  virtual double GetAlpha() const;

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void SetIntensityDifferenceThreshold(double);

  virtual double GetIntensityDifferenceThreshold() const;

  /** Set/Get the threshold below which the gradient magnitude is
   * considered the zero vector. Default is 1e-9. */
  virtual void SetGradientMagnitudeThreshold(double);

  virtual double GetGradientMagnitudeThreshold() const;

  /** Set/Get the standard deviation used for smoothing the moving
   * image prior to calculating gradients. */
  virtual void SetGradientSmoothingStandardDeviations(double);

  virtual double GetGradientSmoothingStandardDeviations() const;

  /** Use the image spacing information in calculations. Use this option if you
   * want derivatives in physical space. Default is UseImageSpacing ON, due to a
   * backward compatibility state. */
  void SetUseImageSpacing(bool);

  bool GetUseImageSpacing() const;

protected:
  LevelSetMotionRegistrationFunction();
  ~LevelSetMotionRegistrationFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** FixedImage image neighborhood iterator type. */
  typedef ConstNeighborhoodIterator< FixedImageType >
  FixedImageNeighborhoodIteratorType;

  /** A global data type for this class of equation. Used to store
   * information for computing the metric. */
  struct GlobalDataStruct {
    double m_SumOfSquaredDifference;
    SizeValueType m_NumberOfPixelsProcessed;
    double m_SumOfSquaredChange;
    double m_MaxL1Norm;
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetMotionRegistrationFunction);

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
  mutable SimpleFastMutexLock m_MetricCalculationLock;

  bool m_UseImageSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetMotionRegistrationFunction.hxx"
#endif

#endif
