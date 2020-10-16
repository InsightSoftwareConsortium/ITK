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
#ifndef itkVariationalRegistrationFunction_h
#define itkVariationalRegistrationFunction_h

#include "itkFiniteDifferenceFunction.h"
// #include "itkWarpImageFilter.h"
#include "itkContinuousBorderWarpImageFilter.h"
#include <mutex>

namespace itk
{

/** \class itk::VariationalRegistrationFunction
 *
 *  \brief Base class for force calculation in the variational registration framework.
 *
 *  This class is templated over fixed image type, moving image type and deformation field type.
 *  This function has the fixed image, the moving image and the current displacement field as input
 *  and computes an update value in ComputeUpdate().
 *
 *  Implement a concrete force type in a subclass; overwrite the methods
 *  InitializeIteration() and ComputeUpdate().
 *
 *  \sa VariationalRegistrationFilter
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class VariationalRegistrationFunction : public FiniteDifferenceFunction<TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VariationalRegistrationFunction);

  /** Standard class type alias. */
  using Self = VariationalRegistrationFunction;
  using Superclass = FiniteDifferenceFunction<TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using TimeStepType = typename Superclass::TimeStepType;

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationFunction, FiniteDifferenceFunction);

  /** Get image dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** MovingImage image type. */
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::ConstPointer;

  /** FixedImage image type. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::ConstPointer;

  /** FixedImage image type. */
  using WarpedImageType = TFixedImage;
  using WarpedImagePointer = typename WarpedImageType::ConstPointer;

  /** Deformation field type. */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldTypePointer = typename DisplacementFieldType::ConstPointer;

  /** MovingImage image type. */
  using MaskImagePixelType = unsigned char;
  using MaskImageType = Image<MaskImagePixelType, ImageDimension>;
  using MaskImagePointer = typename MaskImageType::ConstPointer;

  // uncomment the following line to use the standard ITK warper (not recommended)
  // typedef itk::WarpImageFilter< FixedImageType, WarpedImageType, DisplacementFieldType >
  /** Typedef of the warp image filter. */
  using MovingImageWarperType =
    itk::ContinuousBorderWarpImageFilter<FixedImageType, WarpedImageType, DisplacementFieldType>;
  using MovingImageWarperPointer = typename MovingImageWarperType::Pointer;


  /** Set the Moving image.  */
  virtual void
  SetMovingImage(const MovingImageType * ptr)
  {
    m_MovingImage = ptr;
  }

  /** Get the Moving image. */
  virtual const MovingImageType *
  GetMovingImage() const
  {
    return m_MovingImage;
  }

  /** Set the fixed image. */
  virtual void
  SetFixedImage(const FixedImageType * ptr)
  {
    m_FixedImage = ptr;
  }

  /** Get the fixed image. */
  virtual const FixedImageType *
  GetFixedImage() const
  {
    return m_FixedImage;
  }

  /** Set the deformation field. */
  virtual void
  SetDisplacementField(DisplacementFieldType * ptr)
  {
    m_DisplacementField = ptr;
  }

  /** Get the deformation field. */
  virtual const DisplacementFieldType *
  GetDisplacementField() const
  {
    return m_DisplacementField;
  }

  /** Set the mask image. */
  virtual void
  SetMaskImage(const MaskImageType * ptr)
  {
    m_MaskImage = ptr;
  }

  /** Get the mask image. */
  virtual const MaskImageType *
  GetMaskImage() const
  {
    return m_MaskImage;
  }

  /** Set the moving image warper. */
  virtual void
  SetMovingImageWarper(MovingImageWarperType * ptr)
  {
    m_MovingImageWarper = ptr;
  }

  /** Get the moving image warper. */
  virtual const MovingImageWarperType *
  GetMovingImageWarper() const
  {
    return m_MovingImageWarper;
  }

  /** Set the time step. This time step will be used by ComputeGlobalTimeStep(). */
  virtual void
  SetTimeStep(TimeStepType timeStep)
  {
    m_TimeStep = timeStep;
  }

  /** Get the time step. */
  virtual const TimeStepType
  GetTimeStep() const
  {
    return m_TimeStep;
  }

  /** Set the MaskBackgroundThreshold. All Pixels of the mask image will be
   *  treated as background if the are <= this threshold. */
  virtual void
  SetMaskBackgroundThreshold(MaskImagePixelType threshold)
  {
    m_MaskBackgroundThreshold = threshold;
  }

  /** Get the MaskBackgroundThreshold. All Pixels of the mask image will be
   *  treated as background if the are <= this threshold. */
  virtual MaskImagePixelType
  GetMaskBackgroundThreshold() const
  {
    return m_MaskBackgroundThreshold;
  }

  /** Set the object's state before each iteration. */
  void
  InitializeIteration() override;

  /** Computes the time step for an update.
   * Returns the constant time step.
   * \sa SetTimeStep() */
  TimeStepType
  ComputeGlobalTimeStep(void * itkNotUsed(GlobalData)) const override
  {
    return m_TimeStep;
  }

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  void *
  GetGlobalDataPointer() const override;

  /** Release memory for global data structure. */
  void
  ReleaseGlobalDataPointer(void * GlobalData) const override;

  //
  // Metric accessor methods
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

protected:
  VariationalRegistrationFunction();
  ~VariationalRegistrationFunction() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Warp the moving image into the domain of the fixed image using the
   * deformation field. */
  virtual void
  WarpMovingImage();

  /** Get the warped image. */
  virtual const WarpedImagePointer
  GetWarpedImage() const;

  /** A global data type for this class of equation. Used to store
   * information for computing the metric. */
  struct GlobalDataStruct
  {
    double        m_SumOfMetricValues;
    SizeValueType m_NumberOfPixelsProcessed;
    double        m_SumOfSquaredChange;
  };

private:
  /** The Moving image. */
  MovingImagePointer m_MovingImage;

  /** The fixed image. */
  FixedImagePointer m_FixedImage;

  /** The deformation field. */
  DisplacementFieldTypePointer m_DisplacementField;

  /** The deformation field. */
  MaskImagePointer m_MaskImage;

  /** A class to warp the moving image into the domain of the fixed image. */
  MovingImageWarperPointer m_MovingImageWarper;

  /** The global timestep. */
  TimeStepType m_TimeStep;

  /** Threshold to define the background in the mask image. */
  MaskImagePixelType m_MaskBackgroundThreshold;

  /** The metric value is the mean square difference in intensity between
   * the fixed image and transforming moving image computed over the
   * the overlapping region between the two images. */
  mutable double        m_Metric;
  mutable double        m_SumOfMetricValues;
  mutable SizeValueType m_NumberOfPixelsProcessed;
  mutable double        m_RMSChange;
  mutable double        m_SumOfSquaredChange;

  /** Mutex lock to protect modification to metric. */
  mutable std::mutex m_MetricCalculationLock;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationFunction.hxx"
#endif

#endif
