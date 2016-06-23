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
#ifndef __itkVariationalRegistrationFunction_h
#define __itkVariationalRegistrationFunction_h

#include "itkFiniteDifferenceFunction.h"
// #include "itkWarpImageFilter.h"
#include "itkContinuousBorderWarpImageFilter.h"

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
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalRegistrationFunction : public FiniteDifferenceFunction<TDisplacementField>
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationFunction              Self;
  typedef FiniteDifferenceFunction<TDisplacementField> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  typedef typename Superclass::TimeStepType TimeStepType;

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationFunction, FiniteDifferenceFunction);

  /** Get image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** MovingImage image type. */
  typedef TMovingImage                           MovingImageType;
  typedef typename MovingImageType::ConstPointer MovingImagePointer;

  /** FixedImage image type. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::ConstPointer FixedImagePointer;

  /** FixedImage image type. */
  typedef TFixedImage                            WarpedImageType;
  typedef typename WarpedImageType::ConstPointer WarpedImagePointer;

  /** Deformation field type. */
  typedef TDisplacementField                           DisplacementFieldType;
  typedef typename DisplacementFieldType::ConstPointer DisplacementFieldTypePointer;

  /** MovingImage image type. */
  typedef unsigned char                             MaskImagePixelType;
  typedef Image<MaskImagePixelType, ImageDimension> MaskImageType;
  typedef typename MaskImageType::ConstPointer      MaskImagePointer;

  // uncomment the following line to use the standard ITK warper (not recommended)
  // typedef itk::WarpImageFilter< FixedImageType, WarpedImageType, DisplacementFieldType >
  /** Typedef of the warp image filter. */
  typedef itk::ContinuousBorderWarpImageFilter<FixedImageType, WarpedImageType, DisplacementFieldType>
                                                  MovingImageWarperType;
  typedef typename MovingImageWarperType::Pointer MovingImageWarperPointer;


  /** Set the Moving image.  */
  virtual void
  SetMovingImage(const MovingImageType * ptr)
  {
    m_MovingImage = ptr;
  }

  /** Get the Moving image. */
  virtual const MovingImageType *
  GetMovingImage(void) const
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
  GetFixedImage(void) const
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
  GetDisplacementField(void) const
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
  GetMaskImage(void) const
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
  GetMovingImageWarper(void) const
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
  GetTimeStep(void) const
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
  GetMaskBackgroundThreshold(void) const
  {
    return m_MaskBackgroundThreshold;
  }

  /** Set the object's state before each iteration. */
  virtual void
  InitializeIteration() ITK_OVERRIDE;

  /** Computes the time step for an update.
   * Returns the constant time step.
   * \sa SetTimeStep() */
  virtual TimeStepType
  ComputeGlobalTimeStep(void * itkNotUsed(GlobalData)) const ITK_OVERRIDE
  {
    return m_TimeStep;
  }

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  virtual void *
  GetGlobalDataPointer() const ITK_OVERRIDE;

  /** Release memory for global data structure. */
  virtual void
  ReleaseGlobalDataPointer(void * GlobalData) const ITK_OVERRIDE;

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
  ~VariationalRegistrationFunction() {}

  /** Print information about the filter. */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Warp the moving image into the domain of the fixed image using the
   * deformation field. */
  virtual void
  WarpMovingImage(void);

  /** Get the warped image. */
  virtual const WarpedImagePointer
  GetWarpedImage(void) const;

  /** A global data type for this class of equation. Used to store
   * information for computing the metric. */
  struct GlobalDataStruct
  {
    double        m_SumOfMetricValues;
    SizeValueType m_NumberOfPixelsProcessed;
    double        m_SumOfSquaredChange;
  };

private:
  VariationalRegistrationFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

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
  mutable SimpleFastMutexLock m_MetricCalculationLock;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationFunction.hxx"
#endif

#endif
