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
#ifndef itkFastSymmetricForcesDemonsRegistrationFunction_h
#define itkFastSymmetricForcesDemonsRegistrationFunction_h

#include "itkPDEDeformableRegistrationFunction.h"
#include "itkPoint.h"
#include "itkInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkWarpImageFilter.h"

namespace itk
{
/**
 * \class FastSymmetricForcesDemonsRegistrationFunction
 *
 * This class provides a substantially faster implementation of the
 * symmetric demons registration force. Speed is improved by keeping
 * a deformed copy of the moving image for gradient evaluation.
 *
 * \sa SymmetricForcesDemonsRegistrationFunction
 * \sa SymmetricForcesDemonsRegistrationFilter
 * \sa DemonsRegistrationFilter
 * \sa DemonsRegistrationFunction
 * \ingroup FiniteDifferenceFunctions
 *
 * \author Torsten Rohlfing, Neuroscience Program, SRI International
 * \ingroup ITKPDEDeformableRegistration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
class ITK_TEMPLATE_EXPORT FastSymmetricForcesDemonsRegistrationFunction:
  public PDEDeformableRegistrationFunction< TFixedImage,
                                            TMovingImage, TDisplacementField >
{
public:
  /** Standard class typedefs. */
  typedef FastSymmetricForcesDemonsRegistrationFunction Self;
  typedef PDEDeformableRegistrationFunction< TFixedImage,
                                             TMovingImage, TDisplacementField >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastSymmetricForcesDemonsRegistrationFunction,
               PDEDeformableRegistrationFunction);

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;

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

#ifdef ITKV3_COMPATIBILITY
  typedef typename Superclass::DeformationFieldType        DeformationFieldType;
  typedef typename Superclass::DeformationFieldTypePointer DeformationFieldTypePointer;
#endif

  /** Inherit some enums from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Inherit some enums from the superclass. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::TimeStepType     TimeStepType;

  /** Interpolator type. */
  typedef double                                                          CoordRepType;
  typedef InterpolateImageFunction< MovingImageType, CoordRepType >       InterpolatorType;
  typedef typename InterpolatorType::Pointer                              InterpolatorPointer;
  typedef typename InterpolatorType::PointType                            PointType;
  typedef LinearInterpolateImageFunction< MovingImageType, CoordRepType > DefaultInterpolatorType;

  /** Warper type */
  typedef WarpImageFilter< MovingImageType, MovingImageType, DisplacementFieldType > WarperType;
  typedef typename WarperType::Pointer                                              WarperPointer;

  /** Covariant vector type. */
  typedef CovariantVector< double, itkGetStaticConstMacro(ImageDimension) > CovariantVectorType;

  /** Gradient calculator types. */
  typedef CentralDifferenceImageFunction< FixedImageType > GradientCalculatorType;
  typedef typename GradientCalculatorType::Pointer         GradientCalculatorPointer;

  typedef CentralDifferenceImageFunction< MovingImageType > MovingGradientCalculatorType;
  typedef typename MovingGradientCalculatorType::Pointer    MovingGradientCalculatorPointer;

  /** Set the moving image interpolator. */
  void SetMovingImageInterpolator(InterpolatorType *ptr)
  {
    m_MovingImageInterpolator = ptr; m_MovingImageWarper->SetInterpolator(ptr);
  }

  /** Get the moving image interpolator. */
  InterpolatorType * GetMovingImageInterpolator(void)
  {
    return m_MovingImageInterpolator;
  }

  /** This class uses a constant timestep of 1. */
  virtual TimeStepType ComputeGlobalTimeStep( void *itkNotUsed(GlobalData) ) const
  {
    return m_TimeStep;
  }

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  virtual void * GetGlobalDataPointer() const
  {
    GlobalDataStruct *global = new GlobalDataStruct();

    global->m_SumOfSquaredDifference  = 0.0;
    global->m_NumberOfPixelsProcessed = 0L;
    global->m_SumOfSquaredChange      = 0;
    return global;
  }

  /** Release memory for global data structure. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const;

  /** Set the object's state before each iteration. */
  virtual void InitializeIteration();

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  virtual PixelType  ComputeUpdate( const NeighborhoodType & neighborhood,
                                    void *globalData,
                                    const FloatOffsetType & offset = FloatOffsetType(0.0) );

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images. */
  virtual double GetMetric() const
  {
    return m_Metric;
  }

  /** Get the rms change in deformation field. */
  virtual const double & GetRMSChange() const
  {
    return m_RMSChange;
  }

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void SetIntensityDifferenceThreshold(double);

  virtual double GetIntensityDifferenceThreshold() const;

protected:
  FastSymmetricForcesDemonsRegistrationFunction();
  ~FastSymmetricForcesDemonsRegistrationFunction() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** FixedImage image neighborhood iterator type. */
  typedef ConstNeighborhoodIterator< FixedImageType > FixedImageNeighborhoodIteratorType;

  /** A global data type for this class of equation. Used to store
   * iterators for the fixed image. */
  struct GlobalDataStruct {
    double m_SumOfSquaredDifference;
    SizeValueType m_NumberOfPixelsProcessed;
    double m_SumOfSquaredChange;
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastSymmetricForcesDemonsRegistrationFunction);

  /** Cache fixed image information. */
  double m_Normalizer;

  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the fixed image. */
  MovingGradientCalculatorPointer m_WarpedMovingImageGradientCalculator;

  /** Function to interpolate the moving image. */
  InterpolatorPointer m_MovingImageInterpolator;

  /** Filter to warp moving image for fast gradient computation. */
  WarperPointer m_MovingImageWarper;

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

  /** Mutex lock to protect modification to metric. */
  mutable SimpleFastMutexLock m_MetricCalculationLock;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastSymmetricForcesDemonsRegistrationFunction.hxx"
#endif

#endif
