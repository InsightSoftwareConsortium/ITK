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
#ifndef itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_h
#define itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_h

#include "itkImageRegistrationMethodv4.h"

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkImageMaskSpatialObject.h"
#include "itkTimeVaryingBSplineVelocityFieldTransform.h"

namespace itk
{

/** \class TimeVaryingBSplineVelocityFieldImageRegistrationMethod
 * \brief Interface method for the current registration framework
 * using the time varying velocity field transform.
 *
 *
 * Output: The output is the updated transform which has been added to the
 * composite transform.
 *
 * This derived class from the SimpleImageRegistrationMethod class
 * is specialized to handle time-varying velocity field transforms
 * of which there are currently 3:
 *
 * -# TimeVaryingDisplacementFieldTransform
 * -# GaussianSmoothingOnUpdateTimeVaryingDisplacementFieldTransform
 * -# BSplineSmoothingOnUpdateTimeVaryingDisplacementFieldTransform
 *
 * The latter is derived from the former and performs an optional
 * spatial and temporal smoothing on the update and total velocity
 * fields. Integration of the velocity field is performed using
 * 4th order Runge Kutta and is performed using the class
 * itkTimeVaryingBSplineVelocityFieldIntegrationImageFilter.
 *
 * Optimization occurs in an iterative fashion where for each
 * sample time point, t, in the velocity field, we integrate
 * the velocity field in the range [0, t] to yield the
 * displacement field which warps fixed image to time point
 * t. Simultaneously, we integrate the velocity field in
 * the range [t, 1] to yield the displacement field transform
 * which warps the moving image to time point t.  The metric
 * derivative for each time point of the velocity field
 * calculated in this way produces the normalized update field
 * (or gradient) of the velocity field to be added to the total
 * field at each iteration after being multiplied by the
 * learning rate and optionally smoothed.  Mathematically,
 * this can be described as
 *
 * \f[
 * v_{total} = B_1( v_{total} + \lambda * B_2( v_{update} ) )
 * \f]
 * where

 * \f$ B_1 = \f$ bspline smoothing on the total field
 * \f$ B_2 = \f$ bspline smoothing on the update field
 * \f$ \lambda = \f$ learning rate
 * \f$ v_{update} = \f$ the normalized velocity field where we
 * normalize the velocity field at each time point
 * separately by the max norm of the field at that time
 * point. This is done due to a weakly necessary
 * (but not sufficient) condition being that the velocity
 * field have a constant norm for all time points.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationMethodsv4
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform =
  TimeVaryingBSplineVelocityFieldTransform<double, TFixedImage::ImageDimension>,
  typename TVirtualImage = TFixedImage,
  typename TPointSet = PointSet<unsigned int, TFixedImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT TimeVaryingBSplineVelocityFieldImageRegistrationMethod
: public ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
{
public:
  /** Standard class typedefs. */
  typedef TimeVaryingBSplineVelocityFieldImageRegistrationMethod                      Self;
  typedef ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform,
                                                       TVirtualImage, TPointSet>      Superclass;
  typedef SmartPointer<Self>                                                          Pointer;
  typedef SmartPointer<const Self>                                                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** ImageDimension constants */
  itkStaticConstMacro( ImageDimension, unsigned int, TFixedImage::ImageDimension );

  /** Run-time type information (and related methods). */
  itkTypeMacro( TimeVaryingBSplineVelocityFieldImageRegistrationMethod, SimpleImageRegistrationMethod );

  /** Input typedefs for the images and transforms. */
  typedef TFixedImage                                                 FixedImageType;
  typedef typename FixedImageType::Pointer                            FixedImagePointer;
  typedef typename Superclass::FixedImagesContainerType               FixedImagesContainerType;
  typedef TMovingImage                                                MovingImageType;
  typedef typename MovingImageType::Pointer                           MovingImagePointer;
  typedef typename Superclass::MovingImagesContainerType              MovingImagesContainerType;

  typedef TPointSet                                                   InputPointSetType;
  typedef typename InputPointSetType::Pointer                         InputPointSetPointer;
  typedef typename Superclass::PointSetsContainerType                 PointSetsContainerType;

  /** Metric and transform typedefs */
  typedef typename Superclass::ImageMetricType                        ImageMetricType;
  typedef typename ImageMetricType::Pointer                           ImageMetricPointer;
  typedef typename ImageMetricType::MeasureType                       MeasureType;
  typedef typename ImageMetricType::DerivativeType                    MetricDerivativeType;

  typedef typename Superclass::VirtualImageType                       VirtualImageType;
  typedef typename Superclass::VirtualImageBaseType                   VirtualImageBaseType;
  typedef typename Superclass::VirtualImageBaseConstPointer           VirtualImageBaseConstPointer;

  typedef typename Superclass::MetricType                             MetricType;
  typedef typename Superclass::MultiMetricType                        MultiMetricType;
  typedef typename MetricType::Pointer                                MetricPointer;
  typedef typename Superclass::PointSetMetricType                     PointSetMetricType;

  typedef ImageMaskSpatialObject<ImageDimension>                      ImageMaskSpatialObjectType;
  typedef typename ImageMaskSpatialObjectType::ImageType              MaskImageType;
  typedef typename Superclass::FixedImageMaskType                     FixedImageMaskType;
  typedef typename ImageMaskSpatialObjectType::ImageType              FixedMaskImageType;
  typedef typename Superclass::FixedImageMasksContainerType           FixedImageMasksContainerType;
  typedef typename Superclass::MovingImageMaskType                    MovingImageMaskType;
  typedef typename ImageMaskSpatialObjectType::ImageType              MovingMaskImageType;
  typedef typename Superclass::MovingImageMasksContainerType          MovingImageMasksContainerType;

  typedef typename Superclass::InitialTransformType                                            InitialTransformType;
  typedef TOutputTransform                                                                     OutputTransformType;
  typedef typename OutputTransformType::Pointer                                                OutputTransformPointer;
  typedef typename OutputTransformType::ScalarType                                             RealType;
  typedef typename OutputTransformType::DerivativeType                                         DerivativeType;
  typedef typename DerivativeType::ValueType                                                   DerivativeValueType;
  typedef typename OutputTransformType::DisplacementFieldType                                  DisplacementFieldType;
  typedef typename DisplacementFieldType::PointType                                            DisplacementFieldPointType;

  typedef ContinuousIndex<typename DisplacementFieldPointType::CoordRepType, ImageDimension>   ContinuousIndexType;

  typedef typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticeType        TimeVaryingVelocityFieldControlPointLatticeType;
  typedef typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticePointer     TimeVaryingVelocityFieldControlPointLatticePointer;
  typedef typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticeType        TimeVaryingVelocityFieldType;
  typedef typename OutputTransformType::TimeVaryingVelocityFieldControlPointLatticePointer     TimeVaryingVelocityFieldPointer;
  typedef typename TimeVaryingVelocityFieldControlPointLatticeType::PixelType                  DisplacementVectorType;

  typedef typename Superclass::CompositeTransformType                 CompositeTransformType;
  typedef typename CompositeTransformType::TransformType              TransformBaseType;

  typedef typename Superclass::DecoratedOutputTransformType           DecoratedOutputTransformType;
  typedef typename DecoratedOutputTransformType::Pointer              DecoratedOutputTransformPointer;

  typedef Array<SizeValueType>                                        NumberOfIterationsArrayType;

  typedef DisplacementFieldTransform<RealType, ImageDimension>        DisplacementFieldTransformType;
  typedef typename DisplacementFieldTransformType::Pointer            DisplacementFieldTransformPointer;

  typedef PointSet<DisplacementVectorType, ImageDimension + 1>        VelocityFieldPointSetType;
  typedef typename VelocityFieldPointSetType::Pointer                 VelocityFieldPointSetPointer;
  typedef BSplineScatteredDataPointSetToImageFilter
    <VelocityFieldPointSetType, TimeVaryingVelocityFieldType>         BSplineFilterType;
  typedef typename BSplineFilterType::WeightsContainerType            WeightsContainerType;
  typedef typename WeightsContainerType::Element                      WeightsElementType;
  typedef Image<WeightsElementType, ImageDimension>                   WeightedMaskImageType;
  typedef Image<WeightsElementType, ImageDimension + 1>               TimeVaryingWeightedMaskImageType;

  /** Set/Get the learning rate. */
  itkSetMacro( LearningRate, RealType );
  itkGetConstMacro( LearningRate, RealType );

  /** Set/Get the number of iterations per level. */
  itkSetMacro( NumberOfIterationsPerLevel, NumberOfIterationsArrayType );
  itkGetConstMacro( NumberOfIterationsPerLevel, NumberOfIterationsArrayType );

  /** Set/Get the convergence threshold */
  itkSetMacro( ConvergenceThreshold, RealType );
  itkGetConstMacro( ConvergenceThreshold, RealType );

  /** Set/Get the convergence window size */
  itkSetMacro( ConvergenceWindowSize, unsigned int );
  itkGetConstMacro( ConvergenceWindowSize, unsigned int );

  /** Set/Get the number of time point samples. */
  itkSetMacro( NumberOfTimePointSamples, SizeValueType );
  itkGetConstMacro( NumberOfTimePointSamples, SizeValueType );

protected:
  TimeVaryingBSplineVelocityFieldImageRegistrationMethod();
  virtual ~TimeVaryingBSplineVelocityFieldImageRegistrationMethod() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  /** Perform the registration. */
  virtual void  GenerateData() ITK_OVERRIDE;

  /** Handle optimization internally */
  virtual void StartOptimization();

  /** Translate metrics to the point-set for building the (n+1)-D B-spline model */
  void GetMetricDerivativePointSetForAllTimePoints( VelocityFieldPointSetType *, WeightsContainerType * );

  void AttachMetricGradientPointSetAtSpecificTimePoint( const RealType,
    VelocityFieldPointSetType *, WeightsContainerType *, const FixedImagesContainerType,
    const PointSetsContainerType, const TransformBaseType *, const MovingImagesContainerType,
    const PointSetsContainerType, const TransformBaseType *, const FixedImageMasksContainerType );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TimeVaryingBSplineVelocityFieldImageRegistrationMethod);

  DisplacementFieldTransformPointer                   m_IdentityDisplacementFieldTransform;

  RealType                                            m_LearningRate;

  RealType                                            m_ConvergenceThreshold;
  unsigned int                                        m_ConvergenceWindowSize;

  NumberOfIterationsArrayType                         m_NumberOfIterationsPerLevel;

  SizeValueType                                       m_NumberOfTimePointSamples;

  WeightsElementType                                  m_BoundaryWeight;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.hxx"
#endif

#endif
