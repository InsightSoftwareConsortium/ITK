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
#ifndef itkTimeVaryingVelocityFieldImageRegistrationMethodv4_h
#define itkTimeVaryingVelocityFieldImageRegistrationMethodv4_h

#include "itkImageRegistrationMethodv4.h"

#include "itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform.h"

namespace itk
{

/** \class TimeVaryingVelocityFieldImageRegistrationMethodv4
 * \brief Interface method for the current registration framework
 * using the time varying velocity field transform.
 *
 *
 * Output: The output is the updated transform which has been added to the
 * composite transform.
 *
 * This derived class from the ImageRegistrationMethodv4 class
 * is specialized to handle time-varying velocity field transforms
 * of which there are currently 2:
 *
 * -# TimeVaryingDisplacementFieldTransform
 * -# GaussianSmoothingOnUpdateTimeVaryingDisplacementFieldTransform
 *
 * The latter is derived from the former and performs an optional
 * spatial and temporal smoothing on the update and total velocity
 * fields. Integration of the velocity field is performed using
 * 4th order Runge Kutta and is performed using the class
 * itkTimeVaryingVelocityFieldIntegrationImageFilter.
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
 * v_{total} = G_1( v_{total} + \lambda * G_2( v_{update} ) )
 * \f]
 * where
 *
 * \f$ G_1 = \f$ gaussian smoothing on the total field
 * \f$ G_2 = \f$ gaussian smoothing on the update field
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
  GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<double, TFixedImage::ImageDimension>,
  typename TVirtualImage = TFixedImage,
  typename TPointSet = PointSet<unsigned int, TFixedImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT TimeVaryingVelocityFieldImageRegistrationMethodv4
: public ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
{
public:
  /** Standard class typedefs. */
  typedef TimeVaryingVelocityFieldImageRegistrationMethodv4                       Self;
  typedef ImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform,
                                                       TVirtualImage, TPointSet>  Superclass;
  typedef SmartPointer<Self>                                                      Pointer;
  typedef SmartPointer<const Self>                                                ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** ImageDimension constants */
  itkStaticConstMacro( ImageDimension, unsigned int, TFixedImage::ImageDimension );

  /** Run-time type information (and related methods). */
  itkTypeMacro( TimeVaryingVelocityFieldImageRegistrationMethodv4, ImageRegistrationMethodv4 );

  /** Input typedefs for the images and transforms. */
  typedef TFixedImage                                                 FixedImageType;
  typedef typename FixedImageType::Pointer                            FixedImagePointer;
  typedef TMovingImage                                                MovingImageType;
  typedef typename MovingImageType::Pointer                           MovingImagePointer;

  typedef typename MovingImageType::RegionType                        RegionType;

  /** Metric and transform typedefs */
  typedef typename Superclass::ImageMetricType                        ImageMetricType;
  typedef typename ImageMetricType::Pointer                           ImageMetricPointer;
  typedef typename ImageMetricType::VirtualImageType                  VirtualImageType;
  typedef typename ImageMetricType::MeasureType                       MeasureType;
  typedef typename Superclass::MultiMetricType                        MultiMetricType;

  typedef TOutputTransform                                            OutputTransformType;
  typedef typename OutputTransformType::Pointer                       OutputTransformPointer;
  typedef typename OutputTransformType::ScalarType                    RealType;
  typedef typename OutputTransformType::DerivativeType                DerivativeType;
  typedef typename DerivativeType::ValueType                          DerivativeValueType;
  typedef typename OutputTransformType::TimeVaryingVelocityFieldType  TimeVaryingVelocityFieldType;
  typedef typename TimeVaryingVelocityFieldType::Pointer              TimeVaryingVelocityFieldPointer;
  typedef typename OutputTransformType::DisplacementFieldType         DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer                     DisplacementFieldPointer;
  typedef typename TimeVaryingVelocityFieldType::PixelType            DisplacementVectorType;

  typedef typename Superclass::CompositeTransformType                 CompositeTransformType;

  typedef typename Superclass::DecoratedOutputTransformType           DecoratedOutputTransformType;
  typedef typename DecoratedOutputTransformType::Pointer              DecoratedOutputTransformPointer;

  typedef Array<SizeValueType>                                        NumberOfIterationsArrayType;

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

protected:
  TimeVaryingVelocityFieldImageRegistrationMethodv4();
  virtual ~TimeVaryingVelocityFieldImageRegistrationMethodv4() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  /** Perform the registration. */
  virtual void  GenerateData() ITK_OVERRIDE;

  /** Multithreaded function which calculates the norm of the velocity field. */
  void ThreadedGenerateData( const RegionType &, ThreadIdType );

  /** Handle optimization internally */
  virtual void StartOptimization();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TimeVaryingVelocityFieldImageRegistrationMethodv4);

  RealType                                                        m_LearningRate;

  RealType                                                        m_ConvergenceThreshold;
  unsigned int                                                    m_ConvergenceWindowSize;

  NumberOfIterationsArrayType                                     m_NumberOfIterationsPerLevel;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTimeVaryingVelocityFieldImageRegistrationMethodv4.hxx"
#endif

#endif
