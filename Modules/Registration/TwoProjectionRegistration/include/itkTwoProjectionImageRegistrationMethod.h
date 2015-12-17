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
#ifndef itkTwoProjectionImageRegistrationMethod_h
#define itkTwoProjectionImageRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkTwoImageToOneImageMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkDataObjectDecorator.h"

namespace itk
{

/** \class TwoProjectionImageRegistrationMethod
 * \brief Base class for Image Registration Methods
 *
 * This Class define the generic interface for a registration method.
 *
 * This class is templated over the type of the two image to be
 * registered. A generic Transform is used by this class. That allows
 * to select at run time the particular type of transformation that
 * is to be applied for registering the images.
 *
 * This method use a generic Metric in order to compare the two images.
 * the final goal of the registration method is to find the set of
 * parameters of the Transformation that optimizes the metric.
 *
 * The registration method also support a generic optimizer that can
 * be selected at run-time. The only restriction for the optimizer is
 * that it should be able to operate in single-valued cost functions
 * given that the metrics used to compare images provide a single
 * value as output.
 *
 * The terms : Fixed image and Moving image are used in this class
 * to indicate what image is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed image as a reference
 * and searchs for a Transform that will map points from the space of the
 * Fixed image to the space of the Moving image.
 *
 * For doing so, a Metric will be continously applied to compare the Fixed
 * image with the Transformed Moving image. This process also requires to
 * interpolate values from the Moving image.
 *
 * \ingroup RegistrationFilters
 * \ingroup TwoProjectionRegistration
 */
template <typename TFixedImage, typename TMovingImage>
class TwoProjectionImageRegistrationMethod : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef TwoProjectionImageRegistrationMethod Self;
  typedef ProcessObject                        Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TwoProjectionImageRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /**  Type of the Moving image. */
  typedef TMovingImage                           MovingImageType;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /**  Type of the metric. */
  typedef TwoImageToOneImageMetric<FixedImageType, MovingImageType> MetricType;
  typedef typename MetricType::Pointer                              MetricPointer;
  typedef typename MetricType::FixedImageRegionType                 FixedImageRegionType;

  /**  Type of the Transform . */
  typedef typename MetricType::TransformType TransformType;
  typedef typename TransformType::Pointer    TransformPointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  typedef DataObjectDecorator<TransformType>         TransformOutputType;
  typedef typename TransformOutputType::Pointer      TransformOutputPointer;
  typedef typename TransformOutputType::ConstPointer TransformOutputConstPointer;

  /**  Type of the Interpolator. */
  typedef typename MetricType::InterpolatorType InterpolatorType;
  typedef typename InterpolatorType::Pointer    InterpolatorPointer;

  /**  Type of the optimizer. */
  typedef SingleValuedNonLinearOptimizer OptimizerType;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef typename MetricType::TransformParametersType ParametersType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Method that initiates the registration. This will Initialize and ensure
   * that all inputs the registration needs are in place, via a call to
   * Initialize() will then start the optimization process via a call to
   * StartOptimization()  */
  void
  StartRegistration(void);

  /** Method that initiates the optimization process. */
  void
  StartOptimization(void);

  /** Set/Get the Fixed images. */
  void
  SetFixedImage1(const FixedImageType * fixedImage1);
  void
  SetFixedImage2(const FixedImageType * fixedImage2);
  itkGetConstObjectMacro(FixedImage1, FixedImageType);
  itkGetConstObjectMacro(FixedImage2, FixedImageType);

  /** Set/Get the Moving image. */
  void
  SetMovingImage(const MovingImageType * movingImage);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Set/Get the Optimizer. */
  itkSetObjectMacro(Optimizer, OptimizerType);
  itkGetObjectMacro(Optimizer, OptimizerType);

  /** Set/Get the Metric. */
  itkSetObjectMacro(Metric, MetricType);
  itkGetObjectMacro(Metric, MetricType);

  /** Set/Get the Transfrom. */
  itkSetObjectMacro(Transform, TransformType);
  itkGetObjectMacro(Transform, TransformType);

  /** Set/Get the Interpolators. */
  itkSetObjectMacro(Interpolator1, InterpolatorType);
  itkSetObjectMacro(Interpolator2, InterpolatorType);
  itkGetObjectMacro(Interpolator1, InterpolatorType);
  itkGetObjectMacro(Interpolator2, InterpolatorType);

  /** Set/Get the initial transformation parameters. */
  virtual void
  SetInitialTransformParameters(const ParametersType & param);
  itkGetConstReferenceMacro(InitialTransformParameters, ParametersType);

  /** Get the last transformation parameters visited by
   * the optimizer. */
  itkGetConstReferenceMacro(LastTransformParameters, ParametersType);

  /** Set the region of the fixed image to be considered as region of
   interest during the registration. This region will be passed to
   the ImageMetric in order to restrict the metric computation to
   consider only this region.
   \warning The same region can also be set directly into the metric.
   please avoid to set the region in both places since this can lead
   to inconsistent configurations.  */
  void
  SetFixedImageRegion1(const FixedImageRegionType & region1);
  void
  SetFixedImageRegion2(const FixedImageRegionType & region2);
  /** Get the region of the fixed image to be considered as region of
   interest during the registration. This region will be passed to
   the ImageMetric in order to restrict the metric computation to
   consider only this region.  */
  itkGetConstReferenceMacro(FixedImageRegion1, FixedImageRegionType);
  itkGetConstReferenceMacro(FixedImageRegion2, FixedImageRegionType);
  /** True if a region has been defined for the fixed image to which
   the ImageMetric will limit its computation */
  itkGetMacro(FixedImageRegionDefined1, bool);
  itkGetMacro(FixedImageRegionDefined2, bool);
  /** Turn on/off the use of a fixed image region to which
   the ImageMetric will limit its computation.
   \warning The region must have been previously defined using the
   SetFixedImageRegion member function */
  itkSetMacro(FixedImageRegionDefined1, bool);
  itkSetMacro(FixedImageRegionDefined2, bool);

  /** Initialize by setting the interconnects between the components. */
  virtual void
  Initialize();

  /** Returns the transform resulting from the registration process  */
  const TransformOutputType *
  GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using Superclass::MakeOutput;
  virtual DataObjectPointer
  MakeOutput(unsigned int idx);

  /** Method to return the latest modified time of this object or
   * any of its cached ivars */
  unsigned long
  GetMTime() const;

protected:
  TwoProjectionImageRegistrationMethod();
  virtual ~TwoProjectionImageRegistrationMethod() {};
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  virtual void
  GenerateData() ITK_OVERRIDE;

  /** Provides derived classes with the ability to set this private var */
  itkSetMacro(LastTransformParameters, ParametersType);


private:
  TwoProjectionImageRegistrationMethod(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  MetricPointer          m_Metric;
  OptimizerType::Pointer m_Optimizer;

  MovingImageConstPointer m_MovingImage;
  FixedImageConstPointer  m_FixedImage1;
  FixedImageConstPointer  m_FixedImage2;

  TransformPointer    m_Transform;
  InterpolatorPointer m_Interpolator1;
  InterpolatorPointer m_Interpolator2;

  ParametersType m_InitialTransformParameters;
  ParametersType m_LastTransformParameters;

  bool                 m_FixedImageRegionDefined1;
  bool                 m_FixedImageRegionDefined2;
  FixedImageRegionType m_FixedImageRegion1;
  FixedImageRegionType m_FixedImageRegion2;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTwoProjectionImageRegistrationMethod.hxx"
#endif

#endif
