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
#ifndef itkImageRegistrationMethod_h
#define itkImageRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkImageToImageMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
/** \class ImageRegistrationMethod
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
 * For doing so, a Metric will be continuously applied to compare the Fixed
 * image with the Transformed Moving image. This process also requires to
 * interpolate values from the Moving image.
 *
 * \ingroup RegistrationFilters
 * \ingroup ITKRegistrationCommon
 *
 * \wiki
 * \wikiexample{Registration/ImageRegistrationMethod,A basic global registration of two images}
 * \wikiexample{Registration/ImageRegistrationMethodAffine,A global registration of two images}
 * \wikiexample{Registration/ImageRegistrationMethodBSpline,A global registration of two images}
 * \endwiki
 */
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT ImageRegistrationMethod:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageRegistrationMethod    Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  typedef          TFixedImage                  FixedImageType;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /**  Type of the Moving image. */
  typedef          TMovingImage                  MovingImageType;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /**  Type of the metric. */
  typedef ImageToImageMetric< FixedImageType, MovingImageType > MetricType;
  typedef typename MetricType::Pointer                          MetricPointer;
  typedef typename MetricType::FixedImageRegionType             FixedImageRegionType;

  /**  Type of the Transform . */
  typedef  typename MetricType::TransformType TransformType;
  typedef  typename TransformType::Pointer    TransformPointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  typedef  DataObjectDecorator< TransformType >      TransformOutputType;
  typedef typename TransformOutputType::Pointer      TransformOutputPointer;
  typedef typename TransformOutputType::ConstPointer TransformOutputConstPointer;

  /**  Type of the Interpolator. */
  typedef  typename MetricType::InterpolatorType InterpolatorType;
  typedef  typename InterpolatorType::Pointer    InterpolatorPointer;

  /**  Type of the optimizer. */
  typedef   SingleValuedNonLinearOptimizer OptimizerType;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef  typename MetricType::TransformParametersType ParametersType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Set/Get the Fixed image. */
  void SetFixedImage(const FixedImageType *fixedImage);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Set/Get the Moving image. */
  void SetMovingImage(const MovingImageType *movingImage);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Set/Get the Optimizer. */
  itkSetObjectMacro(Optimizer,  OptimizerType);
  itkGetModifiableObjectMacro(Optimizer, OptimizerType);

  /** Set/Get the Metric. */
  itkSetObjectMacro(Metric, MetricType);
  itkGetModifiableObjectMacro(Metric, MetricType);

  /** Set/Get the Transfrom. */
  itkSetObjectMacro(Transform, TransformType);
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Set/Get the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set/Get the initial transformation parameters. */
  virtual void SetInitialTransformParameters(const ParametersType & param);

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
  void SetFixedImageRegion(const FixedImageRegionType & region);

  /** Get the region of the fixed image to be considered as region of
   interest during the registration. This region will be passed to
   the ImageMetric in order to restrict the metric computation to
   consider only this region.  */
  itkGetConstReferenceMacro(FixedImageRegion, FixedImageRegionType);
  /** True if a region has been defined for the fixed image to which
   the ImageMetric will limit its computation */
  itkGetConstMacro(FixedImageRegionDefined, bool);
  /** Turn on/off the use of a fixed image region to which
   the ImageMetric will limit its computation.
   \warning The region must have been previously defined using the
   SetFixedImageRegion member function */
  itkSetMacro(FixedImageRegionDefined, bool);

  /** Initialize by setting the interconnects between the components. */
  virtual void Initialize();

  /** Returns the transform resulting from the registration process  */
  const TransformOutputType * GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  /** Method to return the latest modified time of this object or
   * any of its cached ivars */
  virtual ModifiedTimeType GetMTime() const ITK_OVERRIDE;

#ifdef ITKV3_COMPATIBILITY
  /** Method that initiates the registration. This will Initialize and ensure
   * that all inputs the registration needs are in place, via a call to
   * Initialize() will then start the optimization process via a call to
   * StartOptimization()
   * StartRegistration is an old API from before
   * ImageRegistrationMethod was a subclass of ProcessObject.
   * Historically, one could call StartRegistration() instead of
   * calling Update().  However, when called directly by the user, the
   * inputs to ImageRegistrationMethod may not be up to date.  This
   * may cause an unexpected behavior.
   *
   * Since we cannot eliminate StartRegistration for backward
   * compatibility reasons, we check whether StartRegistration was
   * called directly or whether Update() (which in turn called
   * StartRegistration()). */
  void StartRegistration(void) { this->Update(); }
#endif

protected:
  ImageRegistrationMethod();
  virtual ~ImageRegistrationMethod() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  virtual void  GenerateData() ITK_OVERRIDE;

  /** Provides derived classes with the ability to set this private var */
  itkSetMacro(LastTransformParameters, ParametersType);

  /* Start the Optimization */
  void StartOptimization();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageRegistrationMethod);

  MetricPointer          m_Metric;
  OptimizerType::Pointer m_Optimizer;

  MovingImageConstPointer m_MovingImage;
  FixedImageConstPointer  m_FixedImage;

  TransformPointer    m_Transform;
  InterpolatorPointer m_Interpolator;

  ParametersType m_InitialTransformParameters;
  ParametersType m_LastTransformParameters;

  bool                 m_FixedImageRegionDefined;
  FixedImageRegionType m_FixedImageRegion;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegistrationMethod.hxx"
#endif

#endif
