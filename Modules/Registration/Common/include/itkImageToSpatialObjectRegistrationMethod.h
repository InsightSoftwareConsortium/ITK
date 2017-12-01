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
#ifndef itkImageToSpatialObjectRegistrationMethod_h
#define itkImageToSpatialObjectRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkImageToSpatialObjectMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
/** \class ImageToSpatialObjectRegistrationMethod
 * \brief Base class for Image Registration Methods
 *
 *
 * This class is templated over the type of the image and the type
 * of the SpatialObject to be registered.
 * A generic Transform is used by this class. That allows
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
 * The terms : Fixed image and Moving SpatialObject are used in this class
 * to indicate that the SpatialObject is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed image and searchs
 * for a transform that will map the Moving SpatialObject on top of
 * the Fixed image. For doing so, a Metric will be continuously applied to
 * compare the Fixed image with the Transformed Moving SpatialObject.
 *
 * The ImageToSpatialObjectRegistrationMethod is different from the rest of the
 * registration framework in ITK regarding the interpretation of the Transform
 * with respect to the Fixed and Moving objects. In most of the ITK
 * registration framework, the Transform computed by the optimizer is the one
 * that maps points from the space of the Fixed object into the space of the
 * Moving object. This direction of the transform is the one that makes easier
 * to resample the Moving object into the space of the Fixed object.
 *
 * In the particular case of the ImageToSpatialObject registration, the
 * Transform to be computed is the one mapping points from the SpatialObject
 * into the Image, despite the fact that the SpatialObject is called the
 * "Moving" object and the image is called the "Fixed" object. This change of
 * reference system is the consequence of using this type of registration in
 * applications that are based on Visualization. In the context of such
 * visualizations it is simpler to think in terms of the Transform that can be
 * used for displaying the SpatialObject in the appropriate position with
 * respect to the image. Since this process does not involve resampling, but
 * providing a Transform to a visualization routine, it is usually more
 * natural to use the Transform that maps points from the SpatialObject space
 * the image space.
 *
 * A full discussion of the Transform directions in the ITK registration
 * framework can be found in the ITK Software Guide.
 *
 * \ingroup RegistrationFilters
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedImage, typename TMovingSpatialObject >
class ITK_TEMPLATE_EXPORT ImageToSpatialObjectRegistrationMethod : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageToSpatialObjectRegistrationMethod Self;
  typedef ProcessObject                          Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToSpatialObjectRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  typedef          TFixedImage                  FixedImageType;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /**  Type of the Moving image. */
  typedef TMovingSpatialObject MovingSpatialObjectType;
  typedef typename MovingSpatialObjectType::ConstPointer
  MovingSpatialObjectConstPointer;

  /**  Type of the metric. */
  typedef ImageToSpatialObjectMetric< FixedImageType,
                                      MovingSpatialObjectType >     MetricType;
  typedef typename MetricType::Pointer MetricPointer;

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
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Set/Get the Moving Spatial Object. */
  itkSetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);
  itkGetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);

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
  itkSetMacro(InitialTransformParameters, ParametersType);
  itkGetConstReferenceMacro(InitialTransformParameters, ParametersType);

  /** Get the last transformation parameters visited by
   * the optimizer. */
  itkGetConstReferenceMacro(LastTransformParameters, ParametersType);

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
  // StartRegistration is an old API from before
  // the RegistrationMethod was a subclass of ProcessObject.
  // Historically, one could call StartRegistration() instead of
  // calling Update().  However, when called directly by the user, the
  // inputs to the RegistrationMethod may not be up to date.  This
  // may cause an unexpected behavior.
  //
  // Since we cannot eliminate StartRegistration for ITKv3 backward
  // compatibility reasons, we check whether StartRegistration was
  // called directly or whether Update() (which in turn called
  // StartRegistration()).
  void StartRegistration(void) { this->Update(); }
#endif

protected:
  ImageToSpatialObjectRegistrationMethod();
  virtual ~ImageToSpatialObjectRegistrationMethod() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  virtual void  GenerateData() ITK_OVERRIDE;

  /** Initialize by setting the interconnects between the components. */
  void Initialize();

  ParametersType m_InitialTransformParameters;
  ParametersType m_LastTransformParameters;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToSpatialObjectRegistrationMethod);

  MetricPointer          m_Metric;
  OptimizerType::Pointer m_Optimizer;

  MovingSpatialObjectConstPointer m_MovingSpatialObject;
  FixedImageConstPointer          m_FixedImage;

  TransformPointer    m_Transform;
  InterpolatorPointer m_Interpolator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToSpatialObjectRegistrationMethod.hxx"
#endif

#endif
