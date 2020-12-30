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
 * This class uses the coordinate system of the Fixed image and searches
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
template <typename TFixedImage, typename TMovingSpatialObject>
class ITK_TEMPLATE_EXPORT ImageToSpatialObjectRegistrationMethod : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToSpatialObjectRegistrationMethod);

  /** Standard class type aliases. */
  using Self = ImageToSpatialObjectRegistrationMethod;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToSpatialObjectRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  using FixedImageType = TFixedImage;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;

  /**  Type of the Moving image. */
  using MovingSpatialObjectType = TMovingSpatialObject;
  using MovingSpatialObjectConstPointer = typename MovingSpatialObjectType::ConstPointer;

  /**  Type of the metric. */
  using MetricType = ImageToSpatialObjectMetric<FixedImageType, MovingSpatialObjectType>;
  using MetricPointer = typename MetricType::Pointer;

  /**  Type of the Transform . */
  using TransformType = typename MetricType::TransformType;
  using TransformPointer = typename TransformType::Pointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  using TransformOutputType = DataObjectDecorator<TransformType>;
  using TransformOutputPointer = typename TransformOutputType::Pointer;
  using TransformOutputConstPointer = typename TransformOutputType::ConstPointer;

  /**  Type of the Interpolator. */
  using InterpolatorType = typename MetricType::InterpolatorType;
  using InterpolatorPointer = typename InterpolatorType::Pointer;

  /**  Type of the optimizer. */
  using OptimizerType = SingleValuedNonLinearOptimizer;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  using ParametersType = typename MetricType::TransformParametersType;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Set/Get the Fixed image. */
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Set/Get the Moving Spatial Object. */
  itkSetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);
  itkGetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);

  /** Set/Get the Optimizer. */
  itkSetObjectMacro(Optimizer, OptimizerType);
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
  const TransformOutputType *
  GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType output) override;

  /** Method to return the latest modified time of this object or
   * any of its cached ivars */
  ModifiedTimeType
  GetMTime() const override;

protected:
  ImageToSpatialObjectRegistrationMethod();
  ~ImageToSpatialObjectRegistrationMethod() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  void
  GenerateData() override;

  /** Initialize by setting the interconnects between the components. */
  void
  Initialize();

  ParametersType m_InitialTransformParameters;
  ParametersType m_LastTransformParameters;

private:
  MetricPointer          m_Metric;
  OptimizerType::Pointer m_Optimizer;

  MovingSpatialObjectConstPointer m_MovingSpatialObject;
  FixedImageConstPointer          m_FixedImage;

  TransformPointer    m_Transform;
  InterpolatorPointer m_Interpolator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToSpatialObjectRegistrationMethod.hxx"
#endif

#endif
