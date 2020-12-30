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
 * and searches for a Transform that will map points from the space of the
 * Fixed image to the space of the Moving image.
 *
 * For doing so, a Metric will be continuously applied to compare the Fixed
 * image with the Transformed Moving image. This process also requires to
 * interpolate values from the Moving image.
 *
 * \ingroup RegistrationFilters
 * \ingroup ITKRegistrationCommon
 *
 * \sphinx
 * \sphinxexample{Registration/Common/GlobalRegistrationOfTwoImages,Global Registration Of Two Images}
 * \sphinxexample{Core/Transform/GlobalRegistrationTwoImagesAffine,Global Registration Two Images (Affine)}
 * \sphinxexample{Core/Transform/GlobalRegistrationTwoImagesBSpline,Global Registration Of Two Images (BSpline)}
 * \endsphinx
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT ImageRegistrationMethod : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageRegistrationMethod);

  /** Standard class type aliases. */
  using Self = ImageRegistrationMethod;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegistrationMethod, ProcessObject);

  /**  Type of the Fixed image. */
  using FixedImageType = TFixedImage;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;

  /**  Type of the Moving image. */
  using MovingImageType = TMovingImage;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /**  Type of the metric. */
  using MetricType = ImageToImageMetric<FixedImageType, MovingImageType>;
  using MetricPointer = typename MetricType::Pointer;
  using FixedImageRegionType = typename MetricType::FixedImageRegionType;

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
  void
  SetFixedImage(const FixedImageType * fixedImage);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Set/Get the Moving image. */
  void
  SetMovingImage(const MovingImageType * movingImage);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

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
  SetFixedImageRegion(const FixedImageRegionType & region);

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
  virtual void
  Initialize();

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
  ImageRegistrationMethod();
  ~ImageRegistrationMethod() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  void
  GenerateData() override;

  /** Provides derived classes with the ability to set this private var */
  itkSetMacro(LastTransformParameters, ParametersType);

  /* Start the Optimization */
  void
  StartOptimization();

private:
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
#  include "itkImageRegistrationMethod.hxx"
#endif

#endif
