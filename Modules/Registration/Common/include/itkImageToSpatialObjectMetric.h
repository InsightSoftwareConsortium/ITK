/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageToSpatialObjectMetric_h
#define itkImageToSpatialObjectMetric_h

#include "itkSingleValuedCostFunction.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkLinearInterpolateImageFunction.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkTransform.h"

namespace itk
{
/** \class ImageToSpatialObjectMetric
 * \brief Computes similarity between a moving spatial object
 *        and an Image to be registered
 *
 *  The ImageToSpatialObjectMetric is different from the rest of the
 *  registration framework in ITK regarding the interpretation of the Transform
 *  with respect to the Fixed and Moving objects. In most of the ITK
 *  registration framework, the Transform computed by the optimizer is the one
 *  that maps points from the space of the Fixed object into the space of the
 *  Moving object. This direction of the transform is the one that makes easier
 *  to resample the Moving object into the space of the Fixed object.
 *
 *  In the particular case of ImageToSpatialObject registration, the
 *  Transform to be computed is the one mapping points from the SpatialObject
 *  into the Image.  This allows the SpatialObject to drive the location and
 *  geometry of the measurements made in the image - thereby if the spatial
 *  object is sparse and/or contains varying geometric features, the metric
 *  applied to measure how well that object matches with the image can be
 *  rapidly computed only at the sparse locations and using measures that
 *  match the local geometry that should be at those locations in the image.
 *  This is particularly useful for fast intra-operative registration when the
 *  pre-operative data is used to define a SpatialObject and can anticipate
 *  how that object will appear in the intra-operative images.
 *
 *  A full discussion of the Transform directions in the ITK registration
 *  framework can be found in the ITK Software Guide.
 *
 * \ingroup ITKRegistrationCommon
 */

template <typename TFixedImage, typename TMovingSpatialObject>
class ITK_TEMPLATE_EXPORT ImageToSpatialObjectMetric : public SingleValuedCostFunction
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToSpatialObjectMetric);

  using Self = ImageToSpatialObjectMetric;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type of the fixed image */
  using FixedImageType = TFixedImage;

  /** Type of the MovingSpatialObject */
  using MovingSpatialObjectType = TMovingSpatialObject;

  /** Type used for representing point components  */
  using CoordinateRepresentationType = Superclass::ParametersValueType;

  /** Image dimension enumeration. */
  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension;

  /** Object dimension enumeration. */
  static constexpr unsigned int ObjectDimension = MovingSpatialObjectType::ObjectDimension;

  /**  Type of the Transform Base class */
  using TransformType = Transform<CoordinateRepresentationType, Self::ObjectDimension, Self::ImageDimension>;

  using TransformPointer = typename TransformType::Pointer;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using TransformParametersType = typename TransformType::ParametersType;
  using TransformJacobianType = typename TransformType::JacobianType;

  /**  Type of the Interpolator Base class */
  using InterpolatorType = LinearInterpolateImageFunction<TFixedImage, CoordinateRepresentationType>;

  using InterpolatorPointer = typename InterpolatorType::Pointer;

  /** Typede of the vector type to return derivatives */
  using VectorType = vnl_vector_fixed<double, Self::ObjectDimension>;

  /**  Type of the match measure */
  using MeasureType = Superclass::MeasureType;

  /** Type of the derivative of the match measure */
  using DerivativeType = Superclass::DerivativeType;

  /** Pointer type for the FixedImage  */
  using FixedImagePointer = typename FixedImageType::Pointer;

  /** Pointer type for the MovingSpatialObject */
  using MovingSpatialObjectPointer = typename MovingSpatialObjectType::Pointer;

  /** Const pointer type for the FixedImage */
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;

  /** Const pointer type for the MovingSpatialObject */
  using MovingSpatialObjectConstPointer = typename MovingSpatialObjectType::ConstPointer;

  /**  ParametersType type alias.
   *  It defines a position in the optimization search space. */
  using ParametersType = Superclass::ParametersType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToSpatialObjectMetric, Object);

  /** Get/Set the FixedImage. */
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  itkGetConstObjectMacro(FixedImage, FixedImageType);

  /** Get/Set the MovingSpatialObject */
  itkSetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);
  itkGetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);

  /** Connect the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);

  /** Get the Interpolator. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Get Value and Derivatives for MultipleValuedOptimizers */
  void
  GetValueAndDerivative(const ParametersType & parameters,
                        MeasureType &          Value,
                        DerivativeType &       Derivative) const override = 0;

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override;

  /** Initialize the metric */
  virtual void
  Initialize();

  /** Get the last transformation parameters visited by
   * the optimizer. This function overload the superclass's one */
  itkGetConstReferenceMacro(LastTransformParameters, ParametersType);

  /** Set/Get the Transform. */
  itkSetObjectMacro(Transform, TransformType);

protected:
  ImageToSpatialObjectMetric();
  ~ImageToSpatialObjectMetric() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  MeasureType              m_MatchMeasure{ 0 };
  DerivativeType           m_MatchMeasureDerivatives;
  mutable TransformPointer m_Transform;
  InterpolatorPointer      m_Interpolator;

  MovingSpatialObjectConstPointer m_MovingSpatialObject;
  FixedImageConstPointer          m_FixedImage;
  ParametersType                  m_LastTransformParameters;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToSpatialObjectMetric.hxx"
#endif

#endif
