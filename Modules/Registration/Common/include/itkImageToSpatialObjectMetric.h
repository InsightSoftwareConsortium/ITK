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
 * \brief Computes similarity between a moving spatial obejct
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
 *  In the particular case of the ImageToSpatialObject registration, the
 *  Transform to be computed is the one mapping points from the SpatialObject
 *  into the Image, despite the fact that the SpatialObject is called the
 *  "Moving" object and the image is called the "Fixed" object. This change of
 *  reference system is the consequence of using this type of registration in
 *  applications that are based on Visualization. In the context of such
 *  visualizations it is simpler to think in terms of the Transform that can be
 *  used for displaying the SpatialObject in the appropriate position with
 *  respect to the image. Since this process does not involve resampling, but
 *  providing a Transform to a visualization routine, it is usually more
 *  natural to use the Transform that maps points from the SpatialObject space
 *  the image space.
 *
 *  A full discussion of the Transform directions in the ITK registration
 *  framework can be found in the ITK Software Guide.
 *
 * \ingroup ITKRegistrationCommon
 */

template< typename TFixedImage, typename TMovingSpatialObject >
class ITK_TEMPLATE_EXPORT ImageToSpatialObjectMetric:
  public SingleValuedCostFunction
{
public:
  typedef ImageToSpatialObjectMetric Self;
  typedef SingleValuedCostFunction   Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Type of the fixed image */
  typedef TFixedImage FixedImageType;

  /** Type of the MovingSpatialObject */
  typedef TMovingSpatialObject MovingSpatialObjectType;

  /** Type used for representing point components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Image dimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      FixedImageType::ImageDimension);

  /** Object dimension enumeration. */
  itkStaticConstMacro(ObjectDimension, unsigned int,
                      MovingSpatialObjectType::ObjectDimension);

  /**  Type of the Transform Base class */
  typedef Transform< CoordinateRepresentationType,
                     itkGetStaticConstMacro(ObjectDimension),
                     itkGetStaticConstMacro(ImageDimension) > TransformType;

  typedef typename TransformType::Pointer         TransformPointer;
  typedef typename TransformType::InputPointType  InputPointType;
  typedef typename TransformType::OutputPointType OutputPointType;
  typedef typename TransformType::ParametersType  TransformParametersType;
  typedef typename TransformType::JacobianType    TransformJacobianType;

  /**  Type of the Interpolator Base class */
  typedef LinearInterpolateImageFunction<
    TFixedImage,
    CoordinateRepresentationType > InterpolatorType;

  typedef typename InterpolatorType::Pointer InterpolatorPointer;

  /** Typede of the vector type to return derivatives */
  typedef vnl_vector_fixed< double,
                            itkGetStaticConstMacro(ObjectDimension) > VectorType;

  /**  Type of the match measure */
  typedef Superclass::MeasureType MeasureType;

  /** Type of the derivative of the match measure */
  typedef Superclass::DerivativeType DerivativeType;

  /** Pointer type for the FixedImage  */
  typedef typename FixedImageType::Pointer FixedImagePointer;

  /** Pointer type for the MovingSpatialObject */
  typedef typename MovingSpatialObjectType::Pointer
  MovingSpatialObjectPointer;

  /** Const pointer type for the FixedImage */
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /** Const pointer type for the MovingSpatialObject */
  typedef typename MovingSpatialObjectType::ConstPointer MovingSpatialObjectConstPointer;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

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
  virtual void GetValueAndDerivative(const ParametersType & parameters,
                                     MeasureType & Value,
                                     DerivativeType  & Derivative) const ITK_OVERRIDE = 0;

  /** Return the number of parameters required by the Transform */
  virtual unsigned int GetNumberOfParameters( void ) const ITK_OVERRIDE;

  /** Initialize the metric */
  virtual void Initialize(void);

  /** Get the last transformation parameters visited by
   * the optimizer. This function overload the superclass's one */
  itkGetConstReferenceMacro(LastTransformParameters, ParametersType);

  /** Set/Get the Transform. */
  itkSetObjectMacro(Transform, TransformType);

protected:

  ImageToSpatialObjectMetric();
  virtual ~ImageToSpatialObjectMetric() ITK_OVERRIDE {}
  ImageToSpatialObjectMetric(const Self &) {}
  void operator=(const Self &) {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  MeasureType              m_MatchMeasure;
  DerivativeType           m_MatchMeasureDerivatives;
  mutable TransformPointer m_Transform;
  InterpolatorPointer      m_Interpolator;

  MovingSpatialObjectConstPointer m_MovingSpatialObject;
  FixedImageConstPointer          m_FixedImage;
  ParametersType                  m_LastTransformParameters;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToSpatialObjectMetric.hxx"
#endif

#endif
