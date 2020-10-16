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
#ifndef itkTwoImageToOneImageMetric_h
#define itkTwoImageToOneImageMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkInterpolateImageFunction.h"
#include "itkSingleValuedCostFunction.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkSpatialObject.h"

namespace itk
{

/** \class TwoImageToOneImageMetric
 * \brief Computes similarity between two fixed images and one fixed image.
 *
 * This Class is templated over the type of the two input images.
 * It expects a Transform and two Interpolators to be plugged in.
 * This particular class is the base class for a hierarchy of
 * similarity metrics.
 *
 * This class computes a value that measures the similarity
 * between two Fixed image and the transformed Moving images.
 * The Interpolators are used to compute intensity values on
 * non-grid positions resulting from mapping points through
 * the Transform.
 *
 *
 * \ingroup RegistrationMetrics
 * \ingroup TwoProjectionRegistration
 *
 */

template <typename TFixedImage, typename TMovingImage>
class TwoImageToOneImageMetric : public SingleValuedCostFunction
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TwoImageToOneImageMetric);

  /** Standard class type alias. */
  using Self = TwoImageToOneImageMetric;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type used for representing point components  */
  using CoordinateRepresentationType = Superclass::ParametersValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TwoImageToOneImageMetric, SingleValuedCostFunction);

  /**  Type of the moving Image. */
  using MovingImageType = TMovingImage;
  using MovingImagePixelType = typename TMovingImage::PixelType;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /**  Type of the fixed Image. */
  using FixedImageType = TFixedImage;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;
  using FixedImageRegionType = typename FixedImageType::RegionType;

  /** Constants for the image dimensions */
  static constexpr unsigned int MovingImageDimension = TMovingImage::ImageDimension;
  static constexpr unsigned int FixedImageDimension = TFixedImage::ImageDimension;

  /**  Type of the Transform Base class */
  using TransformType = Transform<CoordinateRepresentationType,
                                  itkGetStaticConstMacro(MovingImageDimension),
                                  itkGetStaticConstMacro(FixedImageDimension)>;

  using TransformPointer = typename TransformType::Pointer;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using TransformParametersType = typename TransformType::ParametersType;
  using TransformJacobianType = typename TransformType::JacobianType;

  /**  Type of the Interpolator Base class */
  using InterpolatorType = InterpolateImageFunction<MovingImageType, CoordinateRepresentationType>;


  /** Gaussian filter to compute the gradient of the Moving Image */
  using RealType = typename NumericTraits<MovingImagePixelType>::RealType;
  using GradientPixelType = CovariantVector<RealType, itkGetStaticConstMacro(MovingImageDimension)>;
  using GradientImageType = Image<GradientPixelType, itkGetStaticConstMacro(MovingImageDimension)>;
  using GradientImagePointer = SmartPointer<GradientImageType>;
  using GradientImageFilterType = GradientRecursiveGaussianImageFilter<MovingImageType, GradientImageType>;
  using GradientImageFilterPointer = typename GradientImageFilterType::Pointer;
  using InterpolatorPointer = typename InterpolatorType::Pointer;

  /**  Type for the mask of the fixed image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject<itkGetStaticConstMacro(FixedImageDimension)> FixedImageMaskType;
  using FixedImageMaskPointer = typename FixedImageMaskType::Pointer;

  /**  Type for the mask of the moving image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject<itkGetStaticConstMacro(MovingImageDimension)> MovingImageMaskType;
  using MovingImageMaskPointer = typename MovingImageMaskType::Pointer;


  /**  Type of the measure. */
  using MeasureType = Superclass::MeasureType;

  /**  Type of the derivative. */
  using DerivativeType = Superclass::DerivativeType;

  /**  Type of the parameters. */
  using ParametersType = Superclass::ParametersType;

  /** Connect the Fixed Image.  */
  itkSetConstObjectMacro(FixedImage1, FixedImageType);

  /** Connect the Fixed Image.  */
  itkSetConstObjectMacro(FixedImage2, FixedImageType);

  /** Get the Fixed Image. */
  itkGetConstObjectMacro(FixedImage1, FixedImageType);

  /** Get the Fixed Image. */
  itkGetConstObjectMacro(FixedImage2, FixedImageType);

  /** Connect the Moving Image.  */
  itkSetConstObjectMacro(MovingImage, MovingImageType);

  /** Get the Moving Image. */
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);

  /** Get a pointer to the Transform.  */
  itkGetConstObjectMacro(Transform, TransformType);

  /** Connect the Interpolator. */
  itkSetObjectMacro(Interpolator1, InterpolatorType);

  /** Connect the Interpolator. */
  itkSetObjectMacro(Interpolator2, InterpolatorType);

  /** Get a pointer to the Interpolator.  */
  itkGetConstObjectMacro(Interpolator1, InterpolatorType);

  /** Get a pointer to the Interpolator.  */
  itkGetConstObjectMacro(Interpolator2, InterpolatorType);

  /** Get the number of pixels considered in the computation. */
  itkGetConstReferenceMacro(NumberOfPixelsCounted, unsigned long);

  /** Set the region over which the metric will be computed */
  itkSetMacro(FixedImageRegion1, FixedImageRegionType);

  /** Set the region over which the metric will be computed */
  itkSetMacro(FixedImageRegion2, FixedImageRegionType);

  /** Get the region over which the metric will be computed */
  itkGetConstReferenceMacro(FixedImageRegion1, FixedImageRegionType);

  /** Get the region over which the metric will be computed */
  itkGetConstReferenceMacro(FixedImageRegion2, FixedImageRegionType);

  /** Set/Get the moving image mask. */
  itkSetObjectMacro(MovingImageMask, MovingImageMaskType);
  itkGetConstObjectMacro(MovingImageMask, MovingImageMaskType);

  /** Set/Get the fixed image mask. */
  itkSetObjectMacro(FixedImageMask1, FixedImageMaskType);
  itkSetObjectMacro(FixedImageMask2, FixedImageMaskType);
  itkGetConstObjectMacro(FixedImageMask1, FixedImageMaskType);
  itkGetConstObjectMacro(FixedImageMask2, FixedImageMaskType);

  /** Set/Get gradient computation. */
  itkSetMacro(ComputeGradient, bool);
  itkGetConstReferenceMacro(ComputeGradient, bool);
  itkBooleanMacro(ComputeGradient);

  /** Get Gradient Image. */
  itkGetConstObjectMacro(GradientImage, GradientImageType);

  /** Set the parameters defining the Transform. */
  void
  SetTransformParameters(const ParametersType & parameters) const;

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override
  {
    return m_Transform->GetNumberOfParameters();
  }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void
  Initialize();

protected:
  TwoImageToOneImageMetric();
  ~TwoImageToOneImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  mutable unsigned long m_NumberOfPixelsCounted;

  FixedImageConstPointer  m_FixedImage1;
  FixedImageConstPointer  m_FixedImage2;
  MovingImageConstPointer m_MovingImage;

  mutable TransformPointer m_Transform;
  InterpolatorPointer      m_Interpolator1;
  InterpolatorPointer      m_Interpolator2;

  bool                 m_ComputeGradient;
  GradientImagePointer m_GradientImage;

  mutable FixedImageMaskPointer  m_FixedImageMask1;
  mutable FixedImageMaskPointer  m_FixedImageMask2;
  mutable MovingImageMaskPointer m_MovingImageMask;

private:
  FixedImageRegionType m_FixedImageRegion1;
  FixedImageRegionType m_FixedImageRegion2;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTwoImageToOneImageMetric.hxx"
#endif

#endif
