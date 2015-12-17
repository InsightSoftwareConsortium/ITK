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
#ifndef itkTwoImageToOneImageMetric_h
#define itkTwoImageToOneImageMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkInterpolateImageFunction.h"
#include "itkSingleValuedCostFunction.h"
#include "itkExceptionObject.h"
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
  /** Standard class typedefs. */
  typedef TwoImageToOneImageMetric Self;
  typedef SingleValuedCostFunction Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Type used for representing point components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TwoImageToOneImageMetric, SingleValuedCostFunction);

  /**  Type of the moving Image. */
  typedef TMovingImage                           MovingImageType;
  typedef typename TMovingImage::PixelType       MovingImagePixelType;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /**  Type of the fixed Image. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;
  typedef typename FixedImageType::RegionType   FixedImageRegionType;

  /** Constants for the image dimensions */
  itkStaticConstMacro(MovingImageDimension, unsigned int, TMovingImage::ImageDimension);
  itkStaticConstMacro(FixedImageDimension, unsigned int, TFixedImage::ImageDimension);

  /**  Type of the Transform Base class */
  typedef Transform<CoordinateRepresentationType,
                    itkGetStaticConstMacro(MovingImageDimension),
                    itkGetStaticConstMacro(FixedImageDimension)>
    TransformType;

  typedef typename TransformType::Pointer         TransformPointer;
  typedef typename TransformType::InputPointType  InputPointType;
  typedef typename TransformType::OutputPointType OutputPointType;
  typedef typename TransformType::ParametersType  TransformParametersType;
  typedef typename TransformType::JacobianType    TransformJacobianType;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction<MovingImageType, CoordinateRepresentationType> InterpolatorType;


  /** Gaussian filter to compute the gradient of the Moving Image */
  typedef typename NumericTraits<MovingImagePixelType>::RealType                   RealType;
  typedef CovariantVector<RealType, itkGetStaticConstMacro(MovingImageDimension)>  GradientPixelType;
  typedef Image<GradientPixelType, itkGetStaticConstMacro(MovingImageDimension)>   GradientImageType;
  typedef SmartPointer<GradientImageType>                                          GradientImagePointer;
  typedef GradientRecursiveGaussianImageFilter<MovingImageType, GradientImageType> GradientImageFilterType;
  typedef typename GradientImageFilterType::Pointer                                GradientImageFilterPointer;


  typedef typename InterpolatorType::Pointer InterpolatorPointer;


  /**  Type for the mask of the fixed image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject<itkGetStaticConstMacro(FixedImageDimension)> FixedImageMaskType;
  typedef typename FixedImageMaskType::Pointer                       FixedImageMaskPointer;

  /**  Type for the mask of the moving image. Only pixels that are "inside"
       this mask will be considered for the computation of the metric */
  typedef SpatialObject<itkGetStaticConstMacro(MovingImageDimension)> MovingImageMaskType;
  typedef typename MovingImageMaskType::Pointer                       MovingImageMaskPointer;


  /**  Type of the measure. */
  typedef Superclass::MeasureType MeasureType;

  /**  Type of the derivative. */
  typedef Superclass::DerivativeType DerivativeType;

  /**  Type of the parameters. */
  typedef Superclass::ParametersType ParametersType;

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
  GetNumberOfParameters() const
  {
    return m_Transform->GetNumberOfParameters();
  }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void
  Initialize();

protected:
  TwoImageToOneImageMetric();
  virtual ~TwoImageToOneImageMetric() {};
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const;

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
  TwoImageToOneImageMetric(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  FixedImageRegionType m_FixedImageRegion1;
  FixedImageRegionType m_FixedImageRegion2;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTwoImageToOneImageMetric.hxx"
#endif

#endif
