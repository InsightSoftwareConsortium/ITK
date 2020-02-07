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
#ifndef itkANTSNeighborhoodCorrelationImageToImageMetricv4_h
#define itkANTSNeighborhoodCorrelationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

/** \class ANTSNeighborhoodCorrelationImageToImageMetricv4
 *
 * \brief Computes normalized cross correlation using a small neighborhood
 * for each voxel between two images, with speed optimizations for dense
 * registration.
 *
 * Please cite this reference for more details:
 *
 * Brian B. Avants, Nicholas J. Tustison, Gang Song, Philip A. Cook,
 * Arno Klein, James C. Gee, A reproducible evaluation of ANTs similarity metric
 * performance in brain image registration, NeuroImage, Volume 54, Issue 3,
 * 1 February 2011, Pages 2033-2044, ISSN 1053-8119,
 * DOI: 10.1016/j.neuroimage.2010.09.025.
 *
 * Around each voxel, the neighborhood is defined as a N-Dimensional
 * rectangle centered at the voxel. The size of the rectangle is 2*radius+1.
 * The normalized correlation between neighborhoods of fixed image and moving
 * image are averaged over the whole image as the final metric.
 * \note A radius less than 2 can be unstable. 2 is the default.
 *
 * This class uses a specific fast implementation that is described in the
 * above paper. There are two particular speed-ups:
 *
 * 1) It is assumed that the derivative is only affected by changes in the
 * transform at the center of the window. This is obviously not true but speeds
 * the evaluation up considerably and works well in practice. This assumption
 * is the main differentiation of this approach from a more generic one.
 *
 * 2) The evaluation uses on-the-fly queues with multi-threading and a sliding
 * neighborhood window. This is described in the above paper and specifically
 * optimized for dense registration.
 *
 *  Example of usage:
 *
 *  using MetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4
 *    <ImageType, ImageType>;
 *  using MetricTypePointer = MetricType::Pointer;
 *  MetricTypePointer metric = MetricType::New();
 *
 *  // set all parameters
 *  Size<Dimension> neighborhoodRadius;
 *  neighborhoodRadius.Fill(2);
 *  metric->SetRadius(neighborhood_radius);
 *  metric->SetFixedImage(fixedImage);
 *  metric->SetMovingImage(movingImage);
 *  metric->SetFixedTransform(transformFix);
 *  metric->SetMovingTransform(transformMov);
 *
 *  // initialization after parameters are set.
 *  metric->Initialize();
 *
 *  // getting derivative and metric value
 *  metric->GetValueAndDerivative(valueReturn, derivativeReturn);
 *
 *
 * This class is templated over the type of the two input objects.
 * This is the base class for a hierarchy of similarity metrics that may, in
 * derived classes, operate on meshes, images, etc.  This class computes a
 * value that measures the similarity between the two objects.
 *
 * \note Sparse sampling is not supported by this metric. An exception will be
 * thrown if m_UseSampledPointSet is set. Support for sparse sampling
 * will require a parallel implementation of the neighborhood scanning, which
 * currently caches information as the neighborhood window moves.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits =
            DefaultImageToImageMetricTraitsv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType>>
class ITK_TEMPLATE_EXPORT ANTSNeighborhoodCorrelationImageToImageMetricv4
  : public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ANTSNeighborhoodCorrelationImageToImageMetricv4);

  /** Standard class type aliases. */
  using Self = ANTSNeighborhoodCorrelationImageToImageMetricv4;
  using Superclass =
    ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Superclass);

  /** superclass types */
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename Superclass::DerivativeValueType;
  using VirtualPointType = typename Superclass::VirtualPointType;
  using FixedImagePointType = typename Superclass::FixedImagePointType;
  using FixedImagePixelType = typename Superclass::FixedImagePixelType;
  using FixedTransformType = typename Superclass::FixedTransformType;
  using FixedImageGradientType = typename Superclass::FixedImageGradientType;
  using FixedImageJacobianType = typename FixedTransformType::JacobianType;

  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using MovingImagePixelType = typename Superclass::MovingImagePixelType;
  using MovingImageGradientType = typename Superclass::MovingImageGradientType;
  using MovingTransformType = typename Superclass::MovingTransformType;
  using MovingImageJacobianType = typename MovingTransformType::JacobianType;
  using JacobianType = typename Superclass::JacobianType;

  using VirtualImageGradientType = typename Superclass::VirtualImageGradientType;

  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using VirtualImageType = typename Superclass::VirtualImageType;
  using FixedOutputPointType = typename Superclass::FixedOutputPointType;
  using MovingOutputPointType = typename Superclass::MovingOutputPointType;

  using FixedTransformJacobianType = typename Superclass::FixedTransformType::JacobianType;
  using MovingTransformJacobianType = typename Superclass::MovingTransformType::JacobianType;

  using NumberOfParametersType = typename Superclass::NumberOfParametersType;
  using ImageDimensionType = typename Superclass::ImageDimensionType;

  using ImageRegionType = typename VirtualImageType::RegionType;
  using RadiusType = typename VirtualImageType::SizeType;
  using IndexType = typename VirtualImageType::IndexType;

  /* Image dimension accessors */
  static constexpr ImageDimensionType FixedImageDimension = FixedImageType::ImageDimension;

  static constexpr ImageDimensionType MovingImageDimension = MovingImageType::ImageDimension;

  static constexpr ImageDimensionType VirtualImageDimension = VirtualImageType::ImageDimension;

  // Set the radius of the neighborhood window centered at each pixel.
  // See the note above about using a radius less than 2.
  itkSetMacro(Radius, RadiusType);

  // Get the Radius of the neighborhood window centered at each pixel
  itkGetMacro(Radius, RadiusType);
  itkGetConstMacro(Radius, RadiusType);

  void
  Initialize() override;

protected:
  ANTSNeighborhoodCorrelationImageToImageMetricv4();
  ~ANTSNeighborhoodCorrelationImageToImageMetricv4() override = default;

  friend class ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader<
    ThreadedImageRegionPartitioner<VirtualImageDimension>,
    Superclass,
    Self>;
  using ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreaderType =
    ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader<
      ThreadedImageRegionPartitioner<VirtualImageDimension>,
      Superclass,
      Self>;

  friend class ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader<
    ThreadedIndexedContainerPartitioner,
    Superclass,
    Self>;
  using ANTSNeighborhoodCorrelationImageToImageMetricv4SparseGetValueAndDerivativeThreaderType =
    ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader<ThreadedIndexedContainerPartitioner,
                                                                                 Superclass,
                                                                                 Self>;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  // Radius of the neighborhood window centered at each pixel
  RadiusType m_Radius;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.hxx"
#endif

#endif
