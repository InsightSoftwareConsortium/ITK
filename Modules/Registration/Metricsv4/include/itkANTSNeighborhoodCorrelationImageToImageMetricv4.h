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
#ifndef itkANTSNeighborhoodCorrelationImageToImageMetricv4_h
#define itkANTSNeighborhoodCorrelationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk {

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
 *  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4
 *    <ImageType, ImageType> MetricType;
 *  typedef MetricType::Pointer MetricTypePointer;
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
 * thrown if m_UseFixedSampledPointSet is set. Support for sparse sampling
 * will require a parallel implementation of the neighborhood scanning, which
 * currently caches information as the neighborhood window moves.
 *
 * \ingroup ITKMetricsv4
 */
template<typename TFixedImage, typename TMovingImage, typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits = DefaultImageToImageMetricTraitsv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType>
          >
class ITK_TEMPLATE_EXPORT ANTSNeighborhoodCorrelationImageToImageMetricv4 :
  public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:

  /** Standard class typedefs. */
  typedef ANTSNeighborhoodCorrelationImageToImageMetricv4                  Self;
  typedef ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage,
                             TInternalComputationValueType,TMetricTraits>  Superclass;
  typedef SmartPointer<Self>                                               Pointer;
  typedef SmartPointer<const Self>                                         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Superclass);

  /** superclass types */
  typedef typename Superclass::MeasureType                    MeasureType;
  typedef typename Superclass::DerivativeType                 DerivativeType;
  typedef typename Superclass::DerivativeValueType            DerivativeValueType;
  typedef typename Superclass::VirtualPointType               VirtualPointType;
  typedef typename Superclass::FixedImagePointType            FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType            FixedImagePixelType;
  typedef typename Superclass::FixedTransformType             FixedTransformType;
  typedef typename Superclass::FixedImageGradientType         FixedImageGradientType;
  typedef typename FixedTransformType::JacobianType           FixedImageJacobianType;

  typedef typename Superclass::MovingImagePointType           MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType           MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType        MovingImageGradientType;
  typedef typename Superclass::MovingTransformType            MovingTransformType;
  typedef typename MovingTransformType::JacobianType          MovingImageJacobianType;
  typedef typename Superclass::JacobianType                   JacobianType;

  typedef typename Superclass::VirtualImageGradientType       VirtualImageGradientType;

  typedef typename Superclass::FixedImageType                 FixedImageType;
  typedef typename Superclass::MovingImageType                MovingImageType;
  typedef typename Superclass::VirtualImageType               VirtualImageType;
  typedef typename Superclass::FixedOutputPointType           FixedOutputPointType;
  typedef typename Superclass::MovingOutputPointType          MovingOutputPointType;

  typedef typename Superclass::FixedTransformType::JacobianType
                            FixedTransformJacobianType;
  typedef typename Superclass::MovingTransformType::JacobianType
                            MovingTransformJacobianType;

  typedef typename Superclass::NumberOfParametersType         NumberOfParametersType;
  typedef typename Superclass::ImageDimensionType             ImageDimensionType;

  typedef typename VirtualImageType::RegionType               ImageRegionType;
  typedef typename VirtualImageType::SizeType                 RadiusType;
  typedef typename VirtualImageType::IndexType                IndexType;

  /* Image dimension accessors */
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      FixedImageType::ImageDimension);

  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
        MovingImageType::ImageDimension);

  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
        VirtualImageType::ImageDimension);

  // Set the radius of the neighborhood window centered at each pixel.
  // See the note above about using a radius less than 2.
  itkSetMacro(Radius, RadiusType);

  // Get the Radius of the neighborhood window centered at each pixel
  itkGetMacro(Radius, RadiusType);
  itkGetConstMacro(Radius, RadiusType);

  void Initialize(void) ITK_OVERRIDE;

protected:
  ANTSNeighborhoodCorrelationImageToImageMetricv4();
  virtual ~ANTSNeighborhoodCorrelationImageToImageMetricv4() ITK_OVERRIDE;

  friend class ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< VirtualImageDimension >, Superclass, Self >;
  typedef ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< VirtualImageDimension >, Superclass, Self >
    ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreaderType;

  friend class ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;
  typedef ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    ANTSNeighborhoodCorrelationImageToImageMetricv4SparseGetValueAndDerivativeThreaderType;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ANTSNeighborhoodCorrelationImageToImageMetricv4);

  // Radius of the neighborhood window centered at each pixel
  RadiusType m_Radius;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.hxx"
#endif

#endif
