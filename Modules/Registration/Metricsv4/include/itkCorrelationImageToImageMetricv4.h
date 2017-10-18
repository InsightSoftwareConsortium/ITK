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
#ifndef itkCorrelationImageToImageMetricv4_h
#define itkCorrelationImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"

#include "itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkCorrelationImageToImageMetricv4HelperThreader.h"

namespace itk
{

/** \class CorrelationImageToImageMetricv4
 *
 *  \brief Class implementing normalized cross correlation image metric.
 *
 *  Definition of the normalized cross correlation metric used here:
 *
 *  negative square of normalized cross correlation
 *
 *  \f[
 *  C(f, m) = -\frac{<f-\bar{f}, m-\bar{m} >^2}{|f-\bar{f}|^2 |m-\bar{m}|^2}
 *  \f]
 *
 *  in which, f, m are the vectors of image pixel intensities, \f$\bar{f}\f$ and
 *  \f$\bar{m}\f$ are the mean values of f and m. <,> denotes inner product, \f$|\cdot|\f$
 *  denotes the 2-norm of the vector. The minus sign makes the metric to optimize
 *  towards its minimal value. Note that
 *  this uses the *square* of the mathematical notion of normalized cross correlation
 *  to avoid the square root computation in practice.
 *
 *  Moving image (m) is a function of the parameters (p) of the moving transforms. So
 *  \f$ C(f, m) = C(f, m(p)) \f$
 *  GetValueAndDerivative will return the value as \f$ C(f,m) \f$ and the derivative as
 *
 *  \f[
 *  \frac{d}{dp} C = 2 \frac{<f1, m1>}{|f1|^2 |m1|^2} * (
 *                      <f1, \frac{dm}{dp}>
 *                      - \frac{<f1, m1>}{|m1|^2} < m1, \frac{dm}{dp} > )
 *  \f]
 *
 *  in which, \f$ f1 = f - \bar{f} \f$, \f$ m1 = m - \bar{m} \f$
 *  (Note: there should be a minus sign of \f$ \frac{d}{dp} \f$ mathematically, which
 *  is not in the implementation to match the requirement of the metricv4
 *  optimization framework.
 *
 *  See
 *  CorrelationImageToImageMetricv4GetValueAndDerivativeThreader::ProcessPoint
 *  for algorithm implementation.
 *
 *  This metric only works with the global transform. It throws an exception if the
 *  transform has local support.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedImage, typename TMovingImage, typename TVirtualImage = TFixedImage,
          typename TInternalComputationValueType = double,
          typename TMetricTraits = DefaultImageToImageMetricTraitsv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType>
          >
class ITK_TEMPLATE_EXPORT CorrelationImageToImageMetricv4 :
  public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
{
public:
  /** Standard class typedefs. */
  typedef CorrelationImageToImageMetricv4                                           Self;
  typedef ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage,
                             TInternalComputationValueType,TMetricTraits>  Superclass;
  typedef SmartPointer<Self>                                                        Pointer;
  typedef SmartPointer<const Self>                                                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CorrelationImageToImageMetricv4, ImageToImageMetricv4);

  /** Superclass types */
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;

  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;

  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;

  typedef typename Superclass::MovingTransformType        MovingTransformType;
  typedef typename Superclass::JacobianType               JacobianType;
  typedef typename Superclass::VirtualImageType           VirtualImageType;
  typedef typename Superclass::VirtualIndexType           VirtualIndexType;
  typedef typename Superclass::VirtualPointType           VirtualPointType;
  typedef typename Superclass::VirtualPointSetType        VirtualPointSetType;

  /* Image dimension accessors */
  itkStaticConstMacro(VirtualImageDimension, typename TVirtualImage::ImageDimensionType, TVirtualImage::ImageDimension);
  itkStaticConstMacro(FixedImageDimension,   typename TFixedImage::ImageDimensionType,   TFixedImage::ImageDimension);
  itkStaticConstMacro(MovingImageDimension,  typename TMovingImage::ImageDimensionType,  TMovingImage::ImageDimension);

protected:
  CorrelationImageToImageMetricv4();
  virtual ~CorrelationImageToImageMetricv4() ITK_OVERRIDE;

  /** Perform any initialization required before each evaluation of
   * \c GetValueAndDerivative. This is distinct from Initialize, which
   * is called only once before a number of iterations, e.g. before
   * a registration loop. */
  virtual void InitializeForIteration() const ITK_OVERRIDE;

  friend class ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Self >;
  friend class ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedIndexedContainerPartitioner, Self >;
  friend class ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Self >;
  friend class ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Self >;

  friend class CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >;
  friend class CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;
  typedef CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >
    CorrelationDenseGetValueAndDerivativeThreaderType;
  typedef CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    CorrelationSparseGetValueAndDerivativeThreaderType;

  friend class CorrelationImageToImageMetricv4HelperThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >;
  friend class CorrelationImageToImageMetricv4HelperThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;

  typedef CorrelationImageToImageMetricv4HelperThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >
    CorrelationHelperDenseThreaderType;
  typedef CorrelationImageToImageMetricv4HelperThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    CorrelationHelperSparseThreaderType;

  typename CorrelationHelperDenseThreaderType::Pointer  m_HelperDenseThreader;
  typename CorrelationHelperSparseThreaderType::Pointer m_HelperSparseThreader;

  /* These values are computed during InitializeForIteration(),
   * using the helper class
   * */
  mutable MeasureType m_AverageFix;
  mutable MeasureType m_AverageMov;

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CorrelationImageToImageMetricv4);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrelationImageToImageMetricv4.hxx"
#endif

#endif
