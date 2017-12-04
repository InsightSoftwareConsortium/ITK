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
#ifndef itkGradientDifferenceImageToImageMetric_h
#define itkGradientDifferenceImageToImageMetric_h

#include "itkImageToImageMetric.h"

#include "itkSobelOperator.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkPoint.h"
#include "itkCastImageFilter.h"
#include "itkResampleImageFilter.h"

namespace itk
{
/** \class GradientDifferenceImageToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the Images to be compared and
 * over the type of transformation and Iterpolator to be used.
 *
 * This metric computes the sum of squared differences between pixels in
 * the derivatives of the moving and fixed images after passing the squared
 * difference through a function of type \f$ \frac{1}{1+x} \f$.
 *
 * \warning THIS IMAGE METRIC IS CURRENTLY UNDER DEBUGGING. USE AT YOUR OWN RISK.
 *
 * Spatial correspondance between both images is established through a
 * Transform. Pixel values are taken from the Moving image. Their positions
 * are mapped to the Fixed image and result in general in non-grid position
 * on it. Values at these non-grid position of the Fixed image are
 * interpolated using a user-selected Interpolator.
 *
 * Implementation of this class is based on:
 * Hipwell, J. H., et. al. (2003), "Intensity-Based 2-D-3D Registration of
 * Cerebral Angiograms,", IEEE Transactions on Medical Imaging,
 * 22(11):1417-1426.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT GradientDifferenceImageToImageMetric:
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef GradientDifferenceImageToImageMetric            Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDifferenceImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                RealType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;

  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::FixedImageType          FixedImageType;
  typedef typename Superclass::MovingImageType         MovingImageType;
  typedef typename Superclass::FixedImageConstPointer  FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer MovingImageConstPointer;

  typedef typename TFixedImage::PixelType  FixedImagePixelType;
  typedef typename TMovingImage::PixelType MovedImagePixelType;

  itkStaticConstMacro(FixedImageDimension, unsigned int, TFixedImage::ImageDimension);
  /** Types for transforming the moving image */
  typedef itk::Image< FixedImagePixelType,
                      itkGetStaticConstMacro(FixedImageDimension) >
  TransformedMovingImageType;

  typedef itk::ResampleImageFilter< MovingImageType, TransformedMovingImageType >
  TransformMovingImageFilterType;

  /** Sobel filters to compute the gradients of the Fixed Image */

  typedef itk::Image< RealType, itkGetStaticConstMacro(FixedImageDimension) > FixedGradientImageType;

  typedef itk::CastImageFilter< FixedImageType, FixedGradientImageType > CastFixedImageFilterType;
  typedef typename CastFixedImageFilterType::Pointer                     CastFixedImageFilterPointer;

  typedef typename FixedGradientImageType::PixelType FixedGradientPixelType;

  /** Sobel filters to compute the gradients of the Moved Image */

  itkStaticConstMacro(MovedImageDimension, unsigned int, MovingImageType::ImageDimension);

  typedef itk::Image< RealType, itkGetStaticConstMacro(MovedImageDimension) > MovedGradientImageType;

  typedef itk::CastImageFilter< TransformedMovingImageType, MovedGradientImageType > CastMovedImageFilterType;
  typedef typename CastMovedImageFilterType::Pointer                                 CastMovedImageFilterPointer;

  typedef typename MovedGradientImageType::PixelType MovedGradientPixelType;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType & parameters,
                     DerivativeType  & derivative) const ITK_OVERRIDE;

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue(const TransformParametersType & parameters) const ITK_OVERRIDE;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative(const TransformParametersType & parameters,
                             MeasureType & Value, DerivativeType & derivative) const ITK_OVERRIDE;

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void Initialize(void) ITK_OVERRIDE;

  /** Write gradient images to a files for debugging purposes. */
  void WriteGradientImagesToFiles() const;

  /** Set/Get the value of Delta used for computing derivatives by finite
   * differences in the GetDerivative() method */
  itkSetMacro(DerivativeDelta, double);
  itkGetConstReferenceMacro(DerivativeDelta, double);

protected:
  GradientDifferenceImageToImageMetric();
  virtual ~GradientDifferenceImageToImageMetric() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Compute the range of the moved image gradients. */
  void ComputeMovedGradientRange() const;

  /** Compute the variance and range of the moving image gradients. */
  void ComputeVariance() const;

  /** Compute the similarity measure using a specified subtraction factor. */
  MeasureType ComputeMeasure(const TransformParametersType & parameters,
                             const double *subtractionFactor) const;

  typedef NeighborhoodOperatorImageFilter<
    FixedGradientImageType, FixedGradientImageType > FixedSobelFilter;

  typedef NeighborhoodOperatorImageFilter<
    MovedGradientImageType, MovedGradientImageType > MovedSobelFilter;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDifferenceImageToImageMetric);

  /** The variance of the moving image gradients. */
  mutable MovedGradientPixelType m_Variance[FixedImageDimension];

  /** The range of the moving image gradients. */
  mutable MovedGradientPixelType m_MinMovedGradient[MovedImageDimension];
  mutable MovedGradientPixelType m_MaxMovedGradient[MovedImageDimension];
  /** The range of the fixed image gradients. */
  mutable FixedGradientPixelType m_MinFixedGradient[FixedImageDimension];
  mutable FixedGradientPixelType m_MaxFixedGradient[FixedImageDimension];

  /** The filter for transforming the moving image. */
  typename TransformMovingImageFilterType::Pointer m_TransformMovingImageFilter;

  /** The Sobel gradients of the fixed image */
  CastFixedImageFilterPointer m_CastFixedImageFilter;

  SobelOperator< FixedGradientPixelType,
                 itkGetStaticConstMacro(FixedImageDimension) >
  m_FixedSobelOperators[FixedImageDimension];

  typename FixedSobelFilter::Pointer m_FixedSobelFilters[itkGetStaticConstMacro(FixedImageDimension)];

  ZeroFluxNeumannBoundaryCondition< MovedGradientImageType > m_MovedBoundCond;
  ZeroFluxNeumannBoundaryCondition< FixedGradientImageType > m_FixedBoundCond;

  /** The Sobel gradients of the moving image */
  CastMovedImageFilterPointer m_CastMovedImageFilter;

  SobelOperator< MovedGradientPixelType,
                 itkGetStaticConstMacro(MovedImageDimension) >
  m_MovedSobelOperators[MovedImageDimension];

  typename MovedSobelFilter::Pointer m_MovedSobelFilters[itkGetStaticConstMacro(MovedImageDimension)];

  double m_DerivativeDelta;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientDifferenceImageToImageMetric.hxx"
#endif

#endif
