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
#ifndef itkNormalizedCorrelationImageToImageMetric_h
#define itkNormalizedCorrelationImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkPoint.h"

namespace itk
{
/** \class NormalizedCorrelationImageToImageMetric
 * \brief Computes similarity between two images to be registered
 *
 * This metric computes the correlation between pixels in the fixed image
 * and pixels in the moving image. The spatial correspondence between
 * fixed and moving image is established through a Transform. Pixel values are
 * taken from the fixed image, their positions are mapped to the moving
 * image and result in general in non-grid position on it. Values at these
 * non-grid position of the moving image are interpolated using a user-selected
 * Interpolator. The correlation is normalized by the autocorrelations of both
 * the fixed and moving images.
 *
 * A more negative metric value indicates a greater degree of correlation
 * between the fixed and moving image. This makes the metric simpler to use
 * with optimizers that strive to minimize their cost function by default.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT NormalizedCorrelationImageToImageMetric : public ImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NormalizedCorrelationImageToImageMetric);

  /** Standard class type aliases. */
  using Self = NormalizedCorrelationImageToImageMetric;
  using Superclass = ImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  using typename Superclass::RealType;
  using typename Superclass::TransformType;
  using typename Superclass::TransformPointer;
  using typename Superclass::TransformParametersType;
  using typename Superclass::TransformJacobianType;
  using typename Superclass::GradientPixelType;
  using typename Superclass::OutputPointType;
  using typename Superclass::InputPointType;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::FixedImageType;
  using typename Superclass::MovingImageType;
  using typename Superclass::FixedImageConstPointer;
  using typename Superclass::MovingImageConstPointer;

  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const TransformParametersType & parameters, DerivativeType & derivative) const override;

  /**  Get the value for single valued optimizers. */
  MeasureType
  GetValue(const TransformParametersType & parameters) const override;

  /**  Get value and derivatives for multiple valued optimizers. */
  void
  GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType &                   value,
                        DerivativeType &                derivative) const override;

  /** Set/Get SubtractMean boolean. If true, the sample mean is subtracted
   * from the sample values in the cross-correlation formula and
   * typically results in narrower valleys in the cost function.
   * Default value is false. */
  itkSetMacro(SubtractMean, bool);
  itkGetConstReferenceMacro(SubtractMean, bool);
  itkBooleanMacro(SubtractMean);

protected:
  NormalizedCorrelationImageToImageMetric();
  ~NormalizedCorrelationImageToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool m_SubtractMean;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizedCorrelationImageToImageMetric.hxx"
#endif

#endif
