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
#ifndef itkNormalizedCorrelationTwoImageToOneImageMetric_h
#define itkNormalizedCorrelationTwoImageToOneImageMetric_h

#include "itkTwoImageToOneImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
/** \class NormalizedCorrelationTwoImageToOneImageMetric
 * \brief Computes similarity between two fixed images and one moving image
 *
 * This metric computes the correlation between pixels in the two fixed images
 * and pixels in the moving image. The spatial correspondance between
 * two fixed images and the moving image is established through a Transform. Pixel values are
 * taken from the fixed images, their positions are mapped to the moving
 * image and result in general in non-grid position on it. Values at these
 * non-grid position of the moving image are interpolated using user-selected
 * Interpolators. The correlation is normalized by the autocorrelations of both
 * the fixed and moving images.
 *
 * \ingroup RegistrationMetrics
 * \ingroup TwoProjectionRegistration
 */
template <typename TFixedImage, typename TMovingImage>
class NormalizedCorrelationTwoImageToOneImageMetric : public TwoImageToOneImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NormalizedCorrelationTwoImageToOneImageMetric);

  /** Standard class type alias. */
  using Self = NormalizedCorrelationTwoImageToOneImageMetric;
  using Superclass = TwoImageToOneImageMetric<TFixedImage, TMovingImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationTwoImageToOneImageMetric, Object);


  /** Types transferred from the base class */
  using RealType = typename Superclass::RealType;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using GradientPixelType = typename Superclass::GradientPixelType;

  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedImageConstPointer = typename Superclass::FixedImageConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;


  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const TransformParametersType & parameters, DerivativeType & Derivative) const override;

  /**  Get the value for single valued optimizers. */
  MeasureType
  GetValue(const TransformParametersType & parameters) const override;

  /**  Get value and derivatives for multiple valued optimizers. */
  void
  GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType &                   Value,
                        DerivativeType &                Derivative) const override;

  /** Set/Get SubtractMean boolean. If true, the sample mean is subtracted
   * from the sample values in the cross-correlation formula and
   * typically results in narrower valleys in the cost fucntion.
   * Default value is false. */
  itkSetMacro(SubtractMean, bool);
  itkGetConstReferenceMacro(SubtractMean, bool);
  itkBooleanMacro(SubtractMean);

protected:
  NormalizedCorrelationTwoImageToOneImageMetric();
  ~NormalizedCorrelationTwoImageToOneImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool m_SubtractMean;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizedCorrelationTwoImageToOneImageMetric.hxx"
#endif

#endif
