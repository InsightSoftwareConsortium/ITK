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
#ifndef itkMeanReciprocalSquareDifferenceImageToImageMetric_h
#define itkMeanReciprocalSquareDifferenceImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkPoint.h"

namespace itk
{
/** \class MeanReciprocalSquareDifferenceImageToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the Images to be compared and
 * over the type of transformation and Iterpolator to be used.
 *
 * This metric computes the sum of squared differences between pixels in
 * the moving image and pixels in the fixed image after passing the squared
 * difference through a function of type \f$ \frac{1}{1+x} \f$.

 * Spatial correspondence between both images is established through a
 * Transform. Pixel values are taken from the Moving image. Their positions
 * are mapped to the Fixed image and result in general in non-grid position
 * on it. Values at these non-grid position of the Fixed image are interpolated
 * using a user-selected Interpolator.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT MeanReciprocalSquareDifferenceImageToImageMetric
  : public ImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeanReciprocalSquareDifferenceImageToImageMetric);

  /** Standard class type aliases. */
  using Self = MeanReciprocalSquareDifferenceImageToImageMetric;
  using Superclass = ImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanReciprocalSquareDifferenceImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedImageConstPointer = typename Superclass::FixedImageConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;

  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const TransformParametersType & parameters, DerivativeType & derivative) const override;

  /**  Get the value for single valued optimizers. */
  MeasureType
  GetValue(const TransformParametersType & parameters) const override;

  /**  Get value and derivatives for multiple valued optimizers. */
  void
  GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType &                   Value,
                        DerivativeType &                derivative) const override;

  /** Set/Get Lambda value. This factor regulates the capture radius of
      this metric */
  itkGetConstMacro(Lambda, double);
  itkSetMacro(Lambda, double);

  /** Set/Get Delta value. This value is used as the differential in the
   * computation of the metric derivative using the finite differences method. */
  itkGetConstMacro(Delta, double);
  itkSetMacro(Delta, double);

protected:
  MeanReciprocalSquareDifferenceImageToImageMetric();
  ~MeanReciprocalSquareDifferenceImageToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_Lambda;
  double m_Delta;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeanReciprocalSquareDifferenceImageToImageMetric.hxx"
#endif

#endif
