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
#ifndef itkNormalizedCorrelationPointSetToImageMetric_h
#define itkNormalizedCorrelationPointSetToImageMetric_h

#include "itkPointSetToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"

namespace itk
{
/** \class NormalizedCorrelationPointSetToImageMetric
 * \brief Computes similarity between pixel values of a point set and
 * intensity values of an image.
 *
 * This metric computes the correlation between point values in the fixed
 * point-set and pixel values in the moving image. The correlation is
 * normalized by the autocorrelation values of both the point-set and the
 * moving image. The spatial correspondence between the point-set and the image
 * is established through a Transform. Pixel values are taken from the fixed
 * point-set. Their positions are mapped to the moving image and result in
 * general in non-grid position on it.  Values at these non-grid position of
 * the moving image are interpolated using a user-selected Interpolator.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedPointSet, typename TMovingImage>
class ITK_TEMPLATE_EXPORT NormalizedCorrelationPointSetToImageMetric
  : public PointSetToImageMetric<TFixedPointSet, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(NormalizedCorrelationPointSetToImageMetric);

  /** Standard class type aliases. */
  using Self = NormalizedCorrelationPointSetToImageMetric;
  using Superclass = PointSetToImageMetric<TFixedPointSet, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationPointSetToImageMetric, PointSetToImageMetric);

  /** Types transferred from the base class */
  using RealType = typename Superclass::RealType;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using GradientPixelType = typename Superclass::GradientPixelType;

  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedPointSetType = typename Superclass::FixedPointSetType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedPointSetConstPointer = typename Superclass::FixedPointSetConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;

  using PointIterator = typename Superclass::PointIterator;
  using PointDataIterator = typename Superclass::PointDataIterator;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

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
   * typically results in narrower valleys in the cost function.
   * Default value is false. */
  itkSetMacro(SubtractMean, bool);
  itkGetConstReferenceMacro(SubtractMean, bool);
  itkBooleanMacro(SubtractMean);

protected:
  NormalizedCorrelationPointSetToImageMetric();
  ~NormalizedCorrelationPointSetToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool m_SubtractMean;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizedCorrelationPointSetToImageMetric.hxx"
#endif

#endif
