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
#ifndef itkMeanReciprocalSquareDifferencePointSetToImageMetric_h
#define itkMeanReciprocalSquareDifferencePointSetToImageMetric_h

#include "itkPointSetToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"

namespace itk
{
/** \class MeanReciprocalSquareDifferencePointSetToImageMetric
 * \brief Computes similarity between pixel values of a point set and
 * intensity values in an image.
 *
 * This metric computes the average squared difference between pixels in
 * the point set and transformed point set pixels in the moving image
 * after passing the difference through a function of type
 * \f$ \frac{1}{1+ \frac{ difference^2 }{ \lambda^2 } }\f$.
 * \f$\lambda\f$ controls the capture radius of the metric. The term capture
 * radius used here is in terms of intensity domain and not in the spatial
 * domain.
 *
 * Spatial correspondence between both images is established through a
 * Transform.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedPointSet, typename TMovingImage>
class ITK_TEMPLATE_EXPORT MeanReciprocalSquareDifferencePointSetToImageMetric
  : public PointSetToImageMetric<TFixedPointSet, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeanReciprocalSquareDifferencePointSetToImageMetric);

  /** Standard class type aliases. */
  using Self = MeanReciprocalSquareDifferencePointSetToImageMetric;
  using Superclass = PointSetToImageMetric<TFixedPointSet, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanReciprocalSquareDifferencePointSetToImageMetric, Object);

  /** Types transferred from the base class */
  using RealType = typename Superclass::RealType;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using GradientPixelType = typename Superclass::GradientPixelType;

  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedPointSetType = typename Superclass::FixedPointSetType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedPointSetConstPointer = typename Superclass::FixedPointSetConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;

  using PointIterator = typename Superclass::PointIterator;
  using PointDataIterator = typename Superclass::PointDataIterator;

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

  /**  Set/Get the lambda distance. (controls the capture radius of the metric).
   */
  itkSetMacro(Lambda, double);
  itkGetConstMacro(Lambda, double);

protected:
  MeanReciprocalSquareDifferencePointSetToImageMetric();
  ~MeanReciprocalSquareDifferencePointSetToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_Lambda;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.hxx"
#endif

#endif
