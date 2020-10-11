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
#ifndef itkKappaStatisticImageToImageMetric_h
#define itkKappaStatisticImageToImageMetric_h

#include "itkImageToImageMetric.h"

namespace itk
{
/** \class KappaStatisticImageToImageMetric
 * \brief Computes similarity between two binary objects to be
 * registered
 *
 * This Class is templated over the type of the fixed and moving
 * images to be compared.  The metric here is designed for matching
 * pixels in two images with the same exact value.  Only one value can
 * be considered (the default is 255) and can be specified with the
 * SetForegroundValue method.  In the computation of the metric, only
 * foreground pixels are considered.  The metric value is given
 * by 2*|A&B|/(|A|+|B|), where A is the foreground region in the moving
 * image, B is the foreground region in the fixed image, & is intersection,
 * and |.| indicates the area of the enclosed set.  The metric is
 * described in "Morphometric Analysis of White Matter Lesions in MR
 * Images: Method and Validation", A. P. Zijdenbos, B. M. Dawant, R. A.
 * Margolin, A. C. Palmer.
 *
 * This metric is especially useful when considering the similarity between
 * binary images.  Given the nature of binary images, a nearest neighbor
 * interpolator is the preferred interpolator.
 *
 * Metric values range from 0.0 (no foreground alignment) to 1.0
 * (perfect foreground alignment).  When dealing with optimizers that can
 * only minimize a metric, use the ComplementOn() method.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT KappaStatisticImageToImageMetric : public ImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(KappaStatisticImageToImageMetric);

  /** Standard class type aliases. */
  using Self = KappaStatisticImageToImageMetric;
  using Superclass = ImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KappaStatisticImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  using RealType = typename Superclass::RealType;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using GradientImageType = typename Superclass::GradientImageType;
  using GradientPixelType = typename Superclass::GradientPixelType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedImageConstPointer = typename Superclass::FixedImageConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;
  using FixedImageRegionType = typename Superclass::FixedImageRegionType;

  /** Computes the gradient image and assigns it to m_GradientImage */
  void
  ComputeGradient() override;

  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const TransformParametersType &, DerivativeType & derivative) const override;

  /** Get the value of the metric at a particular parameter
   *  setting. The metric value is given by 2*|A&B|/(|A|+|B|), where A
   *  is the moving image, B is the fixed image, & is intersection,
   *  and |.| indicates the area of the enclosed set. If ComplementOn has
   *  been set, the metric value is 1.0-2*|A&B|/(|A|+|B|). */
  MeasureType
  GetValue(const TransformParametersType & parameters) const override;

  /** Get both the value and derivative. This method internally calls the
    \c GetValue() and the \c GetDerivative() method. */
  void
  GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType &                   Value,
                        DerivativeType &                Derivative) const override;

  /** This method allows the user to set the foreground value. The default
   *  value is 255. */
  itkSetMacro(ForegroundValue, RealType);
  itkGetConstMacro(ForegroundValue, RealType);

  /** Set/Get whether this metric returns 2*|A&B|/(|A|+|B|)
   * (ComplementOff, the default) or 1.0 - 2*|A&B|/(|A|+|B|)
   * (ComplementOn). When using an optimizer that minimizes
   * metric values use ComplementOn(). */
  itkSetMacro(Complement, bool);
  itkBooleanMacro(Complement);
  itkGetConstMacro(Complement, bool);

protected:
  KappaStatisticImageToImageMetric();
  ~KappaStatisticImageToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  RealType m_ForegroundValue;
  bool     m_Complement{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKappaStatisticImageToImageMetric.hxx"
#endif

#endif
