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
 * and pixels in the moving image. The spatial correspondance between
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
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT NormalizedCorrelationImageToImageMetric:
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef NormalizedCorrelationImageToImageMetric         Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationImageToImageMetric, Object);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                RealType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;
  typedef typename Superclass::GradientPixelType       GradientPixelType;
  typedef typename Superclass::OutputPointType         OutputPointType;
  typedef typename Superclass::InputPointType          InputPointType;

  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::FixedImageType          FixedImageType;
  typedef typename Superclass::MovingImageType         MovingImageType;
  typedef typename Superclass::FixedImageConstPointer  FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer MovingImageConstPointer;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType & parameters,
                     DerivativeType & Derivative) const override;

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue(const TransformParametersType & parameters) const override;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative(const TransformParametersType & parameters,
                             MeasureType & Value, DerivativeType & Derivative) const override;

  /** Set/Get SubtractMean boolean. If true, the sample mean is subtracted
   * from the sample values in the cross-correlation formula and
   * typically results in narrower valleys in the cost function.
   * Default value is false. */
  itkSetMacro(SubtractMean, bool);
  itkGetConstReferenceMacro(SubtractMean, bool);
  itkBooleanMacro(SubtractMean);

protected:
  NormalizedCorrelationImageToImageMetric();
  ~NormalizedCorrelationImageToImageMetric() override {}
  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NormalizedCorrelationImageToImageMetric);

  bool m_SubtractMean;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationImageToImageMetric.hxx"
#endif

#endif
