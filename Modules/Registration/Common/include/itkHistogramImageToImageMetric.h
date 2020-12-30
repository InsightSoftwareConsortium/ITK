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
#ifndef itkHistogramImageToImageMetric_h
#define itkHistogramImageToImageMetric_h

#include "itkHistogram.h"
#include "itkImageToImageMetric.h"

namespace itk
{
/** \class HistogramImageToImageMetric
    \brief Computes similarity between two objects to be registered

  This class is templated over the type of the fixed and moving
  images to be compared.

  The metric computes the similarity measure between pixels in the
  moving image and pixels in the fixed image using a histogram.

  \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT HistogramImageToImageMetric : public ImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HistogramImageToImageMetric);

  /** Standard class type aliases. */
  using Self = HistogramImageToImageMetric;
  using Superclass = ImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  using RealType = typename Superclass::RealType;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using GradientPixelType = typename Superclass::GradientPixelType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePixelType = typename Superclass::FixedImageType::PixelType;
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePixelType = typename Superclass::MovingImageType::PixelType;
  using FixedImageConstPointerType = typename Superclass::FixedImageConstPointer;
  using MovingImageConstPointerType = typename Superclass::MovingImageConstPointer;

  /** Typedefs for histogram. This should have been defined as
      Histogram<RealType,2> but a bug in VC++7 produced an internal compiler
      error with such declaration. */
  using HistogramType = Statistics::Histogram<double>;

  using MeasurementVectorType = typename HistogramType::MeasurementVectorType;
  using HistogramSizeType = typename HistogramType::SizeType;
  using HistogramPointer = typename HistogramType::Pointer;

  /** Initializes the metric. */
  void
  Initialize() override;

  /** Define the transform and thereby the parameter space of the metric
   *   and the space of its derivatives */
  void
  SetTransform(TransformType * transform) override;

  /** Sets the histogram size. Note this function must be called before
      \c Initialize(). */
  itkSetMacro(HistogramSize, HistogramSizeType);

  /** Gets the histogram size. */
  itkGetConstReferenceMacro(HistogramSize, HistogramSizeType);

  /** Factor to increase the upper bound for the samples in the histogram.
      Default value is 0.001 */
  itkSetMacro(UpperBoundIncreaseFactor, double);
  itkGetConstMacro(UpperBoundIncreaseFactor, double);

  /** The padding value. */
  itkSetMacro(PaddingValue, FixedImagePixelType);

  /** Returns the padding value. */
  itkGetConstReferenceMacro(PaddingValue, FixedImagePixelType);

  /** Return the joint histogram. This is updated during every call to the
   *  GetValue() method. The histogram can for instance be used by
   *  itk::HistogramToImageFilter to plot the joint histogram. */
  itkGetConstReferenceMacro(Histogram, HistogramPointer);

  /** Set whether the padding value should be used to determine which pixels
      should be ignored when calculating the similarity measure. Those pixels
      in the fixed image which have the padding value will be ignored. */
  itkSetMacro(UsePaddingValue, bool);
  itkGetConstMacro(UsePaddingValue, bool);

  /** Sets the step length used to calculate the derivative. */
  itkSetMacro(DerivativeStepLength, double);

  /** Returns the step length used to calculate the derivative. */
  itkGetConstMacro(DerivativeStepLength, double);

  /** The scales type. */
  using ScalesType = Array<double>;

  /** Sets the derivative step length scales. */
  itkSetMacro(DerivativeStepLengthScales, ScalesType);

  /** Returns the derivate step length scales. */
  itkGetConstReferenceMacro(DerivativeStepLengthScales, ScalesType);

  /**  Get the value for single valued optimizers. */
  MeasureType
  GetValue(const TransformParametersType & parameters) const override;

  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const TransformParametersType & parameters, DerivativeType & derivative) const override;

  /**  Get value and derivatives for multiple valued optimizers. */
  void
  GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType &                   value,
                        DerivativeType &                derivative) const override;

  /** Set the lower bounds of the intensities to be considered for computing
   * the histogram. This option allows to focus the computation of the Metric in
   * a particular range of intensities that correspond to features of interest. */
  void
  SetLowerBound(const MeasurementVectorType & bounds);

  /** Returns the current state of m_LowerBound. */
  const MeasurementVectorType &
  GetLowerBound() const;

  /** Set the upper bounds of the intensities to be considered for computing
   * the histogram. This option allows to focus the computation of the Metric in
   * a particular range of intensities that correspond to features of interest.  */
  void
  SetUpperBound(const MeasurementVectorType & bounds);

  /** Returns the current state of m_UpperBound. */
  const MeasurementVectorType &
  GetUpperBound() const;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  HistogramImageToImageMetric();
  ~HistogramImageToImageMetric() override = default;

  /** The histogram size. */
  HistogramSizeType m_HistogramSize;
  /** The lower bound for samples in the histogram. */
  mutable MeasurementVectorType m_LowerBound;
  /** The upper bound for samples in the histogram. */
  mutable MeasurementVectorType m_UpperBound;
  /** The increase in the upper bound. */
  double m_UpperBoundIncreaseFactor;

  /** Boolean flag to indicate whether the user supplied lower bounds or
   * whether they should be computed from the min of image intensities */
  bool m_LowerBoundSetByUser;

  /** Boolean flag to indicate whether the user supplied upper bounds or
   * whether they should be computed from the max of image intensities */
  bool m_UpperBoundSetByUser;

  /** Computes the joint histogram from the transformation parameters
      passed to the function. */
  void
  ComputeHistogram(const TransformParametersType & parameters, HistogramType & histogram) const;

  /** Computes the joint histogram from the transformation parameters
      passed to the function. */
  void
  ComputeHistogram(const TransformParametersType & parameters,
                   unsigned int                    parameter,
                   double                          step,
                   HistogramType &                 histogram) const;

  /** Copies a histogram. */
  void
  CopyHistogram(HistogramType & target, HistogramType & source) const;

  /** Evaluates the similarity measure using the given histogram. All
      subclasses must reimplement this method. */
  virtual MeasureType
  EvaluateMeasure(HistogramType & histogram) const = 0;

  /** PrintSelf function */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The padding value. */
  FixedImagePixelType m_PaddingValue;

  /** True if those pixels in the fixed image with the same value as the
      padding value should be ignored when calculating the similarity
      measure. */
  bool m_UsePaddingValue;

  /** The step length used to calculate the derivative. */
  double m_DerivativeStepLength;

  /** The derivative step length scales. */
  ScalesType m_DerivativeStepLengthScales;

  /** Pointer to the joint histogram. This is updated during every call to
   * GetValue() */
  HistogramPointer m_Histogram;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHistogramImageToImageMetric.hxx"
#endif

#endif // itkHistogramImageToImageMetric_h
