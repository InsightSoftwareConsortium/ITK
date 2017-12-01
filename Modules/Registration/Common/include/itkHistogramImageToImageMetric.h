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
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT HistogramImageToImageMetric:
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:
  /** Standard class typedefs. */
  typedef HistogramImageToImageMetric                     Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                   RealType;
  typedef typename Superclass::TransformType              TransformType;
  typedef typename Superclass::TransformPointer           TransformPointer;
  typedef typename Superclass::TransformParametersType    TransformParametersType;
  typedef typename Superclass::TransformJacobianType      TransformJacobianType;
  typedef typename Superclass::GradientPixelType          GradientPixelType;
  typedef typename Superclass::InputPointType             InputPointType;
  typedef typename Superclass::OutputPointType            OutputPointType;
  typedef typename Superclass::MeasureType                MeasureType;
  typedef typename Superclass::DerivativeType             DerivativeType;
  typedef typename Superclass::FixedImageType             FixedImageType;
  typedef typename Superclass::FixedImageType::PixelType  FixedImagePixelType;
  typedef typename Superclass::MovingImageType            MovingImageType;
  typedef typename Superclass::MovingImageType::PixelType MovingImagePixelType;
  typedef typename Superclass::FixedImageConstPointer
  FixedImageConstPointerType;
  typedef typename Superclass::MovingImageConstPointer
  MovingImageConstPointerType;

  /** Typedefs for histogram. This should have been defined as
      Histogram<RealType,2> but a bug in VC++7 produced an internal compiler
      error with such declaration. */
  typedef Statistics::Histogram< double > HistogramType;

  typedef typename HistogramType::MeasurementVectorType MeasurementVectorType;
  typedef typename HistogramType::SizeType              HistogramSizeType;
  typedef typename HistogramType::Pointer               HistogramPointer;

  /** Initializes the metric. */
  void Initialize() ITK_OVERRIDE;

  /** Define the transform and thereby the parameter space of the metric
   *   and the space of its derivatives */
  void SetTransform(TransformType *transform) ITK_OVERRIDE;

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
  typedef Array< double > ScalesType;

  /** Sets the derivative step length scales. */
  itkSetMacro(DerivativeStepLengthScales, ScalesType);

  /** Returns the derivate step length scales. */
  itkGetConstReferenceMacro(DerivativeStepLengthScales, ScalesType);

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue(const TransformParametersType & parameters) const ITK_OVERRIDE;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType & parameters,
                     DerivativeType & derivative) const ITK_OVERRIDE;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative(const TransformParametersType & parameters,
                             MeasureType & Value,
                             DerivativeType & Derivative) const ITK_OVERRIDE;

  /** Set the lower bounds of the intensities to be considered for computing
    * the histogram. This option allows to focus the computation of the Metric in
    * a particular range of intensities that correspond to features of interest. */
  void SetLowerBound(const MeasurementVectorType & bound);

  /** Returns the current state of m_LowerBound. */
  const MeasurementVectorType & GetLowerBound() const;

  /** Set the upper bounds of the intensities to be considered for computing
    * the histogram. This option allows to focus the computation of the Metric in
    * a particular range of intensities that correspond to features of interest.  */
  void SetUpperBound(const MeasurementVectorType & bound);

  /** Returns the current state of m_UpperBound. */
  const MeasurementVectorType & GetUpperBound() const;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  HistogramImageToImageMetric();
  virtual ~HistogramImageToImageMetric() ITK_OVERRIDE {}

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
  void ComputeHistogram(const TransformParametersType & parameters,
                        HistogramType & histogram) const;

  /** Computes the joint histogram from the transformation parameters
      passed to the function. */
  void ComputeHistogram(const TransformParametersType & parameters,
                        unsigned int parameter,
                        double step,
                        HistogramType & histogram) const;

  /** Copies a histogram. */
  void CopyHistogram(HistogramType & target, HistogramType & source) const;

  /** Evaluates the similarity measure using the given histogram. All
      subclasses must reimplement this method. */
  virtual MeasureType EvaluateMeasure(HistogramType & histogram) const = 0;

  /** PrintSelf function */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramImageToImageMetric);

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
#include "itkHistogramImageToImageMetric.hxx"
#endif

#endif // itkHistogramImageToImageMetric_h
