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
#ifndef itkCompareHistogramImageToImageMetric_h
#define itkCompareHistogramImageToImageMetric_h

#include "itkHistogramImageToImageMetric.h"

namespace itk
{
/** \class CompareHistogramImageToImageMetric
 *  \brief Compares Histograms between two images to be registered to
 *   a Training Histogram.
 *
 *  This class is templated over the type of the fixed and moving
 *  images to be compared.
 *
 *  This metric computes the similarity between the histogram produced
 *  by two images overlapping and a training histogram.
 *
 *  It is to be sub-classed by the method of comparing the
 *  histograms.
 *
 *  Generally, the histogram from the training data is to be
 *  computed in exactly the same way as the histogram from the
 *  images to be compared are computed. Thus, the user can set the
 *  interpolator, region, two training images and the transform and
 *  the training histogram will be formed. OR, the user can simply
 *  calculate the training histogram separately and set it.
 *
 * \warning The Initialize function does nothing if the training
 * histogram already exists. Thus repeated calls to the Initialize
 * function do nothing after the first call. If you wish the
 * training histogram to be re-calculated, you should set it to 0.
 *
 *  \author Samson Timoner.
 *
 *  \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT CompareHistogramImageToImageMetric
  : public HistogramImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CompareHistogramImageToImageMetric);

  /** Standard class type aliases. */
  using Self = CompareHistogramImageToImageMetric;
  using Superclass = HistogramImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CompareHistogramImageToImageMetric, HistogramImageToImageMetric);

  /** Types transferred from the base class */
  using RealType = typename Superclass::RealType;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformConstPointer = typename TransformType::ConstPointer;

  using TransformParametersType = typename Superclass::TransformParametersType;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using GradientPixelType = typename Superclass::GradientPixelType;

  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedImageConstPointer = typename Superclass::FixedImageConstPointer;
  using MovingImageConstPointer = typename Superclass::MovingImageConstPointer;

  using HistogramType = typename Superclass::HistogramType;
  using HistogramSizeType = typename Superclass::HistogramSizeType;
  using HistogramMeasurementVectorType = typename HistogramType::MeasurementVectorType;
  using HistogramAbsoluteFrequencyType = typename HistogramType::AbsoluteFrequencyType;
  using HistogramFrequencyType = HistogramAbsoluteFrequencyType;

  using HistogramIteratorType = typename HistogramType::Iterator;
  using HistogramPointerType = typename HistogramType::Pointer;

  using InterpolatorType = typename Superclass::InterpolatorType;
  using InterpolatorPointer = typename Superclass::InterpolatorPointer;

  using FixedImageRegionType = typename Superclass::FixedImageRegionType;

  /** Get/Set the histogram to be used in the metric calculation */
  itkSetMacro(TrainingHistogram, HistogramPointerType);
  itkGetConstReferenceMacro(TrainingHistogram, HistogramPointerType);

  /** Get/Set the Training Fixed Image.  */
  itkSetConstObjectMacro(TrainingFixedImage, FixedImageType);

  /** Get/Set the Training Moving Image.  */
  itkSetConstObjectMacro(TrainingMovingImage, MovingImageType);
  itkGetConstObjectMacro(TrainingMovingImage, MovingImageType);

  /** Get/Set the Training Transform. */
  itkSetObjectMacro(TrainingTransform, TransformType);
  itkGetModifiableObjectMacro(TrainingTransform, TransformType);

  /** Get/Set the Interpolator. */
  itkSetObjectMacro(TrainingInterpolator, InterpolatorType);
  itkGetModifiableObjectMacro(TrainingInterpolator, InterpolatorType);

  /** Get/Set the region over which the training histogram will be computed */
  itkSetMacro(TrainingFixedImageRegion, FixedImageRegionType);
  itkGetConstReferenceMacro(TrainingFixedImageRegion, FixedImageRegionType);

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override
  {
    return this->GetTransform()->GetNumberOfParameters();
  }

  /** Forms the histogram of the training images to prepare to evaluate the
   * metric. Must set all parameters first. */
  void
  Initialize() override;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  CompareHistogramImageToImageMetric();
  ~CompareHistogramImageToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Form the Histogram for the Training data */
  void
  FormTrainingHistogram();

  /** Evaluates the comparison histogram metric. All sub-classes must
      re-implement method. */
  MeasureType
  EvaluateMeasure(HistogramType & histogram) const override = 0;

private:
  FixedImageConstPointer  m_TrainingFixedImage;
  MovingImageConstPointer m_TrainingMovingImage;
  TransformPointer        m_TrainingTransform;
  InterpolatorPointer     m_TrainingInterpolator;
  FixedImageRegionType    m_TrainingFixedImageRegion;
  HistogramPointerType    m_TrainingHistogram;
};
} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCompareHistogramImageToImageMetric.hxx"
#endif

#endif // itkCompareHistogramImageToImageMetric_h
