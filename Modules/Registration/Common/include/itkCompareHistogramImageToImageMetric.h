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
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT CompareHistogramImageToImageMetric:
  public HistogramImageToImageMetric< TFixedImage, TMovingImage >
{
public:
  /** Standard class typedefs. */
  typedef CompareHistogramImageToImageMetric                       Self;
  typedef HistogramImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                                     Pointer;
  typedef SmartPointer< const Self >                               ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CompareHistogramImageToImageMetric,
               HistogramImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType         RealType;
  typedef typename Superclass::TransformType    TransformType;
  typedef typename Superclass::TransformPointer TransformPointer;
  typedef typename TransformType::ConstPointer  TransformConstPointer;

  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;
  typedef typename Superclass::GradientPixelType       GradientPixelType;

  typedef typename Superclass::MeasureType            MeasureType;
  typedef typename Superclass::DerivativeType         DerivativeType;
  typedef typename Superclass::FixedImageType         FixedImageType;
  typedef typename Superclass::MovingImageType        MovingImageType;
  typedef typename Superclass::FixedImageConstPointer FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer
  MovingImageConstPointer;

  typedef typename Superclass::HistogramType            HistogramType;
  typedef typename Superclass::HistogramSizeType        HistogramSizeType;
  typedef typename HistogramType::MeasurementVectorType HistogramMeasurementVectorType;
  typedef typename HistogramType::AbsoluteFrequencyType HistogramAbsoluteFrequencyType;
  typedef HistogramAbsoluteFrequencyType                HistogramFrequencyType;

  typedef typename HistogramType::Iterator HistogramIteratorType;
  typedef typename HistogramType::Pointer  HistogramPointerType;

  typedef typename Superclass::InterpolatorType    InterpolatorType;
  typedef typename Superclass::InterpolatorPointer InterpolatorPointer;

  typedef typename Superclass::FixedImageRegionType
  FixedImageRegionType;

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
  unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
  { return this->GetTransform()->GetNumberOfParameters(); }

  /** Forms the histogram of the training images to prepare to evaluate the
   * metric. Must set all parameters first. */
  void Initialize() ITK_OVERRIDE;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  CompareHistogramImageToImageMetric();
  virtual ~CompareHistogramImageToImageMetric() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Form the Histogram for the Training data */
  void FormTrainingHistogram();

  /** Evaluates the comparison histogram metric. All sub-classes must
      re-implement method. */
  virtual MeasureType EvaluateMeasure(HistogramType & histogram) const ITK_OVERRIDE = 0;

private:
  // Purposely not implemented.
  CompareHistogramImageToImageMetric(Self const &);
  void operator=(Self const &); // Purposely not implemented.

  FixedImageConstPointer  m_TrainingFixedImage;
  MovingImageConstPointer m_TrainingMovingImage;
  TransformPointer        m_TrainingTransform;
  InterpolatorPointer     m_TrainingInterpolator;
  FixedImageRegionType    m_TrainingFixedImageRegion;
  HistogramPointerType    m_TrainingHistogram;
};
} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompareHistogramImageToImageMetric.hxx"
#endif

#endif // itkCompareHistogramImageToImageMetric_h
