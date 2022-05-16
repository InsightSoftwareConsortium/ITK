/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkKullbackLeiblerCompareHistogramImageToImageMetric_h
#define itkKullbackLeiblerCompareHistogramImageToImageMetric_h

#include "itkCompareHistogramImageToImageMetric.h"

namespace itk
{
/** \class KullbackLeiblerCompareHistogramImageToImageMetric
 *  \brief Computes the Kubler Lieblach(KL) metric between the histogram
 *  of the two images to be registered and a training histogram.
 *
 *  This class is templated over the type of the fixed and moving
 *  images to be compared.
 *
 *  This class computers the KL-metric by comparing the histograms
 *  of the testing histogram formed by the overlap of intensities in
 *  the images, to a training histogram. It is based on the
 *  following paper:
 *
 *  Albert C.S. Chung, William M. Wells III, Alexander Norbash, and
 *  W. Eric L.  Grimson, Multi-modal Image Registration by
 *  Minimising Kullback-Leibler Distance, In Medical Image Computing
 *  and Computer-Assisted Intervention - MICCAI 2002, LNCS 2489,
 *  pp. 525 - 532.
 *
 *  The metric is given by KL(P_test||P_train)
 *  = Sum_{i1,i2} P_test(i1,i2) std::log(P_test(i1,i2)/P_train(i1,i2))
 *  where P_test and P_train are probabilities given my normalized
 *  histograms, and i1 and i2 are the intensity bins in the histogram.
 *
 *  \par PARAMETERS
 *  Epsilon is added to every bin in both histograms. This prevents
 *  division by zero problems. Epsilon should generally be set to a
 *  number smaller than one divided by the total number bins in
 *  the histogram. So, for a 256 by 256 histogram, Epsilon should be
 *  much less than 1e-5. Tests have shown that choices of epsilon are
 *  not very important as long as it is small enough. The default is 1e-12.
 *  I doubt you will need to change it.
 *
 *  \author Samson Timoner
 *
 *  \par SUPPORT
 *  This work was supported by the Functional Imaging Research in
 *  Schizophrenia Testbed (FIRST) Biomedical Informatics Research
 *  Network (BIRN, https://www.birncommunity.org/), which is funded by the
 *  National Center for Research Resources at the National
 *  Institutes of Health (NIH).  This work is also funded by the
 *  Neuroimage Analysis Center (P41 RR13218).
 *
 *  \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */

template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT KullbackLeiblerCompareHistogramImageToImageMetric
  : public CompareHistogramImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(KullbackLeiblerCompareHistogramImageToImageMetric);

  /** Standard class type aliases. */
  using Self = KullbackLeiblerCompareHistogramImageToImageMetric;
  using Superclass = CompareHistogramImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KullbackLeiblerCompareHistogramImageToImageMetric, HistogramImageToImageMetric);

  /** Types transferred from the base class */
  using typename Superclass::RealType;
  using typename Superclass::TransformType;
  using typename Superclass::TransformPointer;
  using TransformConstPointer = typename Superclass::ConstPointer;
  using typename Superclass::TransformParametersType;
  using typename Superclass::TransformJacobianType;
  using typename Superclass::GradientPixelType;

  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::FixedImageType;
  using typename Superclass::MovingImageType;
  using typename Superclass::FixedImageConstPointer;
  using typename Superclass::MovingImageConstPointer;

  using typename Superclass::HistogramType;
  using typename Superclass::HistogramSizeType;
  using HistogramMeasurementVectorType = typename Superclass::MeasurementVectorType;

  using typename Superclass::HistogramFrequencyType;
  using typename Superclass::HistogramIteratorType;
  using typename Superclass::HistogramPointerType;
  using typename Superclass::InterpolatorType;
  using typename Superclass::InterpolatorPointer;

  /** Set epsilon, which is added to each bin in both Histogram */
  itkSetMacro(Epsilon, double);

  /** Get epsilon, the histogram frequency to use if the frequency is 0 */
  itkGetConstReferenceMacro(Epsilon, double);

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override
  {
    return this->GetTransform()->GetNumberOfParameters();
  }

  /** Forms the histogram of the training images to prepare to evaluate the */
  /** metric. Must set all parameters first */
  void
  Initialize() override;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  KullbackLeiblerCompareHistogramImageToImageMetric();
  ~KullbackLeiblerCompareHistogramImageToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Form the Histogram for the Training data */
  void
  FormTrainingHistogram();

  /** Evaluates the mutual information from the histogram. */
  MeasureType
  EvaluateMeasure(HistogramType & histogram) const override;

  double m_Epsilon;
};
} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKullbackLeiblerCompareHistogramImageToImageMetric.hxx"
#endif

#endif // itkKullbackLeiblerCompareHistogramImageToImageMetric_h
