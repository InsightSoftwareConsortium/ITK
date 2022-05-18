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
#ifndef itkNormalizedMutualInformationHistogramImageToImageMetric_h
#define itkNormalizedMutualInformationHistogramImageToImageMetric_h

#include "itkHistogramImageToImageMetric.h"

namespace itk
{
/** \class NormalizedMutualInformationHistogramImageToImageMetric
 * \brief Computes normalized mutual information between two images to
 * be registered using the histograms of the intensities in the images.
 *
 * The type of Normalize Mutual Information implemented in this class
 * is given by the equation
 *
 *    \f[  \frac{ H(A) + H(B) }{ H(A,B) }  \f]
 *    Where \$ H(A) \$ is the entropy of image \$ A \$,
 *           \$ H(B) \$ is the entropy of image \$ B \$,  and
 *           \$ H(A,B) \$ is the joing entropy of images \$ A \$ and \$ B \$.
 *
 *    Details of this implementation can be found in the book
 *      "Medical Image Registration" by Hajnal, Hill and Hawkes.
 *      The book is available online at
 *               https://books.google.com/books?id=2dtQNsk-qBQC
 *    The implementation of this class corresponds to equation (30) in
 *    Chapter 3 of this book. Note that by slightly changing this class
 *    it will be trivial to compute the Normalized Mutual Information
 *    measures defined in equations (28) and (29) of the same book.
 *
 *    This class is templated over the type of the fixed and moving
 *    images to be compared.
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT NormalizedMutualInformationHistogramImageToImageMetric
  : public HistogramImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NormalizedMutualInformationHistogramImageToImageMetric);

  /** Standard class type aliases. */
  using Self = NormalizedMutualInformationHistogramImageToImageMetric;
  using Superclass = HistogramImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedMutualInformationHistogramImageToImageMetric, HistogramImageToImageMetric);

  /** Types transferred from the base class */
  using typename Superclass::RealType;
  using typename Superclass::TransformType;
  using typename Superclass::TransformPointer;
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
  using HistogramFrequencyType = typename HistogramType::AbsoluteFrequencyType;
  using HistogramIteratorType = typename HistogramType::Iterator;
  using HistogramMeasurementVectorType = typename HistogramType::MeasurementVectorType;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  NormalizedMutualInformationHistogramImageToImageMetric() = default;
  ~NormalizedMutualInformationHistogramImageToImageMetric() override = default;

  /** Evaluates the normalized mutual information from the histogram. */
  MeasureType
  EvaluateMeasure(HistogramType & histogram) const override;
};
} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizedMutualInformationHistogramImageToImageMetric.hxx"
#endif

#endif // itkNormalizedMutualInformationHistogramImageToImageMetric_h
