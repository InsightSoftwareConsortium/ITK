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
 *               http://books.google.com/books?id=2dtQNsk-qBQC
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
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT NormalizedMutualInformationHistogramImageToImageMetric:
  public HistogramImageToImageMetric< TFixedImage, TMovingImage >
{
public:
  /** Standard class typedefs. */
  typedef NormalizedMutualInformationHistogramImageToImageMetric   Self;
  typedef HistogramImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                                     Pointer;
  typedef SmartPointer< const Self >                               ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedMutualInformationHistogramImageToImageMetric,
               HistogramImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                RealType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;
  typedef typename Superclass::GradientPixelType       GradientPixelType;

  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::FixedImageType          FixedImageType;
  typedef typename Superclass::MovingImageType         MovingImageType;
  typedef typename Superclass::FixedImageConstPointer  FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer MovingImageConstPointer;

  typedef typename Superclass::HistogramType            HistogramType;
  typedef typename HistogramType::AbsoluteFrequencyType HistogramFrequencyType;
  typedef typename HistogramType::Iterator              HistogramIteratorType;
  typedef typename HistogramType::MeasurementVectorType HistogramMeasurementVectorType;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  NormalizedMutualInformationHistogramImageToImageMetric(){}
  virtual ~NormalizedMutualInformationHistogramImageToImageMetric() ITK_OVERRIDE {}

  /** Evaluates the normalized mutual information from the histogram. */
  virtual MeasureType EvaluateMeasure(HistogramType & histogram) const ITK_OVERRIDE;

private:
  // Purposely not implemented.
  NormalizedMutualInformationHistogramImageToImageMetric(Self const &);
  void operator=(Self const &); // Purposely not implemented.
};
} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.hxx"
#endif

#endif // itkNormalizedMutualInformationHistogramImageToImageMetric_h
